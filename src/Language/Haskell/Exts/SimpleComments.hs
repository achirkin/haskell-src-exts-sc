{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Exts.SimpleComments
  ( -- * Data types
    CodeComment (..), CommentPos (..)
    -- * Generate source code
  , ppWithCommentsStyleModeParseMode
  , ppWithCommentsStyleMode
  , ppWithCommentsMode
  , ppWithComments
    -- * Convenience functions
  , preComment, postComment, secComment
  ) where

import           Control.Monad                   (forM, forM_, join)
import           Control.Monad.ST.Strict
import           Data.Foldable                   (any, foldl')
import           Data.List                       (sortOn)
import           Data.STRef
import           Data.String                     (IsString (..))
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

import           Generics.ApplyTwins


-- | Annotate AST with comments.
data CodeComment
  = CodeComment
  { ccPos :: !CommentPos
    -- ^ How to place comment
  , ccSym :: !Char
    -- ^ Special character to prepend to the first line keeping the alignment;
    --   e.g. '*' for haddock sections, '|' for top-level haddock declarations.
    --   If ' '(space) is used, no additional indentation is added.
  , ccTxt :: !String
    -- ^ Content of a comment (can be multiline)
  } deriving (Eq, Show, Read)

instance IsString CodeComment where
  fromString = CodeComment NextToCode ' '

-- | Where to place a comment, relative to an AST node
data CommentPos = AboveCode | BelowCode | NextToCode
  deriving (Eq, Show, Read)


-- | Add haddock-style comment above some definition
preComment :: String -> Maybe CodeComment
preComment "" = Nothing
preComment s  = Just $ CodeComment AboveCode '|' s

-- | Add haddock-style comment below some definition
postComment :: String -> Maybe CodeComment
postComment "" = Nothing
postComment s  = Just $ CodeComment BelowCode '^' s

-- | Add haddock-style section ('*'),
--    use it on definitions in an export list.
secComment :: String -> Maybe CodeComment
secComment "" = Nothing
secComment s  = Just $ CodeComment AboveCode '*' s


-- | `ppWithCommentsMode` with default mode
ppWithComments :: Module (Maybe CodeComment)
               -> (Module SrcSpanInfo, [Comment])
ppWithComments = ppWithCommentsMode defaultMode

-- | `ppWithCommentsStyleMode` with default style
ppWithCommentsMode :: PPHsMode
                   -> Module (Maybe CodeComment)
                   -> (Module SrcSpanInfo, [Comment])
ppWithCommentsMode = ppWithCommentsStyleMode style


-- | `ppWithCommentsStyleModeParseMode` with default parse style
ppWithCommentsStyleMode :: Style
                        -> PPHsMode
                        -> Module (Maybe CodeComment)
                        -> (Module SrcSpanInfo, [Comment])
ppWithCommentsStyleMode sty ppm
  = ppWithCommentsStyleModeParseMode sty ppm defaultParseMode

-- | Run pretty print and parse to obtain `SrcSpanInfo`,
--   then gradually insert comments.
ppWithCommentsStyleModeParseMode :: Style
                                 -> PPHsMode
                                 -> ParseMode
                                 -> Module (Maybe CodeComment)
                                 -> (Module SrcSpanInfo, [Comment])
ppWithCommentsStyleModeParseMode sty ppm parsem' m'' = runST $ do
    -- make location info mutable
    mSt <- mapM (\(mt, sloc) -> (,) (sloc, mt) <$> newSTRef sloc) m
    let (allLocRefs, allComments') = foldl' f ([],[]) mSt
          where
            f (ls, cs) ((_, Nothing), l)
              = (l:ls, cs)
            f (ls, cs) ((x, Just c), l)
              = let shiftRight = isShiftRight x m'
                    shiftLoc = snd $ evalLoc (ccPos c) (srcInfoSpan x) shiftRight
                in (l:ls, (shiftLoc, (c, l, shiftRight)):cs)
        -- sort comments by their location, so that insertion of earlier comments
        -- does not affect the position of later comments.
        allComments = map snd $ sortOn fst $ reverse allComments'
    -- update all locations for each comment
    ccs <- forM allComments $ \(comment, locref, shiftRight) -> do
      loc <- readSTRef locref
      let (comLoc, shiftLoc) = evalLoc (ccPos comment) (srcInfoSpan loc) shiftRight
          (updateLoc, cs) = insertComments comLoc shiftLoc comment
      forM_ allLocRefs $ flip modifySTRef updateLoc
      return cs
    mFin <- mapM (readSTRef . snd) mSt
    return (mFin, join ccs)
  where
    m' :: Module SrcSpanInfo
    m' = case parseModuleWithMode parsem $ prettyPrintStyleMode sty ppm m'' of
          err@ParseFailed {} -> error $ show err
          ParseOk r          -> r
    m :: Module (Maybe CodeComment, SrcSpanInfo)
    m = apTwinsDef ((,) Nothing) ((,) <$> m'') m'
    -- check if there is anything to the right from the element
    isShiftRight x = any (isToRight . srcInfoSpan)
      where
        s = srcInfoSpan x
        isToRight z = srcSpanEndLine s == srcSpanStartLine z
                   && srcSpanEndColumn s <= srcSpanStartColumn z
    parsem = parsem' { extensions = extensions parsem' ++ getPragmas m''}
    getPragmas (Module _ _ xs _ _)
      = classifyExtension . getNContent <$> getLPragmas xs
    getPragmas (XmlPage _ _ xs _ _ _ _)
      = classifyExtension . getNContent <$> getLPragmas xs
    getPragmas (XmlHybrid _ _ xs _ _ _ _ _ _)
      = classifyExtension . getNContent <$> getLPragmas xs
    getLPragmas []                         = []
    getLPragmas (LanguagePragma _ xs : ps) = xs ++ getLPragmas ps
    getLPragmas (_:ps)                     = getLPragmas ps
    getNContent (Ident _ s)  = s
    getNContent (Symbol _ s) = s


insertComments :: SrcLoc
                  -- ^ Start position of a comment
               -> SrcLoc
                  -- ^ A point where to start code displacement
               -> CodeComment
                  -- ^ comment itself
               -> (SrcSpanInfo -> SrcSpanInfo, [Comment])
                  -- ^ a function that modifies the code locations
                  --   and a list of `haskell-src-exts` comments.
insertComments cmtLoc@(SrcLoc _ startL _)
                      (SrcLoc _ shiftL shiftC) com = (f, cmts)
  where
    cmts = mkComments cmtLoc (ccSym com) (ccTxt com)
    lineN = length cmts + startL - shiftL
    f SrcSpanInfo {srcInfoSpan = s, srcInfoPoints = ps}
      = SrcSpanInfo
      { srcInfoSpan = g s, srcInfoPoints = fmap g ps }
    g s | srcSpanEndLine s < shiftL ||
          srcSpanStartLine s == shiftL && srcSpanEndColumn s < shiftC
          = s
        | srcSpanStartLine s > shiftL ||
          -- This is a problematic condition: from which column we move code?
          -- Consider two cases commenting b:
          --   (srcSpanStartColumn>=shiftC)  (srcSpanStartColumn>=shiftC-1)
          --    a+b
          -- => a+-- comment is broken       a -- comment is good
          --                                  +b
          --
          --    a + (b - c)
          -- => a + (-- comment is good      a -- comment is misplaced
          --         b - c)                   (b - c)
          --
          -- ATM, I have chosen the left column, because:
          --   * Autogen code seem to put spaces between operators
          --   * Autogen code seem to not put spaces inside parentheses
          --   * Another variant breaks haddock in export lists
          srcSpanStartLine s == shiftL && srcSpanStartColumn s >= shiftC
          = s { srcSpanStartLine = srcSpanStartLine s + lineN
              , srcSpanEndLine = srcSpanEndLine s + lineN
              }
        | otherwise
          = s { srcSpanEndLine = srcSpanEndLine s + lineN }

-- | Determine the position of a comment
evalLoc :: CommentPos -- ^ relative position of a comment
        -> SrcSpan    -- ^ position of a node
        -> Bool -- ^ if there is anything that goes after the node in its line
        -> (SrcLoc, SrcLoc)
evalLoc AboveCode SrcSpan {..} _ = ( loc, loc )
  where
    loc = SrcLoc srcSpanFilename srcSpanStartLine srcSpanStartColumn
evalLoc BelowCode SrcSpan {..} shiftRight = (locStart, locShift )
  where
    locStart = SrcLoc srcSpanFilename (srcSpanEndLine + 1) $
             if srcSpanStartLine == srcSpanEndLine
             then srcSpanStartColumn
             else min srcSpanStartColumn srcSpanEndColumn
    locShift = if shiftRight
               then SrcLoc srcSpanFilename srcSpanEndLine (srcSpanEndColumn + 1)
               else locStart { srcColumn = 1 }
evalLoc NextToCode SrcSpan {..} shiftRight = (locStart, locShift)
  where
    locStart = SrcLoc srcSpanFilename srcSpanEndLine (srcSpanEndColumn + 1)
    locShift = if shiftRight
               then locStart
               else SrcLoc srcSpanFilename (srcSpanEndLine + 1) 1

-- | Make a textual comment into a documentation.
mkComments :: SrcLoc -- ^ location of the comment start
           -> Char -- ^ special comment character (i.e. "*" or "^" or "|")
           -> String -- ^ text to put into a comment (multiline)
           -> [Comment]
mkComments SrcLoc {..} c txt = mkComment srcLine lns
  where
    lns = indent $ lines txt
    indent []     = []
    indent (x:xs) = if c == ' '
                    then map (' ':) (x:xs)
                    else (' ':c:' ':x) : map ("   " ++) xs
    mkComment _ [] = []
    mkComment i (x:xs)
      = Comment False
        (SrcSpan srcFilename i srcColumn i $ srcColumn + 2 + length x) x
        : mkComment (i+1) xs
