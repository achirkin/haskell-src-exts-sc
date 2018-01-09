{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Exts.SimpleComments
  ( testIt, toExactHaddocked
  ) where

import           Control.Monad                    (forM, forM_, join)
import           Control.Monad.ST.Strict
import           Data.Foldable                    (foldl')
import           Data.List                        (sortOn)
import           Data.Maybe
import           Data.STRef

-- import           Language.Haskell.Exts.Build
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

import           Generics.ApplyTwins


testIt :: IO ()
testIt = do

  let m = Module Nothing
         (Just $ ModuleHead Nothing
                            (ModuleName Nothing "GoodModule") Nothing Nothing
         ) [] []
          [ TypeSig Nothing [Ident Nothing "func1"] (TyFun Nothing (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))) (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))))
          , PatBind Nothing (PVar Nothing (Ident Nothing "func1")) (UnGuardedRhs Nothing (LeftSection Nothing (Lit Nothing (String Nothing "asgsd" "asgsd")) (QVarOp Nothing (UnQual Nothing (Symbol Nothing "++"))))) Nothing
          , DataDecl (Just "This is a data declaration") (DataType Nothing) Nothing (DHead Nothing (Ident Nothing "Hello")) [QualConDecl Nothing Nothing Nothing (ConDecl Nothing (Ident Nothing "HelloA") [])
          , QualConDecl Nothing Nothing Nothing (ConDecl (Just "HelloB ConDecl") (Ident (Just "HelloB Ident") "HelloB") [])] (Just (Deriving Nothing [IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Eq")))
          , IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Show")))]))
          , TypeSig (Just "My first comment\naslo multiline!") [Ident Nothing "func2"] (TyFun Nothing (TyCon Nothing (UnQual Nothing (Ident Nothing "Int"))) (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))))
          , PatBind Nothing (PVar Nothing (Ident Nothing "func2")) (UnGuardedRhs Nothing (Var Nothing (UnQual Nothing (Ident Nothing "show")))) Nothing
          ]


  print $ apTwins ((,) <$> m) (fst $ toExactHaddocked m)

  putStrLn "-----------------"
  putStrLn $ prettyPrint m
  putStrLn "-----------------"
  putStrLn $ uncurry exactPrint . toExactHaddocked $ m
  putStrLn "-----------------"



toExactHaddocked :: Module (Maybe String)
                 -> (Module SrcSpanInfo, [Comment])
toExactHaddocked m'' = runST $ do
    -- make location info mutable
    mSt <- mapM (\(mt, sloc) -> (,) (sloc, mt) <$> newSTRef sloc) m
    let (allLocRefs, allComments') = foldl' f ([],[]) mSt
          where
            f (ls, cs) ((_, Nothing), l) = (l:ls, cs)
            f (ls, cs) ((x, Just c), l)  = (l:ls, (x,(c,l)):cs)
        -- sort comments by their location, so that insertion of earlier comments
        -- does not affect the position of later comments.
        allComments = map snd $ sortOn fst allComments'
    -- update all locations for each comment
    ccs <- forM allComments $ \(comment, locref) -> do
      loc <- readSTRef locref
      let cSpan = srcInfoSpan loc
          (updateLoc, cs) = insertPostComments comment cSpan
      forM_ allLocRefs $ flip modifySTRef updateLoc
      return cs
    mFin <- mapM (readSTRef . snd) mSt
    return (mFin, join ccs)
  where
    m' :: Module SrcSpanInfo
    m' = case parseModule $ prettyPrint m'' of
          err@ParseFailed {} -> error $ show err
          ParseOk r          -> r
    m :: Module (Maybe String, SrcSpanInfo)
    m = fromMaybe
      ( error "structure of the original and generate-parsed modules differ." )
      ( apTwins ((,) <$> m'') m' )




-- | Insert comments above codepoints
insertPreComments :: String
                  -> SrcSpan -- ^ location of an element
                             --   for comments to be attached
                  -> (SrcSpanInfo -> SrcSpanInfo, [Comment])
insertPreComments txt locs = (f, cmts)
  where
    cmtLoc = SrcLoc (srcSpanFilename locs)
                    startL
                    startC
    cmts = mkComments cmtLoc '|' txt
    startL = srcSpanStartLine locs
    startC = srcSpanStartColumn locs
    lineN = length cmts
    f SrcSpanInfo {srcInfoSpan = s, srcInfoPoints = ps}
      = SrcSpanInfo
      { srcInfoSpan = g s, srcInfoPoints = fmap g ps }
    g s | srcSpanEndLine s < startL ||
          srcSpanStartLine s == startL && srcSpanEndColumn s < startC
          = s
        | srcSpanStartLine s > startL ||
          srcSpanStartLine s == startL && srcSpanEndColumn s >= startC
          = s { srcSpanStartLine = srcSpanStartLine s + lineN
              , srcSpanEndLine = srcSpanEndLine s + lineN
              }
        | otherwise
          = s { srcSpanEndLine = srcSpanEndLine s + lineN }


-- | Insert comments right and below codepoints
--    (further along the flow of the program)
insertPostComments :: String
                   -> SrcSpan -- ^ location of an element
                              --   for comments to be attached
                   -> (SrcSpanInfo -> SrcSpanInfo, [Comment])
insertPostComments txt locs = (f, cmts)
  where
    cmtLoc = SrcLoc (srcSpanFilename locs)
                    startL
                    startC
    cmts = mkComments cmtLoc '^' txt
    startL = srcSpanEndLine locs
    startC = srcSpanEndColumn locs + 1
    lineN = length cmts
    f SrcSpanInfo {srcInfoSpan = s, srcInfoPoints = ps}
      = SrcSpanInfo
      { srcInfoSpan = g s, srcInfoPoints = fmap g ps }
    g s | srcSpanEndLine s < startL ||
          srcSpanStartLine s == startL && srcSpanEndColumn s < startC
          = s
        | srcSpanStartLine s > startL ||
          srcSpanStartLine s == startL && srcSpanEndColumn s >= startC
          = s { srcSpanStartLine = srcSpanStartLine s + lineN
              , srcSpanEndLine = srcSpanEndLine s + lineN
              }
        | otherwise
          = s { srcSpanEndLine = srcSpanEndLine s + lineN }



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
                    then x:xs
                    else (' ':c:' ':x) : map ("   " ++) xs
    mkComment _ [] = []
    mkComment i (x:xs)
      = Comment False
        (SrcSpan srcFilename i srcColumn i $ srcColumn + 2 + length x) x
        : mkComment (i+1) xs
