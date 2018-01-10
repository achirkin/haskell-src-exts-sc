{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Language.Haskell.Exts
import           Language.Haskell.Exts.SimpleComments

main :: IO ()
main = do
  putStrLn "-------------------------------------------------------------------"
  putStrLn $ prettyPrint testModule
  putStrLn "-------------------------------------------------------------------"
  putStrLn $ uncurry exactPrint . ppWithComments $ testModule
  putStrLn "-------------------------------------------------------------------"


testModule :: Module (Maybe CodeComment)
testModule = Module Nothing
  (Just $ ModuleHead (preComment "Module description could be here")
                    (ModuleName Nothing "GoodModule") Nothing Nothing
  ) [] []
  [ -- func1 :: String -> String
    TypeSig
      Nothing
      [Ident Nothing "func1"]
      (TyFun Nothing
        (TyCon
          (postComment "Argument of a function\nthe code on the right should shift down")
          (UnQual Nothing (Ident Nothing "String"))
        )
        (TyCon
          (Just "Result type of a function.\n  BTW, this comment\n  is three lines long!")
          (UnQual Nothing (Ident Nothing "String"))
        )
      )
  , PatBind Nothing (PVar Nothing (Ident Nothing "func1"))
      (UnGuardedRhs Nothing
        (LeftSection
          (Just "just a comment")
          (Lit Nothing (String Nothing "asgsd" "asgsd"))
          (QVarOp Nothing (UnQual Nothing (Symbol Nothing "++")))
        )
      ) Nothing
    -- data Hello = HelloA | HelloB deriving (Eq,Show)
  , DataDecl
      (preComment "This is a data declaration")
      (DataType Nothing) Nothing (DHead Nothing (Ident Nothing "Hello"))
      [ QualConDecl Nothing Nothing Nothing
          (ConDecl Nothing (Ident (postComment "HelloA Ident") "HelloA") [])
      , QualConDecl Nothing Nothing Nothing
          (ConDecl (postComment "HelloB ConDecl") (Ident Nothing "HelloB") [])
      ]
      (Just (Deriving Nothing
        [ IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Eq")))
        , IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Show")))
        ])
      )
    -- func2 :: Int -> String
  , TypeSig
      (preComment "My first comment\naslo multiline!")
      [Ident Nothing "func2"]
      (TyFun Nothing
        (TyCon (Just "Not a haddock comment")
          (UnQual Nothing (Ident Nothing "Int"))
        )
        (TyCon Nothing
          (UnQual Nothing (Ident Nothing "String"))
        )
      )
  , PatBind Nothing (PVar Nothing (Ident Nothing "func2"))
      (UnGuardedRhs Nothing
        (Var Nothing (UnQual Nothing (Ident Nothing "show")))
      ) Nothing
  ]
