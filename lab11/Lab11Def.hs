module Lab11Def where

data SExpr = Ident String | List [SExpr] deriving (Eq, Show)
