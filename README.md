# parser

- wrote at munihac

ghci
``` haskell
parseString "-(1 + 3)" parserExpr 
-- Just (NegExpr (BinOpExpr (ConstExpr 1) Plus (ConstExpr 3)))
```
