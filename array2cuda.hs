-- Very simple Poly language interpreter written in Haskell
-- Dependency: ghc, stack
-- Usage:
--   Write input in line 39
--   $ make haskell

{-
    expr  = Const( float  val  )
        | Mul  ( expr lhs, expr rhs )
        | Array [[expr]]
-}


data Expr
  = Const Float
  | Mul Expr Expr
  | Array [[Expr]]
  deriving (Show)

eval :: Expr ->  String
eval (Const x) = show x
eval (Mul x y) = "float input_A[] = {" ++ (eval x) ++ "}; " ++ "float input_B[] = {" ++ (eval y) ++ "};";
eval (Array x) = showlist x 16;

fillzero :: Int -> String
fillzero 1 = "";
fillzero itr = "0.0, " ++ fillzero (itr - 1);

filllist :: Int -> String
filllist 1 = fillzero 16;
filllist itr = fillzero 16 ++ filllist (itr - 1);

showlist :: [[Expr]] -> Int -> String
showlist (x:[]) itr = showelem x 16 ++ filllist itr;
showlist (x:y) itr = showelem x 16 ++ showlist y (itr - 1);

showelem :: [Expr] -> Int -> String
showelem (x:[]) itr = eval x ++ ", " ++ fillzero itr;
showelem (x:y) itr = eval x ++ ", " ++ showelem y (itr - 1);

main :: IO ()
main = do
-- Write input here
  let input = Mul (Array [[Const 1.0, Const 2.0], [Const 3.0, Const 4.0]]) (Array [[Const 5.0, Const 6.0], [Const 3.0, Const 2.0]])
  let output = eval input
  print output
