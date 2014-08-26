data Combinator = Var String
                | App Combinator Combinator
                | Lambda String Combinator
                deriving Eq

instance Show Combinator where
  show (Var x)      = x
  show (App x y)    =
    case y of
      App _ _ -> showLambda x ++ "(" ++ show y ++ ")"
      _       -> showLambda x ++ showLambda y
    where
      showLambda l@(Lambda _ _) = "(" ++ show l ++ ")"
      showLambda x              = show x
  show (Lambda x e) = "\\" ++ x ++ "." ++ show e

-- SKI basis
--
s = Var "S"
k = Var "K"
i = Var "I"

free :: String -> Combinator -> Bool
free x (Var y)      = x == y
free x (App e1 e2)  = free x e1 || free x e2
free x (Lambda y e) | x /= y = free x e
                    | x == y = False

transform :: Combinator -> Combinator
transform (Var x)                   = Var x
transform (App x y)                 = App (transform x) (transform y)
transform (Lambda x (Var y))        | x == y = i
                                    | otherwise = App k (Var y)
transform (Lambda x l@(Lambda _ _)) = transform (Lambda x (transform l))
transform (Lambda x (App e1 e2))    = App (App s (transform (Lambda x e1)))
                                                 (transform (Lambda x e2))

-- other useful combinators
--
b = Lambda "x" (Lambda "y" (Lambda "z" (App (Var "x") (App (Var "y") (Var "z")))))
c = Lambda "x" (Lambda "y" (Lambda "z" (App (Var "x") (App (Var "z") (Var "y"))) ))
w = Lambda "x" (Lambda "y" (App (Var "x") (App (Var "y") (Var "y"))))
m = Lambda "x" (App (Var "x") (Var "x"))

-- Fixed-point combinators
--
-- Y = \f.(\x.f(xx)) (\x.f(xx))
y' = Lambda "x" (App (Var "f") (App (Var "x") (Var "x")))
y = App (Lambda "f" y') y'

-- X = \f.(\x.xx) (\x.f(xx))
x' = Lambda "x" (App (Var "x") (Var "x"))
x = App (Lambda "f" x') y'
