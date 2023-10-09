module CobaZ3 where

import Z3.Monad

-- a (Z3) structure representing the fomula "x>1". This formula
-- is not valid, but satisfiable.
f1 :: Z3 AST
f1 = do
     x <- mkFreshIntVar "x"
     one <- mkIntNum 1
     f <- x `mkGt` one
     return f

-- a (Z3) structure representing the fomula "x>1  ==>  x>0".
-- This formula is valid (so, also satisfiable).
f2 :: Z3 AST
f2 = do
   x <- mkFreshIntVar "x"
   zero <- mkIntNum 0
   one  <- mkIntNum 1
   f0 <- x `mkGt` one
   f1 <- x `mkGt` zero
   -- form f1 ==> f0
   f <- f1 `mkImplies` f0
   return f

-- representing the fomula "x>y+1 /\  y<0"
-- This formula is satisfiable, but not valid.
f3 :: Z3 AST
f3 = do
    x <- mkFreshIntVar "x"
    y <- mkFreshIntVar "y"
    zero <- mkIntNum 0
    one  <- mkIntNum 1
    t0 <- mkAdd [y,one]
    f0 <- x `mkGt` t0
    f1 <- y `mkLt` zero
    -- form f0 /\ f1
    f <- mkAnd [f0,f1]
    return f

-- representing the fomula "x>0 /\ x<0" (unsatisfiable)
f4 :: Z3 AST
f4 = do
    x <- mkFreshIntVar "x"
    zero <- mkIntNum 0
    f0 <- x `mkGt` zero
    f1 <- x `mkLt` zero
    -- form f0 /\ f1
    f <- mkAnd [f0,f1]
    return f

-- representing the fomula "(exists x:: x<1)"
-- This formula is valid, so also satisfiable. But you can't get
-- a model of it from Z3 (at least I don't know to get it),
-- because x is bound; so it is not exposed.
g1 :: Z3 AST
g1 = do
    x <- mkFreshIntVar "x"
    one <- mkIntNum 1
    body <- x `mkLt` one
    x' <- toApp x
    g <- mkExistsConst [] [x'] body
    return g

-- representing the fomula "y=1 /\ (exists x:: x<1)"
-- This formula is satisfiable. When inspecting the model of the satisfiability
-- from Z3, you can get the value of y in the model, but not the value of
-- x inside the exists-quantifier (at least I don't know to get it).
--
g2 :: Z3 AST
g2 = do
    x <- mkFreshIntVar "x"
    y <- mkFreshIntVar "y"
    one <- mkIntNum 1
    body <- x `mkLt` one
    x' <- toApp x
    g <- mkExistsConst [] [x'] body
    f0 <- mkEq y one
    f1 <- mkAnd [f0,g]
    return f1

-- representing the fomula "x=1 /\ (exists x:: x<1)"
-- This formula is satisfiable. Notice that the two x's are different.
-- The one inside the exists-quantifier is bounded. When inspecting the
-- model of the satisfiability from Z3, you can get the value of x,
-- but note that this refer to the value of the free-x (the one outside
-- the quantifier).
g3 :: Z3 AST
g3 = do
    x <- mkFreshIntVar "x"
    one <- mkIntNum 1
    body <- x `mkLt` one
    x' <- toApp x
    g <- mkExistsConst [] [x'] body
    f0 <- mkEq x one
    f1 <- mkAnd [f0,g]
    return f1

-- representing the fomula "(forall x:: x<1)". This formula is not
-- valid. It is not satisfiable either, in the sense that there is
-- no instance of its free variable that would make the formula
-- evaluate to true (in this case because the formula has NO free
-- variable).
h1 :: Z3 AST
h1 = do
    x <- mkFreshIntVar "x"
    one <- mkIntNum 1
    body <- x `mkLt` one
    x' <- toApp x
    h <- mkForallConst [] [x'] body
    return h

--
-- A function to use Z3 to check the satisfiability of a formula.
-- If it is satisfiable, it will also print a 'model' of the formula.
-- A model is a mapping of the free variables in the formula to values,
-- that would make the formula true.
--
-- Example use: testSat f1
--
testSAT :: Z3 AST-> IO()
testSAT f =
  let
  -- Construct a Z3 checker. It will "assert" the formula f1, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 (Result, String)
  checker = do
        f_ <- f
        assert f_                    -- asserting the formula to check
        (verdict,model) <- getModel  -- checking if the formula is satisfiable
                                     -- if the formula is satisfiable, you also get
                                     -- a 'model',
        case model of
          Nothing -> return (verdict,"Sorry... can't get a model.")
          Just m  -> do
                     s <- modelToString m  -- I'll just pretty-print the model to a string
                     return (verdict,s)
  in
  -- Next, we run the checker. The we check if the returned verdict is Sat,
  -- and if so we print some Yay-message.
  do -- inside the IO monad now:
    (verdict,model) <- evalZ3 checker
    if verdict == Sat
       then do
            putStrLn "   The formula is satisfiable."
            putStrLn "   Model:"
            putStrLn ("   " ++ model)
       else putStrLn "   The formula s UNsatisfiable."





-- A function that uses Z3 to check if a formula f is valid. It does this
-- by checking if "not f" is satisfiable. It it is, then f is NOT valid.
--
-- Example use: testVALID f1
testVALID :: Z3 AST -> IO()
testVALID f =
  let
  -- Construct a Z3 checker. It will "assert" the formula not-f2, then
  -- check if it is satisfiable. The checker returns (inside Z3-monad)
  -- either Sat or Unsat.
  checker :: Z3 Result
  checker = do
    f <- f
    nf <- mkNot f
    assert nf                         -- asserting the formula to check
    (verdict,model) <- getModel  -- checking if the formula is satisfiable
    return verdict               -- returning the verdict Sat or Unsat
  in
  -- Next, we run the checker. The we check if the returned verdict is Unsat,
  -- and if so we print some Yay-message.
  do verdict <- evalZ3 checker
     if verdict == Unsat
        then putStrLn "   The formula is valid."
        else putStrLn "   The formula is invalid."


-- run this test-function for a demo:
test :: IO()
test = do
   putStrLn "** f1"
   testVALID f1
   testSAT f1
   putStrLn "** f2"
   testVALID f2
   testSAT f2
   putStrLn "** f3"
   testVALID f3
   testSAT f3
   putStrLn "** f4"
   testVALID f4
   testSAT f4
   putStrLn "** g1"
   testVALID g1
   testSAT g1
   putStrLn "** g2"
   testVALID g2
   testSAT g2
   putStrLn "** g3"
   testVALID g3
   testSAT g3
   putStrLn "** h1"
   testVALID h1
   testSAT h1
