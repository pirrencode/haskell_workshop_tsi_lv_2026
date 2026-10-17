-- Lecture 5 Demo: Pure functions, composition, closures, environments, collections
-- Run with:  ghc -o demo Main.hs && ./demo
--   or:      runghc Main.hs
--   or in GHCi:  ghci Main.hs  then  main

module Main where

import Test.HUnit

-- ============================================================
-- 1. Pure functions & composition (slides 4, 7)
--    No side effects: same input, same output, every time.
-- ============================================================

square :: Int -> Int
square x = x * x

inc :: Int -> Int
inc x = x + 1

-- Composition: the output of one function becomes the input of the next.
squareThenInc :: Int -> Int
squareThenInc = inc . square

sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = square x + square y

-- ============================================================
-- 2. Local state without mutation (slides 12-17)
--    Scheme uses set! (imperative). In pure Haskell we return
--    a NEW account value instead of mutating a shared balance.
-- ============================================================

-- A newtype (not a recursive 'type' synonym, which GHC rejects) lets
-- the result carry the next Account, so state is threaded functionally.
newtype Account = Account { runWithdraw :: Int -> (Either String Int, Account) }

makeAccount :: Int -> Account
makeAccount balance = Account $ \amount ->
  if amount <= balance
    then ( Right (balance - amount)
         , makeAccount (balance - amount) )
    else ( Left "Insufficient funds"
         , makeAccount balance )

-- Run a sequence of withdrawals, threading the account forward.
withdrawAll :: Account -> [Int] -> [Either String Int]
withdrawAll _ [] = []
withdrawAll acc (a:rest) =
  let (result, acc') = runWithdraw acc a
  in result : withdrawAll acc' rest

-- ============================================================
-- 3. Imperative vs functional style (slides 19-23)
--    No assignment means no "order of set!" bugs.
-- ============================================================

-- Recursive (declarative) factorial
factorial :: Int -> Int
factorial n
  | n <= 1    = 1
  | otherwise = n * factorial (n - 1)

-- Tail-recursive "iterative" factorial (Task 1, slide 48)
factIter :: Int -> Int
factIter n = go 1 1
  where
    go :: Int -> Int -> Int
    go acc counter
      | counter > n = acc
      | otherwise   = go (counter * acc) (counter + 1)

-- ============================================================
-- 4. Collections (slides 37-46)
--    A collection groups many scalars into one aggregate unit.
-- ============================================================

-- Fold an aggregate back down to a scalar.
total :: [Int] -> Int
total = sum

-- Declarative processing, like the Java streams example (slide 21):
-- filter lines starting with "ERROR", keep at most 40.
errorLines :: [String] -> [String]
errorLines = take 40 . filter (isPrefix "ERROR")

isPrefix :: String -> String -> Bool
isPrefix [] _      = True
isPrefix _  []     = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Recursive list processing: length of a collection.
count :: [a] -> Int
count []     = 0
count (_:xs) = 1 + count xs

-- ============================================================
-- 5. Shadowing & environments (slides 24-34)
--    Inner bindings shadow outer ones, like frames in the
--    environment model. No mutation of the "global" x.
-- ============================================================

globalX :: Int
globalX = 3

-- The parameter x shadows globalX inside this frame only.
shadowDemo :: Int -> Int
shadowDemo x = x + 1

-- ============================================================
-- Unit tests (HUnit)
-- ============================================================

tests :: Test
tests = TestList
  [ "squareThenInc 3"    ~: squareThenInc 3               ~?= 10
  , "sumOfSquares 3 4"   ~: sumOfSquares 3 4             ~?= 25
  , "factorial 6"        ~: factorial 6                  ~?= 720
  , "factIter 6"         ~: factIter 6                   ~?= 720
  , "factIter 0"         ~: factIter 0                   ~?= 1
  , "withdraw ok"         ~: fst (runWithdraw (makeAccount 100) 25)
                                                ~?= Right 75
  , "withdraw too much"   ~: fst (runWithdraw (makeAccount 50) 60)
                                                ~?= Left "Insufficient funds"
  , "isPrefix"           ~: isPrefix "ER" "ERROR"        ~?= True
  , "errorLines filters" ~: errorLines ["ERROR a", "INFO b", "ERROR c"]
                                              ~?= ["ERROR a", "ERROR c"]
  , "errorLines limit 40" ~: length (errorLines (replicate 100 "ERROR x"))
                                              ~?= 40
  , "count"              ~: count [1 .. 10 :: Int]        ~?= 10
  , "total"              ~: total [1 .. 100 :: Int]       ~?= 5050
  , "shadowDemo 7"        ~: shadowDemo 7                 ~?= 8
  ]

runTests :: IO ()
runTests = do
  counts <- runTestTT tests
  if errors counts + failures counts > 0
    then ioError (userError "Unit tests FAILED")
    else putStrLn "All unit tests passed."

-- ============================================================
-- Demo
-- ============================================================

showResult :: Either String Int -> String
showResult (Right v) = show v
showResult (Left e)  = "Error: " ++ e

main :: IO ()
main = do
  runTests

  putStrLn "\n== 1. Pure functions & composition =="
  putStrLn $ "squareThenInc 3 = " ++ show (squareThenInc 3)
  putStrLn $ "sumOfSquares 3 4 = " ++ show (sumOfSquares 3 4)

  putStrLn "\n== 2. Account with local state (functional) =="
  let acc0 = makeAccount 100
      (r1, acc1) = runWithdraw acc0 25
      (r2, acc2) = runWithdraw acc1 25
      (r3, acc3) = runWithdraw acc2 60
      (r4, _)    = runWithdraw acc3 15
  putStrLn $ "withdraw 25 -> " ++ showResult r1
  putStrLn $ "withdraw 25 -> " ++ showResult r2
  putStrLn $ "withdraw 60 -> " ++ showResult r3
  putStrLn $ "withdraw 15 -> " ++ showResult r4

  putStrLn "\n== 3. Factorial: recursive vs iterative =="
  putStrLn $ "factorial  6 = " ++ show (factorial 6)
  putStrLn $ "factIter   6 = " ++ show (factIter 6)

  putStrLn "\n== 4. Collections =="
  let logLines = [ "INFO ok", "ERROR disk full", "ERROR timeout"
                 , "INFO ok", "ERROR disk full", "ERROR again" ]
  putStrLn $ "errorLines: " ++ show (errorLines logLines)
  putStrLn $ "count: " ++ show (count logLines)
  putStrLn $ "total [1..100] = " ++ show (total [1 .. 100 :: Int])

  putStrLn "\n== 5. Shadowing & environments =="
  putStrLn $ "globalX = " ++ show globalX
  putStrLn $ "shadowDemo 7 = " ++ show (shadowDemo 7)
  putStrLn $ "globalX still = " ++ show globalX