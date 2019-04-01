{-
How to use:

One-time installation of the HUnit library if you don't already have it:

    cabal install HUnit

(How do you tell? If there is an error message saying "Could not find module
Test.HUnit".)

Then this attempts all test cases:

    runghc testLab04.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import qualified Lab04 as Candidate (myFilter, myFoldl)

-- Re-assert desired types.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = Candidate.myFilter

myFoldl :: (b -> a -> b) -> [a] -> (b -> b)
myFoldl = Candidate.myFoldl

tests = testFilter ++ testFoldl

testFilter =
    [ TestCase (assertBool
                "myFilter (\\_ -> True) []"
                (null (myFilter (\_ -> True) [])))
    , "myFilter handout" ~: myFilter (\x -> x > 0) [1, -2, 3] ~?= [1, 3]
    -- more tests when marking
    ]

testFoldl =
    [ "myFoldl handout" ~: myFoldl (-) [1,2,3] 10 ~?= 4
    -- more tests when marking
    ]

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
