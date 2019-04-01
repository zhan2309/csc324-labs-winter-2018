{-
How to use:

One-time installation of the HUnit library if you don't already have it:

    cabal install HUnit

(How do you tell? If there is an error message saying "Could not find module
Test.HUnit".)

Then this attempts all test cases:

    runghc testLab03merge.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab03merge (merge)

tests = [ "handout" ~: merge [2, 3, 5] [1, 3, 4] ~?= [1, 2, 3, 3, 4, 5]
          -- More test cases when marking.
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
