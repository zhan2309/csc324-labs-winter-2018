{-
How to use:

    runghc testLab08.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab08 ()
import Lab08Def

tree1 = Branch (Tip 2) (Branch (Tip 7) (Tip 8))
k1 i | r == 0 = Tip q
     | otherwise = Branch (Tip q) (Tip (q+ 1))
  where
    (q, r) = divMod i 2
answer1 = Branch (Tip 1) (Branch (Branch (Tip 3) (Tip 4)) (Tip 4))

tests = [ "handout" ~: (tree1 >>= k1) ~?= answer1
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
