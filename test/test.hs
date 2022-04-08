import Test.Tasty
import Test.Tasty.Ingredients.Rerun

import THTests (thTests)
import SpecTests (specTests)
import Tests (tests)

main :: IO ()
main = defaultMainWithRerun $
  testGroup "tests" $ tests ++ [specTests, thTests]
