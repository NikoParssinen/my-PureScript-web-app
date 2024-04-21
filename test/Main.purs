module Test.Main where

import Prelude
import Main (calculate)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException, message)
import Test.QuickCheck (
      quickCheck'
    , (<?>)
    , class Testable
    )

-- wraps the test output with the name of the test
runTest :: String -> Effect Unit -> Effect Unit
runTest testName test = do
  log $ "=== Running test:  " <> testName
  test
  log $ "=== Test finished: " <> testName <> "\n\n"

-- runs our tests
main :: Effect Unit
main = do
  runTest "Addition" addition
  runTest "Substraction" substraction
  runTest "Multiplication" multiplication
  runTest "Multiplication with negative first value" multiplication_with_negative_first_value
  runTest "Multiplication with negative second value" multiplication_with_negative_second_value
  runTest "Division" division
  runTest "Divided by zero" divided_by_zero
  runTest "Not an operation" non_operations
  runTest "Pointless zeros" pointless_zeros

addition :: Effect Unit
addition = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i) <> "+" <> (show i)) == show (i + i)
                    <?> ((calculate ((show i) <> "+" <> (show i))) <> " did not equal " <> show (i + i)))

substraction :: Effect Unit
substraction = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i) <> "-" <> (show i)) == show (i - i)
                    <?> ((calculate ((show i) <> "-" <> (show i))) <> " did not equal " <> show (i - i)))

multiplication :: Effect Unit
multiplication = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i) <> "*" <> (show 2)) == show (i * 2)
                    <?> ((calculate ((show i) <> "*" <> (show 2))) <> " did not equal " <> show (i * 2)))

multiplication_with_negative_first_value :: Effect Unit
multiplication_with_negative_first_value = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show (-2)) <> "*" <> (show i)) == show ((-2) * i)
                    <?> ((calculate ((show (-2)) <> "*" <> (show i))) <> " did not equal " <> show ((-2) * i)))

multiplication_with_negative_second_value :: Effect Unit
multiplication_with_negative_second_value = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i) <> "*" <> (show (-2))) == show (i * (-2))
                    <?> ((calculate ((show i) <> "*" <> (show (-2)))) <> " did not equal " <> show (i * (-2))))

division :: Effect Unit
division = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i) <> "/" <> (show i)) == show (i / i)
                    <?> ((calculate ((show i) <> "/" <> (show i))) <> " did not equal " <> show (i / i)))

divided_by_zero :: Effect Unit
divided_by_zero = printErrorMessage $ quickCheck_int 1 (\i -> calculate ((show i) <> "/0") == show (i / 0)
                    <?> calculate ((show i) <> "/0") <> " did not equal " <> (show (i / 0)))

non_operations :: Effect Unit
non_operations = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ((show i)) == show i
                    <?> ((calculate (show i)) <> " did not equal " <> show i))

pointless_zeros :: Effect Unit
pointless_zeros = printErrorMessage $ quickCheck_int 1000 (\i -> calculate ("00001234+" <> (show i)) == show (1234 + i)
                    <?> calculate ("00001234+" <> (show i)) <> " did not equal " <> show (1234 + i))

-- Helper functions

printErrorMessage :: Effect Unit -> Effect Unit
printErrorMessage test = catchException (\error -> log $ message error) test

quickCheck_int :: forall a. Testable a => Int -> (Int -> a) -> Effect Unit
quickCheck_int numOfTests test = quickCheck' numOfTests test
