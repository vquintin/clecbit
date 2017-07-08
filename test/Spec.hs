import Test.HUnit
import Text.XML.HXT.Core
import Lib (xpSports)

main :: IO ()
main = putStrLn "Test suite not yet implemented"


testParsing = TestCase $
  do [actual] <- runX
                 ( xunpickleDocument xpSports
                      [ withValidate no
                      ] "example.xml"
                 )
     assertEqual "The parsed file is not as expected" expected actual
  where
    expected = undefined
