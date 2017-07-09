import qualified Data.Map as Map
import Data.Time
import Test.HUnit
import Text.XML.HXT.Core
import Lib

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
    expected =
      XMLSports
        (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T21:55:17.403")
        Map.empty
