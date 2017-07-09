import qualified Data.Map as Map
import Data.Time
import Test.HUnit
import Text.XML.HXT.Core
import Lib

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where tests = TestList [testParsingEmptySports]


testParsingEmptySports= TestCase $
  do [actual] <- runX
                 ( xunpickleDocument xpSports
                      [ withValidate no
                      , withRemoveWS yes
                      ] "test/exampleSports.xml"
                 )
     assertEqual "The parsed file is not as expected" expected actual
  where
    expected =
      XMLSports
        (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
        Map.empty
