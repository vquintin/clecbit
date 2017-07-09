import qualified Data.Map as Map
import Data.Time
import Test.HUnit
import Text.XML.HXT.Core
import Lib

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where tests = TestList [ testParsingEmptySports
                         , testParsingEmptySport
                         ]


testParsingEmptySports = parsingTest "test/exampleSports.xml" $
  XMLSports
    (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
    Map.empty

testParsingEmptySport = parsingTest "test/exampleSport.xml" $
  XMLSports
    (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
    (Map.fromList
      [ (1 , XMLSport "Football" Map.empty)
      ]
    )

parsingTest :: FilePath -> XMLSports -> Test
parsingTest file expected = TestCase $
  do [actual] <- runX
                 ( xunpickleDocument xpSports
                      [ withValidate no
                      , withRemoveWS yes
                      ] file
                 )
     assertEqual "The parsed file is not as expected" expected actual
