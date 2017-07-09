import qualified Data.Map as Map
import Data.Time
import Test.HUnit
import Text.XML.HXT.Core
import Lib

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where tests = TestList [ testParsingEmptySports
                         , testParsingEmptySport
                         , testParsingEmptyEvent
                         , testParsingEmptyBets
                         , testParsingAll
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

testParsingEmptyEvent = parsingTest "test/exampleEvent.xml" $
  XMLSports
    (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
    (Map.fromList
      [ (1 , XMLSport "Football" $ Map.fromList
        [ (3, XMLEvent "Eng. Premier League" Map.empty)
        ])
      ]
    )

testParsingEmptyBets = parsingTest "test/exampleBets.xml" $
  XMLSports
    (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
    (Map.fromList
      [ (1 , XMLSport "Football" $ Map.fromList
        [ (3, XMLEvent "Eng. Premier League" $ Map.fromList
          [ (1429761, XMLMatch (parseTimeOrError False defaultTimeLocale "%FT%X" "2017-08-11T18:45:00")
                     "Arsenal - Leicester"
                     (XMLBets Map.empty)
            )
          ])
        ])
      ]
    )

testParsingAll = parsingTest "test/example.xml" $
  XMLSports
    (parseTimeOrError False defaultTimeLocale "%FT%X%Q" "2017-07-07T20:55:17.403")
    (Map.fromList
      [ (1 , XMLSport "Football" $ Map.fromList
        [ (3, XMLEvent "Eng. Premier League" $ Map.fromList
          [ (1429761, XMLMatch (parseTimeOrError False defaultTimeLocale "%FT%X" "2017-08-11T18:45:00")
                     "Arsenal - Leicester"
                     (XMLBets $ Map.fromList
                       [ (35410981, XMLBet "Ftb_Mr3" "Match Result" $ Map.fromList
                         [ (274179107, XMLChoice "%1%" 1.35)
                         , (274179108, XMLChoice "Draw" 4.75)
                         , (274179109, XMLChoice "%2%" 8.0)
                         ])
                       ])
            )
          ])
        ])
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
