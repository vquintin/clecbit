import ClecBit.XML
import qualified Data.Map as Map
import Data.Time
import Test.HUnit
import Text.XML.HXT.Core

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where
    tests =
      TestList
        [ testParsingEmptySports
        , testParsingEmptySport
        , testParsingEmptyEvent
        , testParsingEmptyBets
        , testParsingAll
        ]

testParsingEmptySports =
  parsingTest "test/exampleSports.xml" $
  Sports
    (parseTimeOrError
       False
       defaultTimeLocale
       "%FT%X%Q"
       "2017-07-07T20:55:17.403")
    Map.empty

testParsingEmptySport =
  parsingTest "test/exampleSport.xml" $
  Sports
    (parseTimeOrError
       False
       defaultTimeLocale
       "%FT%X%Q"
       "2017-07-07T20:55:17.403")
    (Map.fromList [(1, Sport "Football" Map.empty)])

testParsingEmptyEvent =
  parsingTest "test/exampleEvent.xml" $
  Sports
    (parseTimeOrError
       False
       defaultTimeLocale
       "%FT%X%Q"
       "2017-07-07T20:55:17.403")
    (Map.fromList
       [ ( 1
         , Sport "Football" $
           Map.fromList [(3, Event "Eng. Premier League" Map.empty)])
       ])

testParsingEmptyBets =
  parsingTest "test/exampleBets.xml" $
  Sports
    (parseTimeOrError
       False
       defaultTimeLocale
       "%FT%X%Q"
       "2017-07-07T20:55:17.403")
    (Map.fromList
       [ ( 1
         , Sport "Football" $
           Map.fromList
             [ ( 3
               , Event "Eng. Premier League" $
                 Map.fromList
                   [ ( 1429761
                     , Match
                         (parseTimeOrError
                            False
                            defaultTimeLocale
                            "%FT%X"
                            "2017-08-11T18:45:00")
                         "Arsenal - Leicester"
                         (Bets Map.empty))
                   ])
             ])
       ])

testParsingAll =
  parsingTest "test/example.xml" $
  Sports
    (parseTimeOrError
       False
       defaultTimeLocale
       "%FT%X%Q"
       "2017-07-07T20:55:17.403")
    (Map.fromList
       [ ( 1
         , Sport "Football" $
           Map.fromList
             [ ( 3
               , Event "Eng. Premier League" $
                 Map.fromList
                   [ ( 1429761
                     , Match
                         (parseTimeOrError
                            False
                            defaultTimeLocale
                            "%FT%X"
                            "2017-08-11T18:45:00")
                         "Arsenal - Leicester"
                         (Bets $
                          Map.fromList
                            [ ( 35410981
                              , Bet "Ftb_Mr3" "Match Result" $
                                Map.fromList
                                  [ (274179107, Choice "%1%" 1.35)
                                  , (274179108, Choice "Draw" 4.75)
                                  , (274179109, Choice "%2%" 8.0)
                                  ])
                            ]))
                   ])
             ])
       ])

parsingTest :: FilePath -> Sports -> Test
parsingTest file expected =
  TestCase $ do
    [actual] <-
      runX (xunpickleDocument xpSports [withValidate no, withRemoveWS yes] file)
    assertEqual "The parsed file is not as expected" expected actual
