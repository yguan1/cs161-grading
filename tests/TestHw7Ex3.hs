{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TestHw7Ex3 where

import Hw7Ex3 hiding (main, studentsOfTeacher
                     , professorKurtz
                     , professorChugh
                     , allStudents)

import TH
import Test.QuickCheck
import Language.Haskell.TH

-- Expected stuff
--
$(insertTypeNameNotDefined "Person" [d|
    data Person = Student { firstName :: String
                          , lastName :: String
                          , id :: String
                          , major :: String
                          , year :: Int
                          , courses_enrolled :: [(String, (Int, Int))]
                          }
                | Teacher { firstName :: String 
                          , lastName :: String
                          , dept :: String
                          , courses_teaching :: [(Int, Int)]
                          }
        |])

$(autoFail "studentsOfTeacher_" 2)

studentsOfTeacher :: Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher = studentsOfTeacher_ allStudents

professorChugh =
    Teacher "Ravi" "Chugh" "CMSC" [(16100,1)]

professorKurtz =
    Teacher "Stuart" "Kurtz" "CMSC" [(16100,2), (28000,1)]

professorLmao = 
    Teacher "Kek" "Kek" "MATH" [(16100, 1), (16100, 2), (28000, 1)]

professorEmpty = 
    Teacher "" "" "" []

allStudents =
  [ Student "A" "Student" "********" "CMSC" 1 [("CMSC", (15100,1))]
  , Student "B" "Student" "********" "CMSC" 1 [("CMSC", (16100,1))]
  , Student "C" "Student" "********" "CMSC" 2 [("CMSC", (16100,2))]
  , Student "D" "Student" "********" "MATH" 2 [("CMSC", (28000,1))]
  , Student "B" "Kek" "********" "CMSC" 1 [("MATH", (16100,1))]
  , Student "C" "Kek" "********" "CMSC" 2 [("MATH", (16100,2))]
  , Student "D" "Kek" "********" "CMSC" 2 [("MATH", (28000,1))]
  , Student "E" "Student" "********" "MATH" 3 [("CMSC", (28000,1))]
  , Student "F" "Student" "********" "ARTV" 3 [("CMSC", (12100,1))]
  , Student "STEAM" "Student" "********" "ARTV" 4
      [("CMSC", (16100,1)), ("ARTV", (22500,1)), ("ARTV", (22502,1))]
  ]

-- Tests

prop_test1 = studentsOfTeacher professorChugh === 
    [((16100,1),[("Student", "B"),("Student", "STEAM")])]

prop_test2 = studentsOfTeacher professorKurtz ===
    [ ((16100,2),[("Student", "C")])
    , ((28000,1),[("Student", "D"),("Student", "E")])]

prop_test3 = studentsOfTeacher professorLmao ===
    [ ((16100,1),[("Kek", "B")])
    , ((16100,2),[("Kek", "C")])
    , ((28000,1),[("Kek", "D")])]

-- Behavior at empty lists is undefined
prop_empty1 = studentsOfTeacher_ [] professorLmao `seq` True

prop_empty2 = studentsOfTeacher professorEmpty `seq` True

prop_empty3 = studentsOfTeacher_ [] professorEmpty `seq` True


-- RUN ALL TESTS
return []
runTests = $quickCheckAll
main = runTests
