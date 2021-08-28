module Acronym
  ( abbreviate
  ) where

import           Data.Char                      ( isAscii
                                                , isLetter
                                                , isLower
                                                , isUpper
                                                , toUpper
                                                )

abbreviate :: String -> String
abbreviate =
  map (toUpper . head) . words . map charOrBlank . insertBlankIntoCamelCase

charOrBlank :: Char -> Char
charOrBlank c | isLetter c = c
              | otherwise  = ' '

insertBlankIntoCamelCase :: String -> String
insertBlankIntoCamelCase []  = []
insertBlankIntoCamelCase [a] = [a]
insertBlankIntoCamelCase (c1 : c2 : cs)
  | isLower c1 && isUpper c2 = c1 : ' ' : insertBlankIntoCamelCase (c2 : cs)
  | otherwise                = c1 : insertBlankIntoCamelCase (c2 : cs)
