module Main where

import System.Random  -- for randoms
import System.IO      -- for hFlush

type Row = [Int]
type Guess = Row
type Solution = Row

colors, width :: Int
colors = 6
width  = 4

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    s <- generateSolution -- initialization
    loop s                -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution :: IO [Int]
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution -> IO ()
loop s =
  do
    i <- input            -- read (and parse) the user input
    tODO (return ())


black, white :: Solution -> Guess -> Int
black solution guess = tODO 0
white solution guess = tODO 0

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = (black solution guess,
                        white solution guess,
                        tODO False)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (blackScore, whiteScore, correct) = tODO ""

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    tODO (return [])

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = tODO True
