module Main where

import Control.Applicative ( Alternative( (<|>) ) )
import Data.Array ( Ix, Array, (!), (//), array, listArray )
import Data.Maybe ( fromMaybe, mapMaybe )
import System.IO ( stdout, hFlush )
import Text.Read ( readMaybe )

data Piece -- The various different game objects.
  = Red
  | Green
  | Blue
  | Antilight      -- Purple 3-point star.
  | Complement     -- Yellow outward 5-point star.
  | Anticomplement -- Magenta inward 5-point star.
  | Antipurple     -- Dotted white border with purple arrow.
  | Antiorange     -- Dotted white border with orange diamonds.
  | Light          -- Cyan 5-point star made of triangles.
  deriving (Show, Eq, Ord, Enum, Bounded, Ix)

type Board = Array Piece Int

goal :: Board
goal = array (minBound, maxBound) (zip [minBound .. maxBound] (repeat 0))

data Move -- No combination of these should cause an infinite loop.
  = RemoveR         -- Removes 3 red.
  | RemoveG         -- Removes 3 green.
  | RemoveB         -- Removes 3 blue.
  | RemoveA         -- Removes 2 antilight.
  | RemoveAP        -- Removes 3 antipurple.
  | RemoveAO        -- Removes 3 antiorange.
  | MakeA           -- Removes 1 red, green and blue, and adds an antilight.
  | AntilightR      -- Removes 1 antilight, adds 2 red (requires 1 red).
  | AntilightG      -- Removes 1 antilight, adds 2 green (requires 1 green).
  | AntilightB      -- Removes 1 antilight, adds 2 blue (requires 1 blue).
  | AntilightC      -- Removes 1 antilight, adds 2 complements (requires 1 complement).
  | AntilightAC     -- Removes 1 antilight, adds 2 anticomplements (requires 1 anticomplement).
  | AntilightAP     -- Removes 1 antilight, adds 2 antipurple (requires 1 antipurple).
  | AntilightAO     -- Removes 1 antilight, adds 2 antiorange (requires 1 antiorange).
  | AntilightL      -- Removes 1 antilight, adds 2 light (requires 1 light).
  | ComplementRG    -- Removes 1 complement, red and green, and adds a blue.
  | ComplementRB    -- Removes 1 complement, red and blue, and adds a green.
  | ComplementGB    -- Removes 1 complement, green and blue, and adds a red.
  | MakeC           -- Removes 2 antipurple and 1 antiorange, and adds a complement.
  | AnticomplementR -- Removes 1 anticomplement and red, and adds a green and a blue.
  | AnticomplementG -- Removes 1 anticomplement and green, and adds a red and a blue.
  | AnticomplementB -- Removes 1 anticomplement and blue, and adds a red and a green.
  | MakeAC          -- Removes 1 antipurple and 2 antiorange, and adds an anticomplement.
  | LightC          -- Removes 1 light and complement, and adds 2 antipurple and an antiorange.
  | LightAC         -- Removes 1 light and anticomplement, and adds an antipurple and 2 antiorange.
  | MakeL           -- Removes 1 complement and anticomplement, adds a light.
  deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show Move where
  show RemoveR         = "3R -> X"
  show RemoveG         = "3G -> X"
  show RemoveB         = "3B -> X"
  show RemoveA         = "2A -> X"
  show RemoveAP        = "3AP -> X"
  show RemoveAO        = "3AO -> X"
  show MakeA           = "1R, 1G, 1B -> 1A"
  show AntilightR      = "1A, 1R -> 3R"
  show AntilightG      = "1A, 1G -> 3G"
  show AntilightB      = "1A, 1B -> 3B"
  show AntilightC      = "1A, 1C -> 3C"
  show AntilightAC     = "1A, 1AC -> 3AC"
  show AntilightAP     = "1A, 1AP -> 3AP"
  show AntilightAO     = "1A, 1AO -> 3AO"
  show AntilightL      = "1A, 1L -> 3L"
  show ComplementRG    = "1C, 1R, 1G -> 1B"
  show ComplementRB    = "1C, 1R, 1B -> 1G"
  show ComplementGB    = "1C, 1G, 1B -> 1R"
  show MakeC           = "2AP, 1AO -> 1C"
  show AnticomplementR = "1AC, 1R -> 1G, 1B"
  show AnticomplementG = "1AC, 1G -> 1R, 1B"
  show AnticomplementB = "1AC, 1B -> 1R, 1G"
  show MakeAC          = "1AP, 2AO -> 1AC"
  show LightC          = "1L, 1C -> 2AP, 1AO"
  show LightAC         = "1L, 1AC -> 1AP, 2AO"
  show MakeL           = "1C, 1AC -> 1L"

moves :: Board -> [Move]
moves board = mapMaybe ($ board)
  [ RemoveR         `requires` [atLeast 3 Red]
  , RemoveG         `requires` [atLeast 3 Green]
  , RemoveB         `requires` [atLeast 3 Blue]
  , RemoveA         `requires` [atLeast 2 Antilight]
  , RemoveAP        `requires` [atLeast 3 Antipurple]
  , RemoveAO        `requires` [atLeast 3 Antiorange]
  , MakeA           `requires` [atLeast 1 Red, atLeast 1 Green, atLeast 1 Blue]
  , AntilightR      `requires` [atLeast 1 Antilight, atLeast 1 Red]
  , AntilightG      `requires` [atLeast 1 Antilight, atLeast 1 Green]
  , AntilightB      `requires` [atLeast 1 Antilight, atLeast 1 Blue]
  , AntilightC      `requires` [atLeast 1 Antilight, atLeast 1 Complement]
  , AntilightAC     `requires` [atLeast 1 Antilight, atLeast 1 Anticomplement]
  , AntilightAP     `requires` [atLeast 1 Antilight, atLeast 1 Antipurple]
  , AntilightAO     `requires` [atLeast 1 Antilight, atLeast 1 Antiorange]
  , AntilightL      `requires` [atLeast 1 Antilight, atLeast 1 Light]
  , ComplementRG    `requires` [atLeast 1 Complement, atLeast 1 Red, atLeast 1 Green]
  , ComplementRB    `requires` [atLeast 1 Complement, atLeast 1 Red, atLeast 1 Blue]
  , ComplementGB    `requires` [atLeast 1 Complement, atLeast 1 Green, atLeast 1 Blue]
  , MakeC           `requires` [atLeast 1 Antipurple, atLeast 2 Antiorange]
  , AnticomplementR `requires` [atLeast 1 Anticomplement, atLeast 1 Red]
  , AnticomplementG `requires` [atLeast 1 Anticomplement, atLeast 1 Green]
  , AnticomplementB `requires` [atLeast 1 Anticomplement, atLeast 1 Blue]
  , MakeAC          `requires` [atLeast 2 Antipurple, atLeast 1 Antiorange]
  , LightC          `requires` [atLeast 1 Light, atLeast 1 Complement]
  , LightAC         `requires` [atLeast 1 Light, atLeast 1 Anticomplement]
  , MakeL           `requires` [atLeast 1 Complement, atLeast 1 Anticomplement]
  ]
  where
    requires move preds b = if all ($ b) preds then Just move else Nothing
    atLeast m p brd       = (brd ! p) >= m

performMove :: Move -> Board -> Board
-- Pre: Move is legal.
performMove RemoveR         = remove 3 Red
performMove RemoveG         = remove 3 Green
performMove RemoveB         = remove 3 Blue
performMove RemoveA         = remove 2 Antilight
performMove RemoveAP        = remove 3 Antipurple
performMove RemoveAO        = remove 3 Antiorange
performMove MakeA           = remove 1 Red `next` remove 1 Green `next` remove 1 Blue `next` add 1 Antilight
performMove AntilightR      = remove 1 Antilight `next` add 2 Red
performMove AntilightG      = remove 1 Antilight `next` add 2 Green
performMove AntilightB      = remove 1 Antilight `next` add 2 Blue
performMove AntilightC      = remove 1 Antilight `next` add 2 Complement
performMove AntilightAC     = remove 1 Antilight `next` add 2 Anticomplement
performMove AntilightAP     = remove 1 Antilight `next` add 2 Antipurple
performMove AntilightAO     = remove 1 Antilight `next` add 2 Antiorange
performMove AntilightL      = remove 1 Antilight `next` add 2 Light
performMove ComplementRG    = remove 1 Complement `next` remove 1 Red `next` remove 1 Green `next` add 1 Blue
performMove ComplementRB    = remove 1 Complement `next` remove 1 Red `next` remove 1 Blue `next` add 1 Green
performMove ComplementGB    = remove 1 Complement `next` remove 1 Green `next` remove 1 Blue `next` add 1 Red
performMove MakeC           = remove 2 Antipurple `next` remove 1 Antiorange `next` add 1 Complement
performMove AnticomplementR = remove 1 Anticomplement `next` remove 1 Red `next` add 1 Green `next` add 1 Blue
performMove AnticomplementG = remove 1 Anticomplement `next` remove 1 Green `next` add 1 Red `next` add 1 Blue
performMove AnticomplementB = remove 1 Anticomplement `next` remove 1 Blue `next` add 1 Red `next` add 1 Green
performMove MakeAC          = remove 1 Antipurple `next` remove 2 Antiorange `next` add 1 Anticomplement
performMove LightC          = remove 1 Light `next` remove 1 Complement `next` add 2 Antipurple `next` add 1 Antiorange
performMove LightAC         = remove 1 Light `next` remove 1 Anticomplement `next` add 1 Antipurple `next` add 2 Antiorange
performMove MakeL           = remove 1 Complement `next` remove 1 Anticomplement `next` add 1 Light

add, remove :: (Ix i, Integral e) => e -> i -> Array i e -> Array i e
add amt p b = b // [(p, (b ! p) + amt)]
remove amt p b = b // [(p, (b ! p) - amt)]

next :: (a -> b) -> (b -> c) -> a -> c
next = flip (.)

solve :: Board -> Maybe [Move]
solve board
  | board == goal = Just []
  | otherwise     = case moves board of
    [] -> Nothing
    ms -> foldl (<|>) Nothing (fmap (\ m -> (m :) <$> solve (performMove m board)) ms)

main :: IO ()
main = do
  r  <- fromMaybe 0 . readMaybe <$> prompt "            How many Reds (R): "
  g  <- fromMaybe 0 . readMaybe <$> prompt "          How many Greens (G): "
  b  <- fromMaybe 0 . readMaybe <$> prompt "           How many Blues (B): "
  a  <- fromMaybe 0 . readMaybe <$> prompt "      How many Antilights (A): "
  c  <- fromMaybe 0 . readMaybe <$> prompt "     How many Complements (C): "
  ac <- fromMaybe 0 . readMaybe <$> prompt "How many Anticomplements (AC): "
  ap <- fromMaybe 0 . readMaybe <$> prompt "    How many Antipurples (AP): "
  ao <- fromMaybe 0 . readMaybe <$> prompt "    How many Antioranges (AO): "
  l  <- fromMaybe 0 . readMaybe <$> prompt "          How many Lights (L): "
  case solve (listArray (minBound, maxBound) [r, g, b, a, c, ac, ap, ao, l]) of
    Nothing -> putStrLn "No solution found."
    Just ms -> putStrLn "Solution found (X = Remove):" >> putStr (showSol ms)
  where
    fls         = hFlush stdout
    prompt msg  = putStr msg >> fls >> getLine
    showSol sol = foldr (++) "" (fmap (\ m -> concat ["- ", show m, "\n"]) sol)
