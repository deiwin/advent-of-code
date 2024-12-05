Start with some housekeeping.

```haskell
module Day04 (main) where

import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Ix (inRange, range)
import Linear.V2 (V2 (..))
import Test.HUnit.Base (Test (TestCase), (@?=))
import Test.HUnit.Text (runTestTT)
```

The parser is simple for this one, resulting in a 2D list of characters.

```haskell
parse :: String -> [[Char]]
parse = lines
```

Add some type aliases to make the code more readable. `Coord` represents a 2D
coordinate, as `V2 y x`, and `Grid` is a random-access data structure (an array
in this case) that maps elements from `Coord`s to the characters at that
coordinate.

```haskell
type Coord = V2 Int

type Grid = A.Array Coord Char
```

Create a `Grid` from the parsed input.

```haskell
createGrid :: [[Char]] -> Grid
createGrid rows = A.listArray bounds $ concat rows
  where
    bounds = (V2 0 0, V2 (length rows - 1) (length (head rows) - 1))
```

Now, for the first part, we will first find all the 'X' characters and then
start reading from them in all 8 diections to see if we can spell "XMAS". To do
this, we first define all 8 directions as relative coordinates.

```haskell
xmasDirs :: [Coord]
xmasDirs =
  [ V2 0 1, -- right
    V2 1 1, -- down right
    V2 1 0, -- down
    V2 1 (-1), -- down left
    V2 0 (-1), -- left
    V2 (-1) (-1), -- up left
    V2 (-1) 0, -- up
    V2 (-1) 1 -- up right
  ]
```

Then, from each of these directions, we create a `Path` of 4 relative
coordinates that start from `V2 0 0` (marking the current 'X' character) and
move in the given direction 4 times. We do this by repeatedly adding the
relative direction coordinate first the starting position (`V2 0 0`) and then
to each subsequent position.

```haskell
type Path = [Coord]

xmasPaths :: [Path]
xmasPaths =
  xmasDirs
    & fmap (\dir -> take 4 $ iterate (+ dir) (V2 0 0))
```

As these paths are relative to the 'X' character, then we need a way to convert
them to absolute coordinates. We do this by taking a starting coordinate and
then for each coordinate in each relative path we add the starting coordinate.

```haskell
absXmasPaths :: Coord -> [Path]
absXmasPaths coord =
  xmasPaths
    & fmap (fmap (coord +))
```

Now, we can find the coordinates of the 'X' characters in the grid. The `range`
function gives us all the coordinates that are within the bounds of the grid,
and we filter them by whether they map to an 'X' character.

```haskell
xCoords :: Grid -> [Coord]
xCoords grid = filter (\coord -> grid A.! coord == 'X') $ range bounds
  where bounds = A.bounds grid
```

Using the above we can now find all candidate absolute `Path`s (i.e. `Coord`
lists) that could potentially spell "XMAS".

```haskell
candidates :: Grid -> [Path]
candidates grid =
  grid
  & xCoords
  & concatMap absXmasPaths
```

Some of these paths may currently go out of bounds, so we need to filter those
out.

```haskell
candidatesInBounds :: Grid -> [Path]
candidatesInBounds grid =
  grid
  & candidates
  & filter (all (inRange bounds))
  where bounds = A.bounds grid
```

Now we also need a way to check whether a particular path spells "XMAS" or not.
We do this by mapping each coordinate within a path to the character at that
coordinate. And as a list of characters is a string, then we can compare the
result to the expected string.

```haskell
isXMAS :: Grid -> Path -> Bool
isXMAS grid path = fmap (grid A.!) path == "XMAS"
```

Finally, we can put all of this together to solve the first part by counting
the paths that spell "XMAS".

```haskell
solve1 :: [[Char]] -> Int
solve1 input =
  grid
    & candidatesInBounds
    & filter (isXMAS grid)
    & length
  where
    grid = createGrid input
```

For part two, we will find all 'A' characters and search for the relevant
pattern around them. For this we create a new `Path` that reads the cross shape
first from top left to bottom right and then from top right to bottom left.

```haskell
crossMAS :: Path
crossMAS =
  [ V2 (-1) (-1), -- up left
    V2 0 0, -- center
    V2 1 1, -- down right
    V2 (-1) 1, -- up right
    V2 0 0, -- center
    V2 1 (-1) -- down left
  ]
```

Again, we need a way to convert this relative path to an absolute path by
adding all of the coordinates to a starting coordinate.

```haskell
absCrossMASPath :: Coord -> Path
absCrossMASPath coord =
  crossMAS
    & fmap (coord +)
```

Now, we can find the coordinates of the 'A' characters in the grid.

```haskell
aCoords :: Grid -> [Coord]
aCoords grid = filter (\coord -> grid A.! coord == 'A') $ range bounds
  where bounds = A.bounds grid
```

From the above we can again create candidate absolute `Path`s to look at.

```haskell
candidates' :: Grid -> [Path]
candidates' grid =
  grid
  & aCoords
  & fmap absCrossMASPath
```

And filter out those that go out of bounds.

```haskell
candidatesInBounds' :: Grid -> [Path]
candidatesInBounds' grid =
  grid
  & candidates'
  & filter (all (inRange bounds))
  where bounds = A.bounds grid
```

Now, to find all the valid strings that would indicate the X-MAS pattern if
read by the above `Path`, we create all possible combinations of "MAS" and
"SAM" and then concatenate them (resulting in e.g. "MASMAS", etc). This
approach is a bit of an overkill as there are only 4 such combinations, but 🤷

```haskell
cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

selfCartProd :: [a] -> [(a, a)]
selfCartProd xs = cartProd xs xs

crossMASCombinations :: [[Char]]
crossMASCombinations =
  ["MAS", "SAM"]
    & selfCartProd
    & fmap (uncurry (++))
```

This allows us to check whether a particular path forms the expected X-MAS
pattern.

```haskell
isCrossMAS :: Grid -> Path -> Bool
isCrossMAS grid path =
  path
    & fmap (grid A.!)
    & (`elem` crossMASCombinations)
```

Finally, we can put all of this together to solve the second part by counting
all the paths that form the X-MAS pattern.

```haskell
solve2 :: [[Char]] -> Int
solve2 input =
  grid
    & candidatesInBounds'
    & filter (isCrossMAS grid)
    & length
  where
    grid = createGrid input
```

And to verify that the above solutions work, we run them against the provided
input.

```haskell
main = do
  input <- readFile "inputs/Day04.txt"
  runTestTT $
    TestCase $ do
      solve1 (parse input) @?= 2406
      solve2 (parse input) @?= 1807
```
