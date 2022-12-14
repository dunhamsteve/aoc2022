import Lean
import Lean.Data.HashMap
import Lean.Parser
open Lean.Parsec
open Lean (HashMap Parsec)  

def pNat := do
  let digs <- many1 digit
  pure digs.data.asString.toNat!

def pPair := do
  let a <- pNat
  skipChar ','
  let b <- pNat
  pure (a,b)

def someSep (p : Parsec α) (s : Parsec Unit) := do
  let a <- p
  let as <- many (s *> p)
  pure (a :: as.toList)

def pLine := someSep pPair (skipString " -> ") 

abbrev Point := Nat × Nat
abbrev Grid := HashMap Point Bool

partial
def readLine (data : Grid) (line : String) : Grid :=
  let points := (pLine.run line).toOption.get!
  let rec loop : Grid -> List Point -> Grid
  | grid, (a,b) :: (c,d) :: rest =>
    let grid' := grid.insert (a,b) true
         if a < c then loop grid' ((a+1,b) :: (c,d) :: rest)
    else if a > c then loop grid' ((a-1,b) :: (c,d) :: rest)
    else if b < d then loop grid' ((a,b+1) :: (c,d) :: rest)
    else if b > d then loop grid' ((a,b-1) :: (c,d) :: rest)
    else loop grid' ((c,d)::rest)
  | grid, _ => grid
  loop data points

partial
def drop (grid : Grid) (maxy : Nat) (grains : Nat) : Nat :=
  let rec loop : Point -> Nat
  | (x,y) =>
    if y > maxy then grains
    else if not (grid.contains (x,y+1)) then loop (x,y+1)
    else if not (grid.contains (x-1,y+1)) then loop (x-1,y+1)
    else if not (grid.contains (x+1,y+1)) then loop (x+1,y+1)
    else if (x,y) == (500,0) then grains
    else drop (grid.insert (x,y) false) maxy (grains + 1)
  loop (500,0)

partial
def drop2 (grid : Grid) (maxy : Nat) (grains : Nat) : Nat :=
  let rec loop : Point -> Point
  | (x,y) =>
    if y > maxy then (x,y)
    else if not (grid.contains (x,y+1)) then loop (x,y+1)
    else if not (grid.contains (x-1,y+1)) then loop (x-1,y+1)
    else if not (grid.contains (x+1,y+1)) then loop (x+1,y+1)
    else (x,y)
  let insert := loop (500,0)
  if insert == (500,0) then grains + 1
    else drop2 (grid.insert insert false) maxy (grains + 1)
  
def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let lines := (content.splitOn "\n").filter (· != "")
  let grid := lines.foldl readLine .empty
  let maxy := (grid.toList.map (·.fst.snd)).foldl max 0
  let part1 := drop grid maxy 0
  let part2 := drop2 grid maxy 0
  println! "{fname} part1 {part1} {part2}"

#eval main ["day14/eg.txt"]
#eval main ["day14/input.txt"]
