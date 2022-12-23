import Lean

open Lean (RBMap)

abbrev Pos := Int × Int

def comparePos (a b : Pos) :=
  match compare a.1 b.1 with
  | .eq => compare a.2 b.2
  | x   => x

abbrev Map := RBMap (Int × Int) Nat comparePos

def readMap (content : String) : IO Map := do
  let mut data : RBMap (Int × Int) Nat comparePos := .empty
  let lines := content.trim.splitOn "\n"
  for (r,line) in lines.enum do
    for (c,ch) in line.toList.enum do
      if ch == '#' then data := data.insert (r,c) 1
  pure data

abbrev Dims := Int × Int × Int × Int
def Map.dims (map : Map) : Dims :=
  let rec loop : List ((Int × Int ) × Nat) -> Dims -> Dims
  | [], dims => dims
  | ((r,c), _) :: rest, (minr, maxr, minc, maxc) =>
      loop rest (min minr r, max maxr r, min minc c, max maxc c)
  loop map.toList (0,0,0,0)

abbrev PMap := RBMap Pos (List Pos) comparePos

def Map.check (map : Map) : Pos -> Fin 4 -> Option Pos
| (r,c), 0 =>
    if not (map.contains (r-1,c-1) ||map.contains (r-1,c) ||map.contains (r-1,c+1)) then (r-1,c) else .none
| (r,c), 1 =>
    if not (map.contains (r+1,c-1) ||map.contains (r+1,c) ||map.contains (r+1,c+1)) then (r+1,c) else .none
| (r,c), 2 =>
    if not (map.contains (r-1,c-1) ||map.contains (r,c-1) ||map.contains (r+1,c-1)) then (r,c-1) else .none
| (r,c), 3 =>
    if not (map.contains (r-1,c+1) ||map.contains (r,c+1) ||map.contains (r+1,c+1)) then (r,c+1) else .none

def Map.propose (map : Map) (round : Nat) (pos : Pos) : Option Pos := do
  for i in [0:4] do
    -- the loop shifts each round
    let dir : Fin 4 := .ofNat ((i + round)  % 4)
    if let .some pos' := map.check pos dir then return pos'
  pos

def Map.update (map : Map) : List (Pos × List Pos) -> IO Map
| ((pos', [elf]) :: rest) => do
    -- println! "move {elf} {pos'}"
    Map.update ((map.erase elf).insert pos' 1) rest
| ((_, elves) :: rest) => map.update rest
| [] => pure map

def Map.crowded (map : Map) (pos : Pos) : Bool :=
  let (r,c) := pos
  #[(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)].any map.contains

partial
def Map.step (map : Map) (round : Nat) : IO (Map × Bool) := do
  -- round 1
  -- make a list of proposed moves
  let mut round1 : PMap := .empty
  let mut stop := true
  for (pos, _) in map.toList do
    if map.crowded pos then
      stop := false
    -- Ignore non-movers
      -- println! "{pos} {map.propose round pos}"
      if let .some pos' := map.propose round pos then
        round1 := round1.insert pos' (pos :: round1.findD pos' [])

  let map <- map.update round1.toList
  -- println! "round {round} count {stop}"
  pure (map, stop)

partial
def Map.part2 (map : Map) (round : Nat) : IO Nat := do
  let map <- map.step round
  match map with
  | (_,   true) => pure (round + 1)
  | (map, false) => map.part2 (round + 1)

def dump (map : Map) : IO Unit := do
  let (rmin,rmax,cmin,cmax) := map.dims
  let a := (rmax-rmin+1).toNat
  let b := (cmax-cmin+1).toNat
  println! "DUMP {a} {b}"
  for dr in [:a] do
    for dc in [:b] do
      let pos := (rmin + dr, cmin + dc)
      IO.print (if map.contains pos  then "#" else ".")
    IO.println ""

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let data <- readMap content
  -- println! (repr data)
  -- dump data
  let mut data := data
  for i in [0:10] do
    let (map,_) <- data.step i
    data := map
  -- dump data
  let (a,b,c,d) := data.dims
  let area := (b + 1 - a) * (d + 1 - c)
  let elves := data.size
  let space := area - elves
  -- println! "dims {data.dims} {area} {elves} {area-elves}"
  println! "{fname} part1 {space}"
  data <- readMap content

  let part2 <- data.part2 0
  println! "{fname} part2 {part2}"

#eval main ["day23/eg.txt"]
-- #eval main ["day23/input.txt"]