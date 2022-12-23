import Lean
open Lean (RBMap Parsec)
open Lean.Parsec

inductive Step where
| left : Step
| right : Step
| go : Nat -> Step
deriving Repr

abbrev Key := List Step
abbrev Pos := Int × Int



structure Face where
  ix : Fin 6
  pos : Pos
  dir : Fin 4
deriving Repr

abbrev FaceMap := RBMap Int Face compare

structure Map where
  faces : FaceMap
  data : Array (Array Char)
  size : Nat
deriving Repr

-- I'm gonna keep the part1 state, see below
structure State where
  map : Map
  dir : Fin 4
  pos : Pos

-- get value, ' ' for out of bounds
def Map.get (map : Map) (pt : Pos) : Char :=
  let (r,c) := pt
  if r < 0 then ' ' else
  if c < 0 then ' ' else
  match map.data.get? r.toNat with
  | .none => ' '
  | .some row => match row.get? c.toNat with
      | .none => ' '
      | .some c => c

/-
Faces are:
  0
2 1 3
  4
  5

Dir:
  3
2   0
  1

Fin 4 also for rotation.
-/

-- Look in direction, get face and rotation
-- Leaving Fin in here because I think I want to add rotation to direction.
-- Direction 0 is east
def look : (Fin 6) -> (Fin 4) -> (Fin 6 × Fin 4)
| 0, 0 => (3,3)
| 0, 1 => (1,0)
| 0, 2 => (2,1)
| 0, 3 => (5,0)
| 1, 0 => (3,0)
| 1, 1 => (4,0)
| 1, 2 => (2,0)
| 1, 3 => (0,0)
| 2, 0 => (1,0)
| 2, 1 => (4,1)
| 2, 2 => (5,2)
| 2, 3 => (0,3)
| 3, 0 => (5,2)
| 3, 1 => (4,3)
| 3, 2 => (1,0)
| 3, 3 => (0,1)
| 4, 0 => (3,1)
| 4, 1 => (5,0)
| 4, 2 => (2,3)
| 4, 3 => (1,0)
| 5, 0 => (3,2)
| 5, 1 => (0,0)
| 5, 2 => (2,2)
| 5, 3 => (4,0)

-- Add face to faceMap, the explore neighbors.
-- we can deduplicate this later, it seems to find things for eg.txt
partial
def Map.addFace (map : Map) (face : Face) : IO Map := do
  let mut map := { map with faces := map.faces.insert face.ix face }
  let (r,c) := face.pos
  
  -- north
  -- guard these because they're Nat and 0 - size = 0 gives a false positive
  if r >= map.size then
    let pos := (r-map.size,c)
    let (ix, d) := look face.ix (3 - face.dir)
    if not (map.faces.contains ix) && map.get pos != ' ' then
      println! "{repr face} has neighbor {ix} to the north at {pos}"
      -- check that face is not taken and data exists
      -- TODO add or subtract or what?  Might need to rotate, oof
      map <- map.addFace { ix, pos, dir := d + face.dir % 4 }
    else
      println! "miss {ix} {pos}"

  if c >= map.size then  
    let pos := (r,c-map.size)
    let (ix,d) := look face.ix (6 - face.dir % 4)
    if not (map.faces.contains ix) && map.get pos != ' ' then
        println! "{repr face} has neighbor {ix} to the west at {pos}"
        map <- map.addFace { ix, pos, dir := d + face.dir % 4 }

  -- east
  if true then
    let pos := (r, c + map.size)
    let (ix,d) := look face.ix (4 - face.dir)
    if not (map.faces.contains ix) && map.get pos != ' ' then
      println! "{repr face} has neighbor {ix} to the east at {pos}"
      map <- map.addFace { ix, pos, dir := d + face.dir % 4 }

  -- south
  if true then 
    let pos := (r+map.size, c)
    println! "check south at {pos}"
    let (ix,d) := look face.ix (5 - face.dir % 4)
    if not (map.faces.contains ix) && map.get pos != ' ' then
      println! "{repr face} has neighbor {ix} to the south at {pos}"
      map <- map.addFace { ix, pos, dir := d + face.dir % 4 }
    
  pure map

def makeMap (content : String) (size : Nat): IO Map := do
    let data := (content.splitOn "\n").toArray.map λ l => (String.toList l).toArray
    let start := (data[0]!.findIdx? (· != ' ') ).get!
    
    println! "start {start}"
    let faces : FaceMap := .empty
    let map : Map := { data, faces, size } 
    let map <- map.addFace (Face.mk 0 (0,start) 0)
    pure map

def pnum := do
  let digs <- many1 digit 
  pure digs.data.asString.toNat!

def pStep : Parsec Step := 
  pchar 'R' *> pure .right 
  <|> pchar 'L' *> pure .left
  <|> .go <$> pnum

def pPath  : Parsec (Array Step) :=
  many pStep

def State.getFace (st : State) : Option Face := do
  let (r,c) := st.pos
  for (_, face) in st.map.faces do
    if r >= face.pos.1 && r < face.pos.1 + st.map.size
      && c >= face.pos.2 && c < face.pos.2 + st.map.size then
      return face
  .none

def Pos.move : Pos -> Fin 4 -> Pos
| (r,c),0 => (r,c+1)
| (r,c),1 => (r+1,c)
| (r,c),2 => (r,c-1)
| (r,c),3 => (r-1,c)


-- TODO add proofs, drop Inhabited, etc
def Array.findIdxRev? [Inhabited α] (arr : Array α) (pred : α -> Bool) : Option Nat :=
  let rec loop : Nat -> Option Nat
    | 0 => .none
    | x + 1 => if pred arr[x]! then x else loop x
  loop arr.size


def p1Forward (st : State) := do
  let pos' := st.pos.move st.dir
  if st.map.get pos' != ' ' then pos' else
  let r := st.pos.1.toNat
  let c := st.pos.2.toNat
  match st.dir with
  | 0 => let c' := (st.map.data[r]!.findIdx? (· != ' ')).get!; (r, c')
  | 2 => let c' := (st.map.data[r]!.findIdxRev? (· != ' ')).get!; (r, c')
  | 1 => let r' := (st.map.data.findIdx? (λ row => row[c]? != .some ' ')).get!; (r',c)
  | 3 => let r' := (st.map.data.findIdxRev? (λ row => row[c]? != .some ' ' && row[c]? != .none)).get!; (r',c)

partial -- terminates on k decreasing
def runPart1 (st : State) : Step -> Option State
| .left => let dir := if st.dir == 0 then 3 else st.dir - 1
           .some {st with dir}
| .right => let dir := if st.dir == 3 then 0 else st.dir + 1
            .some {st with dir}
| .go 0 => st
| .go k => do
    let pos <- p1Forward st
    let ch := st.map.get pos
    if ch == '#' then 
      st
    else if ch != '.' then
      dbg_trace "DRIFTING {st.dir} {st.pos} {pos} '{ch}' {st.map.size}"; st
    else
      runPart1 {st with pos} (.go (k - 1))

def minus : Pos -> Pos -> Pos
| (a,b), (c,d) => (a-c,b-d)

def plus : Pos -> Pos -> Pos
| (a,b), (c,d) => (a+c,b+d)

-- position relative to me, to position relative to cell in dir
def State.adj (st : State) : Pos -> Fin 4 -> Pos
| (r,c),0 => (r, c - st.map.size)
| (r,c),1 =>  (r - st.map.size, c)
| (r,c),2 => (r, c + st.map.size)
| (r,c),3 => (r + st.map.size,c)

partial -- it should see this one.
def State.rot (st : State) : Pos -> Fin 4 -> Pos
| (r,c), 0 => (r,c)
| (r,c), k => st.rot (c, st.map.size - r - 1) (k - 1)

def forward (st : State) : Option (Fin 4 × Pos) := do
  let pos' := st.pos.move st.dir
  if st.map.get pos' != ' ' then (st.dir, pos') else

  let face <- st.getFace
  -- we left face in dir
  let lookDir := (st.dir + 4 - face.dir) % 4
  let (ix',dir') := look face.ix lookDir
  let nextFace <- st.map.faces.find? ix'
  
  let nextDir := nextFace.dir
  let relRot := (4 - dir' + 4 - face.dir + nextDir) % 4

  -- need some tweaks to direction here in the general case
  let relPos' := minus pos' face.pos
  let adjPos' := st.adj relPos' st.dir -- in adjacent version of next cell
  let rotPos' := st.rot adjPos' relRot
  let xlPos' := plus rotPos' nextFace.pos

  -- we've gone right one, so adjust dir
  let adjdir := (st.dir + relRot) % 4

  -- dbg_trace "rel {relPos'} adj {adjPos'} lookDir {lookDir} dir' {dir'} adjdir {adjdir} nextDir {nextDir} foo {foo} rot {rotPos'} xl {xlPos'}"
  -- dbg_trace "*** face {repr face} next {ix'} {dir'} pos {repr st.pos} pos' {pos'} nf {repr nextFace}";
  (adjdir,xlPos')
  
partial -- terminates on k decreasing
def runPart2 (st : State) : Step -> Option State
| .left => let dir := if st.dir == 0 then 3 else st.dir - 1
           .some {st with dir}
| .right => let dir := if st.dir == 3 then 0 else st.dir + 1
            .some {st with dir}
| .go 0 => st
| .go k => do
    let (dir,pos) <- forward st
    let ch := st.map.get pos
    -- let ch := dbg_trace "look at {ch} {dir} {st.pos} {pos}"; ch
    if ch == '#' then 
      st
    else if ch != '.' then
      dbg_trace "DRIFTING {st.pos} {pos} {ch} {st.map.size}"; st
    else
      runPart2 {st with pos, dir} (.go (k - 1))
      
def main (argv : List String) := do
  let fname := argv[0]!
  let size := argv[1]!.toNat!
  println! "size {size}"
  let content <- IO.FS.readFile fname
  let [mapDesc, pathDesc] := content.splitOn "\n\n" 
      | println! "parse error"
  let map <- makeMap mapDesc size
  
  println! "rows {repr map.faces.toList}"
  
  let .some face0 := map.faces.find? 0 | println! "no face 0"
  let start := face0.pos
  let path := (pPath.run pathDesc).toOption.get!
  println! "start {start}"

  println! "-----"  
  
  -- Hold onto P1 stuff, just use data for the loop around. (drop rows/cols)
  let .some final := path.foldlM runPart1 {map, dir := 0, pos := start} | println! "P1 Failed"
  println! "end part1 {final.pos}"
  let (r,c) := final.pos
  let part1 := 1000 * (r+1) + 4 * (c+1) + final.dir
  println! "{fname} part1 {part1}"

  let .some final := path.foldlM runPart2 {map, dir := 0, pos := start} | println! "P2 Failed"
  println! "end part2 {final.pos}"
  let (r,c) := final.pos
  let part2 := 1000 * (r+1) + 4 * (c+1) + final.dir
  println! "{fname} part2 {part2}"  

#eval main ["day22/eg.txt", "4"]
#eval main ["day22/input.txt", "50"]
