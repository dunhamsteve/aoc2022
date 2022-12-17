import Lean

abbrev Pile := List UInt8
abbrev Rock := List UInt8

inductive Dir where
|  left : Dir
|  right : Dir
deriving Repr, Inhabited

structure State where
  winds : Array Dir
  pile : List UInt8
  wstep : Nat
  rstep : Nat
  drop : Nat
deriving Repr

abbrev M α := StateM State α  

def toDir (char : Char): Dir :=
  if char == '<' then .left else .right

def rocks : Array (List UInt8) := #[
  [ 0x0, 0x0, 0x0, 0x1e ],
  [ 0x0, 0x8, 0x1c, 0x8 ],
  [ 0x0, 0x4, 0x4, 0x1c ],
  [ 0x10, 0x10, 0x10, 0x10 ],
  [ 0x0, 0x0, 0x18, 0x18 ]
]

def shift (rock : Rock) : Dir -> Rock
| .left => if rock.all (· &&& 64 == 0) 
      then rock.map (· * 2) else rock
| .right => if rock.all (· &&& 1 == 0)
      then rock.map (· / 2) else rock

def fit (a b : UInt8) := a &&& b == 0

-- Can rock fit at top of pile?
def testRock : Rock -> Pile -> Bool
| [], _ => true
| x :: xs, y :: ys => x &&& y == 0  && testRock xs ys
| _, _ => false

def place : Rock -> Pile -> Pile
| [], ps => ps
| x :: xs, y :: ys => (x ||| y) :: place xs ys
| _,_ => unreachable! -- How should we handle this?

def prepare : Pile -> Pile
| [] => [0,0,0,0,0,0,0]
| 0 :: xs => prepare xs
| pile => (0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: pile) -- I think there's a notation for this?

def getWind : M Dir := do
  let st <- get
  -- can't figure out how to convince Lean x mod n < n, so !
  let rval := st.winds[st.wstep % st.winds.size]!
  set {st with wstep := st.wstep + 1}
  pure rval

def getRock : M Rock := do
  let st <- get
  let rval := rocks[st.rstep % rocks.size]!
  set {st with rstep := st.rstep + 1}
  pure rval

def doWind (rock : Rock) (pile : Pile) : M Rock := do
  let rock' := shift rock (<- getWind)
  if testRock rock' pile then pure rock' else pure rock

def dropRock : M Unit := do
  let rec loop : Rock -> Pile -> M Pile
  | rock, pile@(x :: rest) => do
      let rock <- doWind rock pile
      if testRock rock rest then (x :: · ) <$> loop rock rest
      else pure $ place rock pile
  | _,_ => unreachable!
  let pile <- loop (<-getRock) (prepare (<-get).pile)
  let st <- get
  set {st with pile }


partial
def same (as bs : List UInt8) (i : Nat) : Bool :=
  let rec loop : List UInt8 -> List UInt8 -> Nat -> Bool
  | [], [], _ => true
  | _, _, 0 => true
  | (0 :: as), bs, k => loop as bs k
  | as, (0 :: bs), k => loop as bs k
  | (a :: as), (b :: bs), k =>
    if a != b then false
    else loop as bs (k - 1)
  | _,_,_ => false
  loop as bs i


#eval same [0,30] [0,0] 1

def tetris (count : Nat) : M Unit := do
  for _ in [0:count] do
    dropRock
  pure ()

def lcm (n : Nat) (m : Nat) := n * m / (n.gcd m)

def getHeight (st : State) : Nat :=
  (st.pile.dropWhile (· == 0)).length

partial
def race (a : State) (b : State) : Nat :=
  let a := (dropRock.run a).2
  let b := (dropRock.run (dropRock.run b).2).2

  if a.rstep % rocks.size == b.rstep % rocks.size
    && a.wstep % a.winds.size == b.wstep % b.winds.size
    && same a.pile b.pile 4
  then
    -- First chunk is a little taller, each subsequent one is consistent
    let period := b.rstep - a.rstep
    let deltaH := getHeight b - getHeight a
    let todo := 1000000000000 - a.rstep
    let div := todo / period
    let mod := todo % period
    let (_,st) := (tetris mod).run a
    deltaH * div + getHeight st
  else race a b

def showRow (row : UInt8) : String :=
  ([64,32,16,8,4,2,1].map λ m => if m &&& row != 0 then '#' else '.').asString

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  -- array for modular lookup
  let winds := (content.trim.toList.map toDir).toArray
  let start : State := {winds, wstep := 0, rstep := 0, drop := 0, pile := []}
  
  let (_, st) := (tetris 2022).run start
  let part1 :=  getHeight st
  
  let part2 := race start start
  println! "part1 {part1} part2 {part2}"
  
#eval main ["day17/eg.txt"]
#eval main ["day17/input.txt"]
