import Lean
open Lean

structure Monkey where
  -- This will be a snoc list and we reverse before processing
  items : List Nat
  op :  Bool × Option Nat × Option Nat
  test: Nat
  trueDest : Nat
  falseDest : Nat
  count: Nat
deriving Repr

instance : Inhabited Monkey where
  default := { items := [], op := (false, .none, .none), trueDest := 0, falseDest := 0, count := 0, test := 0 }

def parseMonkey (content : String) := Id.run do
  let lines := content.splitOn "\n"
  let mut m : Monkey := default
  for line in lines do
    let parts := line.splitOn " "
    match parts with
    | [ "Monkey", _] => pure ()
    | [ "", "" , "Operation:", "new", "=", a, b, c] => m := { m with op := (b == "*", a.toNat?, c.toNat?)}
    | ( "" :: "" :: "Starting" :: "items:" :: rest) => m := { m with items := (rest.map String.toNat!).reverse }
    | [ "", "", "Test:" , "divisible", "by", n] => m := { m with test := n.toNat! }
    | [ "", "", "", "", "If", "true:", "throw", "to", "monkey", n] => m := { m with trueDest := n.toNat! }
    | [ "","", "", "", "If", "false:", "throw", "to", "monkey", n] => m := { m with falseDest := n.toNat! }
    | [""] => ()
    | _ => dbg_trace "Parse Error: {parts}"; unreachable!
  m

def parseFile (content : String) :=
  ((content.splitOn "\n\n").map parseMonkey).toArray

abbrev M α := StateM (Array Monkey) α

def push (ix : Nat) (item : Nat) : M Unit := do
  let ms <- get
  set $ ms.modify ix (fun m => { m with items := item :: m.items })

def incr (ix : Nat) : M Unit := do
  let ms <- get
  set $ ms.modify ix fun m => { m with count := m.count + 1 }

def route (adjust : Nat -> Nat) (monkey : Monkey) (item : Nat) :=
  let val v := v.getD item
  let item := adjust $ match monkey.op with
  | (true, a, b) => val a * val b
  | (false, a, b) => val a + val b
  let dest := if item % monkey.test == 0 then monkey.trueDest else monkey.falseDest
  push dest item

def stepMonkey (adjust : Nat -> Nat) (ix : Nat) : M Unit := do
  let ms <- get
  let m := ms[ix]!
  let items := m.items.reverse
  set $ ms.modify ix fun m => { m with items := [] }
  for item in items do
    incr ix
    route adjust m item

def round (adjust : Nat -> Nat) (count : Nat) : M Unit := do
  let len := (<-get).size
  for _ in [0:count] do
    for ix in [0:len] do
      stepMonkey adjust ix

def lcm (n : Nat) (m : Nat) := n * m / (n.gcd m)

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let content := (content.toList.filter fun c => c != ',').asString
  let monkeys := parseFile content

  let (_, ms) := (round (fun n => n / 3) 20).run monkeys
  let p1counts := (ms.map (fun m => m.count)).qsort (fun a b => b < a)
  let part1 := p1counts[0]! * p1counts[1]!

  let modulus := (ms.foldl (fun a m => lcm a m.test) 1)
  println! "mod {modulus}"

  let (_, ms) := (round (fun n => n % modulus) 10000).run monkeys
  let p2counts := (ms.map (fun m => m.count)).qsort (fun a b => b < a)
  let part2 := p2counts[0]! * p2counts[1]!

  println! "{fname} part1 {part1} part2 {part2}"

#eval main ["day11/eg.txt"]
#eval main ["day11/input.txt"]





