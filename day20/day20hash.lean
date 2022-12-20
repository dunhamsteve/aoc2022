import Lean
open Lean (HashMap)

-- Experiment to see if HashMap is faster.
-- 24.3 sec instead of 10 sec
structure Node where
  left : Nat
  right : Nat
  val : Int
deriving Inhabited

abbrev LList := HashMap Nat Node

partial
def seek (linkedNums : LList) (pos : Nat) (delta : Int) : Nat :=
  let rec loop : Nat -> Nat -> Nat
  | 0, pos=> pos
  | k, pos =>
      let node := linkedNums.find! pos
      let next := node.right
      loop (k - 1) next
  loop delta.toNat pos

def remove (linkedNums : LList) (ix : Nat) :=
  let node := linkedNums.find! ix
  let before := linkedNums.find! node.left
  let after := linkedNums.find! node.right
  let linkedNums := linkedNums.insert node.left { before with right := node.right }
  linkedNums.insert node.right {after with left := node.left }

def insert (linkedNums : LList) (dest : Nat) (src : Nat) :=
  let before := linkedNums.find! dest
  let node := linkedNums.find! src
  let after := linkedNums.find! before.right
  let linkedNums := linkedNums.insert dest { before with right := src }
  let linkedNums := linkedNums.insert src { node with left := dest, right := before.right }
  linkedNums.insert before.right { after with left := src }
  

def makeList (nums : Array Int) : HashMap Nat Node :=
  let n := nums.size
  let pairs : Array (Nat × Node) := 
    nums.mapIdx λ ix v => (ix, (Node.mk ((ix + n - 1) % n) ((ix+1) % n) v))
  HashMap.ofList pairs.toList
  

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let nums := ((content.trim.splitOn "\n").map String.toInt!).toArray
  let n := nums.size
  let some head := nums.indexOf? 0 | println! "no zero?"


  -- Part 1
  
  let mut linkedNums := makeList nums
  for h : src in [0:nums.size] do
    let t := nums[src]'h.2
    if t == 0 then continue
    let node := linkedNums.find! src
    linkedNums := remove linkedNums src
    let t := t % (n - 1)
    -- % returns negative numbers in lean
    let t := (t + n - 1) % (n - 1)
    let dest := seek linkedNums node.left t
    linkedNums := insert linkedNums dest src
  
  let x := seek linkedNums head 1000
  let y := seek linkedNums head 2000
  let z := seek linkedNums head 3000

  let a := linkedNums.find! x
  let b := linkedNums.find! y
  let c := linkedNums.find! z
  println! "{fname} part1 {a.val + b.val + c.val}"

  -- Part 2
  let nums := nums.map (· * 811589153)
  linkedNums := makeList nums
  
  for _ in [0:10] do
    for h : src in [0:nums.size] do
      let t := nums[src]'h.2
      if t == 0 then continue
      let node := linkedNums.find! src
      linkedNums := remove linkedNums src
      let t := t % (n - 1)
      -- % returns negative numbers in lean
      let t := (t + n - 1) % (n - 1)
      let dest := seek linkedNums node.left t
      linkedNums := insert linkedNums dest src

  let x := seek linkedNums head 1000
  let y := seek linkedNums head 2000
  let z := seek linkedNums head 3000

  let a := linkedNums.find! x
  let b := linkedNums.find! y
  let c := linkedNums.find! z

  let part1 := a.val + b.val + c.val
  println! "{fname} part2 {part1}"

#eval main ["day20/eg.txt"]
-- #eval main ["day20/input.txt"]
