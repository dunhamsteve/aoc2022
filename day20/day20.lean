import Lean

abbrev Node := (Nat × Nat × Int)

-- Putting a linked list in an Array, hoping Lean doesn't copy it.

partial
def seek (linkedNums : Array Node) (pos : Nat) (delta : Int) : Nat :=
  let rec loop : Nat -> Nat -> Nat
  | 0, pos => pos
  | k, pos =>
      let node := linkedNums[pos]!
      let next := node.2.1
      loop (k - 1) next
  loop delta.toNat pos

def remove (linkedNums : Array Node) (ix : Nat) :=
  let (l,r,_) := linkedNums[ix]!
  let (a,_,c) := linkedNums[l]!
  let (_,e,f) := linkedNums[r]!
  let linkedNums := linkedNums.setD l (a,r,c)
  linkedNums.setD r (l,e,f)

def insert (linkedNums : Array Node) (dest : Nat) (src : Nat) :=
  let (a,b,c) := linkedNums[dest]!
  let (_,_,f) := linkedNums[src]!
  let (_,h,i) := linkedNums[b]!
  let linkedNums := linkedNums.setD dest (a,src,c)
  let linkedNums := linkedNums.setD src (dest,b,f)
  linkedNums.setD b (src,h,i)

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let nums := ((content.trim.splitOn "\n").map String.toInt!).toArray
  let n := nums.size
  let some head := nums.indexOf? 0 | println! "no zero?"

  -- Part 1
  let mut linkedNums := (nums.mapIdx λ a b => ((a + n - 1) % n, (a+1) % n, b))
  for h : src in [0:nums.size] do
    let t := nums[src]'h.2
    if t == 0 then continue
    let (prev,_,_) := linkedNums[src]!
    linkedNums := remove linkedNums src
    let t := t % (n - 1)
    -- % returns negative numbers in lean
    let t := (t + n - 1) % (n - 1)
    let dest := seek linkedNums prev t
    
    linkedNums := insert linkedNums dest src

  let x := seek linkedNums head 1000
  let y := seek linkedNums head 2000
  let z := seek linkedNums head 3000

  let a := linkedNums[x]!.2.2
  let b := linkedNums[y]!.2.2
  let c := linkedNums[z]!.2.2
  println! "{fname} part1 {a + b + c}"

  -- Part 2
  let nums := nums.map (· * 811589153)
  linkedNums := (nums.mapIdx λ a b => ((a + n - 1) % n, (a+1) % n, b))
  
  for _ in [0:10] do
    for h : src in [0:nums.size] do
      let t := nums[src]'h.2
      if t == 0 then continue
      let (prev,_,_) := linkedNums[src]!
      linkedNums := remove linkedNums src

      let t := t % (n - 1)
      -- % returns negative numbers in lean
      let t := (t + n - 1) % (n - 1)
      let dest := seek linkedNums prev t
      linkedNums := insert linkedNums dest src

  let x := seek linkedNums head 1000
  let y := seek linkedNums head 2000
  let z := seek linkedNums head 3000

  let a := linkedNums[x]!.2.2
  let b := linkedNums[y]!.2.2
  let c := linkedNums[z]!.2.2

  let part1 := a + b + c
  println! "{fname} part2 {part1}"

#eval main ["day20/eg.txt"]
-- #eval main ["day20/input.txt"]
