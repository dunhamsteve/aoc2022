import Lean
open Lean (HashMap RBTree RBMap)

-- probably need to skip empty ones to make the graph better
-- otherwise we start having a lot of extra branching..
-- or just distance worked in better

-- sensor / beacon points

structure Node where
  name : String
  rate : Nat
  out : List String
deriving Repr, Inhabited

def String.getNum (part : String) : Nat :=
  let a := part.dropWhile (λ c => not c.isDigit)
  let b := a.takeWhile (λc => c.isDigit)
  b.toNat!

def parseLine (line : String) : Node :=
  let parts := line.splitOn " "
  let name := parts[1]!
  let rate := parts[4]!.getNum
  let out := (parts.drop 9).map λ s => s.takeWhile Char.isAlpha
  {name,rate,out}

-- Potential for a state
-- assume adjacency, take in order, stop when time runs out.
-- e.g. I have 5 minutes
-- move, on, move, on, move
-- n-2*biggest, n-4*next biggest

-- at state x
-- score is Z
-- avail is A B C
-- remaining time is x

-- I need an ordering for these nodes, 
structure State where
  path : List String
  time : Nat
  score : Nat
  est : Nat -- total estimated score
  -- Maybe keep path
deriving Repr

def State.cmp (a : State) (b : State) : Ordering :=
  match compare a.est b.est with
  | .eq => compare (String.join a.path) (String.join b.path)
  | x => x

-- our priority queue
abbrev Queue := RBTree State State.cmp




abbrev NodeMap := HashMap String Node

structure Data where
  nodes : List Node
  nodeMap : NodeMap


def estimate (data : Data) (st : State) (name : String): State :=
  let node := data.nodeMap.find! name
  let path := name :: st.path
  let rate := if st.path.contains name then 0 else node.rate
  let time := if rate > 0 then st.time - 2 else st.time - 1
  let score := time * rate + st.score
  let rec loop : (List Node) -> Nat -> Nat
  | [], _ => 0
  | (n :: ns), time => 
    if path.contains n.name then loop ns time
    else n.rate * (time - 2) + loop ns (time - 2)
  let est := score + loop data.nodes time
  { path, score, time, est }

partial
def process (data : Data) (queue : Queue) :=
  match queue.max with
  | .none => 0
  | .some st =>
    -- dbg_trace "Process state {repr st}"
    let q := queue.erase st
    if st.time == 0 then st.score
    else
      let n := data.nodeMap.find! st.path.head!
      let q := n.out.foldl (λ q name => RBTree.insert q (estimate data st name)) q
      process data q
    
  
def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let nodes  := ((content.trim.splitOn "\n").map parseLine).toArray.qsort (λ n m => n.rate > m.rate)
  let nodeMap := nodes.foldl (λ m n => m.insert n.name n) .empty
  let data : Data := { nodeMap, nodes := nodes.toList }
  IO.println (repr nodes)

  let start : State := { path := ["AA"], time := 30, score := 0, est := 0 }
  let result := process data (RBTree.insert .empty start)
  println! "part1 {result}"
  

  -- oof thought I might have to impl SortedMap, which is a big lift
  -- so we need to find the best solution.  We can measure what we have
  -- and the best remaining score.

  


-- this is getting the wrong answer (one short) for the first, but not the second?
#eval main ["day16/eg.txt"]
#eval main ["day16/input.txt"]

