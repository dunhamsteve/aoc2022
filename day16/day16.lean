import Lean
open Lean (HashMap RBTree RBMap)

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

-- Need this for an Ord on State.
def compareList [Ord α] : (List α) -> (List α) -> Ordering
| [], [] => .eq
| (a::as), (b ::bs) => match compare a b with
    | .eq => compareList as bs
    | x => x
| [], _ => .lt
| _, _ => .gt

instance [Ord α] : Ord (List α) where
  compare a b := compareList a b

structure State where
  est : Nat -- total estimated score
  score : Nat
  time : Nat
  closed : List String
  loc : String
deriving Repr, Ord

-- our priority queue
abbrev Queue := RBTree State compare

abbrev NodeMap := HashMap String Node

structure Data where
  nodes : List Node
  nodeMap : NodeMap

def estimate (data : Data) (st : State) : State :=
  -- trying to get a upper bound for remaining score, that is as tight as possible
  -- add best score for opening valves in time remaining
  -- we'll assume we want to close the local valve and then the others in order
  -- of size (assuming each is just one jump away)
  let rec loop : (List Node) -> Nat -> Nat
  | [], _ => 0
  | (n :: ns), time =>
    if time < 2 then 0
    else if st.closed.contains n.name && n.name != st.loc then
      n.rate * (time - 2) + loop ns (time - 2)
    else loop ns time
  
  let est := if st.closed.contains st.loc 
    then 
      let n := data.nodeMap.find! st.loc
      st.score + n.rate * (st.time - 1) + loop data.nodes (st.time - 1)
    else st.score + loop data.nodes st.time

  {st with est }

-- make this work with the changes, then give each agent 
-- separate times and only advance one at a time.

def openValve (data : Data) (st : State) : State :=
  let n := data.nodeMap.find! st.loc
  let closed := st.closed.erase st.loc
  let time := st.time - 1
  let score := st.score + time * n.rate
  estimate data { st with closed, time, score }

def moveTo (data : Data) (st : State) (loc : String) : State :=
  estimate data { st with loc, time := st.time - 1}
  
partial
def process (data : Data) (queue : Queue) (best : Nat) :=
  -- when we hit an end, we pass along the best choice until our
  -- estimates are lower than our best
  match queue.max with
   -- this shouldn't happen because we cut below if we've nothing to add
  | .none => dbg_trace "empty"; best
  | .some st =>
      let q := queue.erase st;
      if st.est < best then dbg_trace (repr (st.est, best));best
      else if st.closed.isEmpty || st.time < 1 
        then process data q (max best st.score)
      else 
        let n := data.nodeMap.find! st.loc
        let tmp := n.out.map λ name => (moveTo data st name)
        let cand := tmp.filter (λ c => State.est c > st.score)
        let cand := if st.closed.contains st.loc then
          openValve data st :: cand
          else cand
        if cand.isEmpty
          then process data q (max best st.score)
          else
            let q := cand.foldl (λ q c => RBTree.insert q c) q
            process data q (max best st.score)

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let nodes  := ((content.trim.splitOn "\n").map parseLine).toArray.qsort (λ n m => n.rate > m.rate)
  let nodeMap := nodes.foldl (λ m n => m.insert n.name n) .empty
  let data : Data := { nodeMap, nodes := nodes.toList }
  let closed := (nodes.filter (λn=>n.rate > 0)).toList.map λ n => n.name
  println! closed
  let start : State := { loc := "AA", closed, time := 30, score := 0, est := 0 }
  let result := process data (RBTree.insert .empty start) 0
  println! "{fname} part1 {result}"

#eval main ["day16/eg.txt"]
#eval main ["day16/input.txt"]

