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
  etime : Nat
  loc : String
  eloc : String
  closed : List String

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
  let rec loop : (List Node) -> Nat -> Nat -> Nat
  | [], _, _ => 0
  | (n :: ns), time, etime =>
    if st.closed.contains n.name && n.name != st.loc && n.name != st.eloc then
      if time >= etime then
        if time < 2 then 0 else n.rate * (time - 2) + loop ns (time - 2) etime
      else
        if etime < 2 then 0 else n.rate * (etime - 2) + loop ns time (etime - 2)
    else loop ns time etime

  let (a,time) := if st.closed.contains st.loc 
    then 
      let n := data.nodeMap.find! st.loc
      (n.rate * (st.time -1), st.time - 1)
    else (0,st.time)
  let (b,etime) := if st.closed.contains st.eloc
    then
      let n := data.nodeMap.find! st.eloc
      (n.rate * (st.etime - 1) , st.etime - 1)
    else (0,st.etime)

  let est := st.score + a + b + loop data.nodes time etime
  {st with est}

-- make this work with the changes, then give each agent 
-- separate times and only advance one at a time.

def openValve (data : Data) (st : State) : Option State :=
  if st.time > 0 && st.closed.contains st.loc then
    let n := data.nodeMap.find! st.loc
    let closed := st.closed.erase st.loc
    let time := st.time - 1
    let score := st.score + time * n.rate
    some $ { st with closed, time, score }
  else
    none

def moveTo (data : Data) (st : State) (loc : String) : Option State :=
  if st.time > 0
    then some $ { st with loc, time := st.time - 1}
    else none

def openEValve (data : Data) (st : State) : Option State :=
  if st.etime > 0 && st.closed.contains st.eloc then
    let n := data.nodeMap.find! st.eloc
    let closed := st.closed.erase st.eloc
    let etime := st.etime - 1
    let score := st.score + etime * n.rate
    some $ { st with closed, etime, score }
  else
    none

def moveETo (data : Data) (st : State) (eloc : String) : Option State :=
    if st.etime > 0
    then some $ { st with eloc, etime := st.etime - 1}
    else none

inductive Action where
| valve : Action
| move : String -> Action
deriving Repr


def perform (data : Data) (st : State) (me : Action) (ele : Action) : Option State := do
  -- dbg_trace "perform {repr me} {repr ele} {st.time} {st.etime}" .none;

  let st <- match me with
    | .valve => openValve data st
    | .move n => moveTo data st n
  let st <- match ele with
    | .valve => openEValve data st
    | .move n => moveETo data st n
  estimate data st

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
      -- else if st.closed.isEmpty || st.time < 1 then process data q (max best st.score)
      else
        let n := data.nodeMap.find! st.loc
        let n2 := data.nodeMap.find! st.eloc
        let myActions : List Action := .valve :: n.out.map λ n => .move n
        let eActions : List Action := .valve :: n2.out.map λ n => .move n

        let cand := (myActions.map (λ a => (eActions.filterMap λ b => perform data st a b))).join

        -- -- dbg_trace (repr ("cand",myActions, eActions, cand))
        -- -- let cand := []
        -- let me := n.out.filterMap λ name => moveTo data st name
        -- let me := match openValve data st with
        -- | some st => st :: me
        -- | none => me

        -- let ele := n2.out.filterMap λ name => moveETo data st name
        -- let ele := match openEValve data st with
        -- | some st => st :: ele
        -- | none => ele
        
        -- let cand := me.append ele

        let cand := cand.filter (λ c => State.est c > st.score)
        
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
  let start : State := { loc := "AA", eloc := "AA", time := 30, etime := 0, closed, score := 0, est := 0 }
  let result := process data (RBTree.insert .empty start) 0
  println! "{fname} part1 {result}"
  
  let start : State := { loc := "AA", eloc := "AA", time := 26, etime := 26, closed, score := 0, est := 0 }
  let result := process data (RBTree.insert .empty start) 0
  println! "{fname} part2 {result}"
#eval main ["day16/eg.txt"]
-- #eval main ["day16/input.txt"]

