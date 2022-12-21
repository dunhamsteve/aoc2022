import Lean
open Lean (HashMap RBTree)

structure Node where
  name : String
  index : UInt64
  rate : UInt64
  out : UInt64
deriving Repr, Inhabited, Ord

def Node.isOut (n : Node) (ix : UInt64) := n.out &&& (1 <<< ix) != 0
def Node.mask (n : Node) : UInt64 := 1 <<< n.index

def String.getNum (part : String) : UInt64 :=
  let a := part.dropWhile (λ c => not c.isDigit)
  let b := a.takeWhile (λc => c.isDigit)
  b.toNat!.toUInt64

def parseFile (content : String) : Array Node :=
  let lines := (content.trim.splitOn "\n").map λ line => line.splitOn " "
  let names := (lines.map λ xs => xs[1]!).toArray
  let getIndex (s : String) :=
    let x : Option Nat := (names.indexOf? s); x.get!
  let mkOut (ns : List String) : UInt64 :=
    let foo := (ns.map getIndex).foldl (λ acc ix => acc ||| (1 <<< (ix))) 0
    foo.toUInt64
  let pNode (pts : List String) : Node := {
    name := pts[1]!,
    index := (getIndex pts[1]!).toUInt64,
    rate := pts[4]!.getNum,
    out := mkOut ((pts.drop 9).map λ s => s.takeWhile Char.isAlpha)
  }
  (lines.map pNode).toArray


-- Need this for an Ord on State.
def compareList [Ord α] : (List α) -> (List α) -> Ordering
| [], [] => .eq
| (a::as), (b ::bs) => match compare a b with
    | .eq => compareList as bs
    | x => x
| [], _ => .lt
| _, _ => .gt

structure State where
  est : UInt64 -- total estimated score
  score : UInt64
  time : UInt64
  etime : UInt64
  -- loc : Nat
  -- eloc : Nat
  closed : UInt64
  node : Node
  enode : Node

deriving Repr, Ord

def State.isClosed (st : State) (ix : UInt64) := 
  let m := 1 <<< ix
  st.closed &&& m == m

-- our priority queue
abbrev Queue := RBTree State compare

abbrev NodeMap := HashMap String Node

structure Data where
  nodes : List Node
  

def estimate (data : Data) (st : State) : State :=
  -- trying to get a upper bound for remaining score, that is as tight as possible
  -- add best score for opening valves in time remaining
  -- we'll assume we want to close the local valve and then the others in order
  -- of size (assuming each is just one jump away)
  let rec loop : (List Node) -> UInt64 -> UInt64 -> UInt64
  | [], _, _ => 0
  | (n :: ns), time, etime =>
    if st.isClosed n.index && n.index != st.node.index && n.index != st.enode.index then
      if time >= etime then
        if time < 2 then 0 else n.rate * (time - 2) + loop ns (time - 2) etime
      else
        if etime < 2 then 0 else n.rate * (etime - 2) + loop ns time (etime - 2)
    else loop ns time etime

  let (a,time) := if st.isClosed st.node.index 
    then 
      let n := st.node
      (n.rate * (st.time -1), st.time - 1)
    else (0,st.time)
  let (b,etime) := if st.isClosed st.enode.index
    then
      let n := st.enode
      (n.rate * (st.etime - 1) , st.etime - 1)
    else (0,st.etime)

  let est := st.score + a + b + loop data.nodes time etime
  {st with est}

-- make this work with the changes, then give each agent 
-- separate times and only advance one at a time.

def openValve (st : State) : Option State :=
  if st.time > 0 && st.isClosed st.node.index then
    let n := st.node
    let closed := st.closed ^^^ n.mask
    let time := st.time - 1
    let score := st.score + time * n.rate
    some $ { st with closed, time, score }
  else
    none

def moveTo (st : State) (node : Node) : Option State :=
  if st.time > 0
    then some $ { st with node, time := st.time - 1}
    else none

def openEValve (st : State) : Option State :=
  if st.etime > 0 && st.isClosed st.enode.index then
    let n := st.enode
    let closed := st.closed ^^^ n.mask
    let etime := st.etime - 1
    let score := st.score + etime * n.rate
    some $ { st with closed, etime, score }
  else
    none

def moveETo (st : State) (enode : Node) : Option State :=
    if st.etime > 0
    then some $ { st with enode, etime := st.etime - 1}
    else none

inductive Action where
| valve : Action
| move : Node -> Action
deriving Repr


def perform (data : Data) (st : State) (me : Action) (ele : Action) : Option State := do
  -- dbg_trace "perform {repr me} {repr ele} {st.time} {st.etime}" .none;

  let st <- match me with
    | .valve => openValve st
    | .move n => moveTo st n
  let st <- match ele with
    | .valve => openEValve st
    | .move n => moveETo st n
  estimate data st

partial
def process (data : Data) (queue : Queue) (best : UInt64) :=
  -- when we hit an end, we pass along the best choice until our
  -- estimates are lower than our best
  match queue.max with
   -- this shouldn't happen because we cut below if we've nothing to add
  | .none => dbg_trace "empty"; best
  | .some st =>
      let q := queue.erase st;
      if st.est <= best then best
      else
        let n := st.node
        let n2 := st.enode
        
        let getMoves (mask : UInt64): List Action :=
          (data.nodes.filter λ n => Node.mask n &&& mask != 0).map λ n => Action.move n

        let myActions : List Action := .valve :: getMoves n.out
        let eActions  : List Action := .valve :: getMoves n2.out
        let cand := (myActions.map (λ a => (eActions.filterMap λ b => perform data st a b))).join
        let cand := cand.filter (λ c => State.est c > st.score)
        if cand.isEmpty
          then process data q (max best st.score)
          else
            let q := cand.foldl (λ q c => RBTree.insert q c) q
            process data q (max best st.score)

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let nodes  := parseFile content
  let .some node := nodes.find? λ n => n.name == "AA" | println! "Can't find AA"
  let enode := node
  let data : Data := { nodes := (nodes.qsort (λ a b => a.rate > b.rate)).toList }
  let closed := nodes.foldl (λ acc n => if n.rate > 0 then acc ||| n.mask else acc) 0
  
  -- Can't do both p1 and p2 for now.

  -- let start : State := estimate data { time := 30, etime := 0, closed, score := 0, est := 0, node, enode }
  -- let result := process data (RBTree.insert .empty start) 0
  -- println! "{fname} part1 {result}"
  
  let start : State := estimate data { time := 26, etime := 26, closed, score := 0, est := 0, node, enode }
  let result := process data (RBTree.insert .empty start) 0
  println! "{fname} part2 {result}"
#eval main ["day16/eg.txt"]
-- #eval main ["day16/input.txt"]

