import Lean

open Lean (RBTree)

abbrev Plan := Array (Array Nat)

def getNums (line : String) : Option Plan :=
  let nums := ((line.split (not ∘ Char.isDigit)).filterMap String.toNat?).toArray
  -- TODO is there an easy to get indexing to work
  if h : nums.size > 6 then
    #[
      #[ nums[1]! ],
      #[ nums[2]! ],
      #[ nums[3]!, nums[4]! ],
      #[ nums[5]!, 0, nums[6] ]
    ]
  else none

structure State where
  est : Nat
  time : Nat
  robots : Array Nat
  resources : Array Nat
deriving Repr

def estimate (plan : Plan) (st : State) := Id.run do
  let time := st.time

  -- build one per tick?
  let mut resources := st.resources
  let mut robots := st.robots

  let t := resources[0]!
  -- letting them have their own ore
  let mut ore := #[t,t,t,t]

  for _ in [0:time] do
    -- fire everything that can, ignoring ore
    let oldBots := robots
    for i in [0:plan.size] do
      -- let i := plan.size - i - 1
      let thisPlan := plan[i]!
      -- Fire best if we have enough resources, but consume them
      let fire := resources[i-1]! >= thisPlan[i-1]! && ore[i]! >= thisPlan[0]!
      if fire then
        robots := robots.setD i (robots[i]! + 1)
        if i > 0 then
          resources := resources.setD (i-1) (resources[i-1]! - thisPlan[i-1]!)
        ore := ore.setD i (ore[i]! - thisPlan[0]!)
      ore := ore.setD i (ore[i]! + oldBots[0]!)

    -- old robots generate resources
    for h : i in [0:oldBots.size] do
      resources := resources.setD i (resources[i]! + oldBots[i]'h.2)

    -- build an ore bot too
    if resources[0]! >= plan[0]![0]! then
      robots := robots.setD 0 (robots[0]! + 1)

  let est := resources[3]!
  {st with est}

def build (st : State) (plan : Plan) (ix : Fin α) (req : Array Nat) := Id.run do
  -- check before making new objects
  if st.time == 0 then return none
  let mut resources := st.resources
  for h : i in [0:req.size] do
    if resources[i]! < req[i]'h.2 then return none

  -- cost for building
  for h : i in [0:req.size] do
    resources := resources.set! i (resources[i]! - req[i]'h.2)

  -- generate
  for h : i in [0:st.robots.size] do
    resources := resources.setD i (resources[i]! + st.robots[i]'h.2)

  -- add new robot
  let robots := st.robots.set! ix (st.robots[ix]! + 1)
  some $ estimate plan {st with resources, robots, time := st.time - 1 }

def skip (st : State) (plan : Plan) := Id.run do
  let mut resources := st.resources
  -- Generate resources
  for h : i in [0:st.robots.size] do
    resources := resources.setD i (resources[i]! + st.robots[i]'h.2)
  estimate plan { st with resources, time := st.time - 1 }

def compareState (a b : State) : Ordering := Id.run do
  if a.est != b.est then return compare a.est b.est
  if a.time != b.time then return compare a.time b.time
  for i in [0:4] do
    let x := compare a.resources[i]! b.resources[i]!
    if x != .eq then return x
  for i in [0:4] do
    let x := compare a.robots[i]! b.robots[i]!
    if x != .eq then return x
  .eq

partial
def run  (time : Nat) (plan : Plan) : Nat :=
  let rec loop (queue : RBTree State compareState) (best : Nat) : Nat :=
    match queue.max with
    | none => best
    | some st => Id.run do
        let mut queue := queue.erase st
        if st.est <= best then
          loop queue (max best st.resources[3]!)
        else if st.time == 0 then loop queue (max best st.resources[3]!) else

        let next := (plan.mapIdx (build st plan)).filterMap id
        let next := next.filter (λ st => st.est >= best)
        queue := next.foldl RBTree.insert queue
        queue := queue.insert (skip st plan)
        loop queue best
  let start := { est := 0, time, robots := #[1,0,0,0], resources := #[0,0,0,0] }
  loop ((RBTree.empty).insert (estimate plan start)) 0

def main (argv: List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let lines := content.trim.splitOn "\n"
  let plans := lines.filterMap getNums
  
  let values := plans.map (run 24)
  let total := (values.toArray.mapIdx λ a b => (a + 1) * b).foldl (· + · ) 0
  println! "part1 {total}"

  let values := (plans.take 3).map (run 32)
  let total := values.toArray.foldl (· * · ) 1
  println! "part2 {total}"

-- #eval main ["day19/eg.txt","false"]
