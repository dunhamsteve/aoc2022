import Lean
open Lean

abbrev Point := Int × Int × Int
abbrev PointMap := HashMap Point Nat

def pointFoo (f : Int -> Int -> Int) : Point -> Point -> Point
| (a,b,c),(x,y,z) => (f a x, f b y, f c z)

def parsePoint (line : String) : Point :=
match (line.splitOn ",").map String.toInt! with
| [a,b,c] => (a,b,c)
| _ => unreachable! 

-- I did a quick check and know they go from 0 to 19
-- We need slack space of one for the floodfilling of steam
def inbounds (pt : Point) : Bool :=
  let check : Int -> Bool
    | -2 => false
    | 21 => false
    | _ => true
  check pt.1 && check pt.2.1 && check pt.2.2

def Point.neighbors : Point -> List Point
| (a,b,c) => [
  (a+1,b,c), (a-1,b,c),
  (a,b+1,c), (a,b-1,c),
  (a,b,c+1), (a,b,c-1)
  ].filter inbounds

def count (pointMap : PointMap) (pt : Point) (default : Nat) : Nat :=
  let reducer := λ acc pt => acc + pointMap.findD pt default
  pt.neighbors.foldl reducer 0

partial
def fill (pm : PointMap) (todo : Array Point) : PointMap :=
  match todo.back? with
  | .none => pm
  | .some pt =>
    let todo := todo.pop
    let ns := pt.neighbors.filter (not ∘ pm.contains)
    let pm := ns.foldl (λ pm pt => pm.insert pt 1) pm
    let todo := todo.appendList ns
    fill pm todo

def main (args : List String) : IO Unit := do
  let fname := args[0]!
  let content <- IO.FS.readFile fname
  let points := (content.trim.splitOn "\n").map parsePoint
  
  let pointMap : PointMap := points.foldl (λ s x => s.insert x 0) .empty
  
  let part1 := points.foldl (λ a pt => a + count pointMap pt 1) 0
  
  let pointMap := fill pointMap #[(0,0,0)]
  let part2 := points.foldl (λ a pt => a + count pointMap pt 0) 0
  println! "{fname} p1 {part1} p2 {part2} {pointMap.size}"

#eval main ["day18/eg.txt"]
#eval main ["day18/input.txt"]
