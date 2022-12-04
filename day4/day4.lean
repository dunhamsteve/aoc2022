import Lean

-- move this to a common file
-- get non-empty lines
def get_lines (s : String) :=
  (s.splitOn "\n").filter (fun n => n.length > 0)

structure Range where
  x : Int
  y : Int
  deriving Repr

instance : Inhabited Range where
  default := Range.mk 0 0

def parseRange (s : String) : Range :=
  match s.splitOn "-" with
  | [a,b]  => Range.mk a.toInt! b.toInt!
  | _ => unreachable!

def parse (s : String) :=
  match s.splitOn "," with
  | [a,b] => (parseRange a , parseRange b)
  | _ => unreachable!

def Range.includes (a b : Range) : Bool :=
  a.x <= b.x && a.y >= b.y

def Range.overlaps (a b : Range) : Bool :=
  max a.x b.x <= min a.y b.y 

def main (args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let data := (get_lines content).map parse
  let mut count := 0
  let mut count2 := 0
  for (a,b) in data do
    if a.includes b || b.includes a then count := count + 1
    if a.overlaps b then count2 := count2 + 1

  println! "{fname} {count} {count2}"

#eval main [ "eg.txt" ]
#eval main [ "input.txt" ]
