import Lean
import Init.Data.Char

def parse (line: String) := 
  match (line.splitOn " ").map (fun x => (x.get 0).toNat) with
    | [a,b] => (a-65, b-88)
    | _ => unreachable!

def part1 
  | (a , b) => b + 1 + 3 * ((4 + b - a) % 3)
  
def part2
  | (a , b) => let c := (a + (b+2)) % 3; 3 * b + c + 1

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let lines := (content.splitOn "\n").filter fun x => x.length > 0
  let data := lines.map parse
  let p1 := data.foldl (fun a b => a + part1 b) 0
  let p2 := data.foldl (fun a b => a + part2 b) 0
  println! "{fname} {p1} {p2}"

#eval main [ "day2/eg.txt" ]
#eval main [ "day2/input.txt" ]
