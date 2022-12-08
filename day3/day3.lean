import Lean
import Init.Data.Nat

def get_lines (s : String) :=
  (s.splitOn "\n").filter (fun n => n.length > 0)

def bit (c : Char) :=
  let n := c.toNat
  let s := if n > 96 then n - 97 else n - 65 + 26
  Nat.shiftLeft 1 s

def bits (s : String) :=
  s.foldl (fun a c => a.lor (bit c)) 0

-- not sure how to make this total, but n _is_ getting smaller
partial
def zeros (n : Nat) :=
  if n == 0 then 0
  else if n % 2 == 1 then 0
  else 1 + zeros (n >>> 1)

def step1 (s : String) :=
  let m := s.length / 2;
  let a := bits (s.take m);
  let b := bits (s.drop m);
  1 + zeros (a.land b)

def part2 : Int -> List String -> Int
  | acc, (a :: b :: c :: ss) =>
      let score := 1 + zeros ((bits a).land ((bits b).land (bits c)));
      part2 (acc + score) ss
  | acc, [] => acc
  | _, _ => unreachable!

def main(args : List String) : IO Unit := do
  let fname := args.head!
  let input <- IO.FS.readFile fname
  let lines := get_lines input
  let one := (lines.map step1).foldl (fun a x => a + x) 0
  let two := part2 0 lines
  println! "{fname} {one} {two}"
  
#eval main [ "day3/eg.txt" ]
#eval main [ "day3/input.txt" ]


