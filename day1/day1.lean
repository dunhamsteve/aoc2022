import Lean
import Init.Data.Array


-- "imperative" part1
def part1i (input: List Int): Int := Id.run do
  let mut max : Int := 0
  let mut acc : Int := 0
  for n in input do
    acc := acc + n
    if n == 0 then do
      if max < acc then max := acc
      acc := 0
  return max

def collect (acc : Int) : List Int -> List Int
| (0 :: ns) => acc :: collect 0 ns
| (n :: ns) => collect (acc + n) ns
| [] => []

def main(args: List String) : IO Unit := do
  let input <- IO.FS.readFile args.head!
  let nums := List.map String.toInt! (input.splitOn "\n")
  let sorted := Array.qsort { data := collect 0 nums } fun a b => a > b
  IO.println <| repr <| part1i nums
  IO.println <| repr <| (sorted.data.take 3).foldl (fun a b => a + b) 0

#eval main [ "eg.txt" ]
#eval main [ "input.txt" ]
