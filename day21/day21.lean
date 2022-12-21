import Lean
open Lean (RBMap)

inductive Op where
| plus : Op
| minus : Op
| times : Op
| divide : Op
deriving Repr, Inhabited

inductive Term where
| binOp : Op -> Term -> Term -> Term
| val : Int -> Term
| var : String -> Term
deriving Repr, Inhabited

def parseOp : (s : String) -> Op
| "+" => .plus
| "-" => .minus
| "*" => .times
| "/" => .divide
| _   => unreachable!

def parseLine (s : String) : (String × Term) :=
  let parts := s.splitOn " "
  let key := parts[0]!.dropRight 1
  if parts.length == 4 then
    (key,.binOp (parseOp parts[2]!) (.var parts[1]!) (.var parts[3]!))
  else
    (key,.val parts[1]!.toInt!)

abbrev Data := RBMap String Term compare

abbrev M α := EStateM String Data α

-- too complicated because I was guessing part two required memoization
partial
def calculate (data : Data) (key : String) : Int :=
  match data.find? key with
  | .none => unreachable!
  | .some (.val v) => v
  | .some (.binOp op (.var a) (.var b)) =>
    let aVal := calculate data a
    let bVal := calculate data b
    let result := match op with
      | .plus => aVal + bVal
      | .minus => aVal - bVal
      | .times => aVal * bVal
      | .divide => aVal / bVal
    result
  | _ => unreachable!

partial
def simplify (data : Data) : Term -> Term
| .val v => .val v
| .var "humn" => .var "humn"
| .var x => simplify data (data.find! x)
| .binOp op a b =>
  let aSimp := simplify data a
  let bSimp := simplify data b
  match op, aSimp, bSimp with
  | .plus, .val a, .val b => .val (a + b)
  | .minus, .val a, .val b => .val (a - b)
  | .times, .val a, .val b => .val (a * b)
  | .divide, .val a, .val b => .val (a / b)
  | op, a, b => .binOp op a b

partial
def run (v : Int) : (t : Term) -> Int
| .var "humn" => v
| .binOp .divide a (.val x) => run (v * x) a
| .binOp .plus (.val x) a => run (v - x) a
| .binOp .plus a (.val x)  => run (v - x) a
| .binOp .times (.val x) a => run (v / x) a
| .binOp .times a (.val x) => run (v / x) a
| .binOp .minus a (.val x) => run (v + x) a
| .binOp .minus (.val x) a => run (x - v) a
| tm => dbg_trace "FIXME {repr tm}"; unreachable!

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let contents <- IO.FS.readFile fname
  let stuff := (contents.trim.splitOn "\n").map parseLine
  let data := RBMap.fromList stuff compare
  let part1 := calculate data "root"
  println! "{fname} part1 {part1}"

  let .some (.binOp _ a b) := data.find? "root" | println! "ERR: root didn't match"

  let x := simplify data a
  let y := simplify data b

  let .val v := y | println! "assumed y was a val"
  let result := run v x
  println! "{fname} part2 {result}"

#eval main ["day21/eg.txt"]
#eval main ["day21/input.txt"]