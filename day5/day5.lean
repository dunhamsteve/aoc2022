import Lean
import Lean.Data.PersistentArray

abbrev Tableau := Array (List Char)

abbrev M (α : Type) := EStateM String Tableau α

def pop (col : Nat) : M Char := do
  let arr <- get
  let (x::xs) := arr.get! col 
    | throw "no pop for you"
  set (arr.set! col xs)
  pure x

def push (col : Nat) (ch: Char) : M Unit := do
  let arr <- get
  let xs := arr.get! col
  set (arr.set! col (ch :: xs))

def handleMove (line : String) : M Unit := do
  let parts := line.splitOn " "
  let count := parts[1]!.toNat!
  let moveFrom := parts[3]!.toNat!
  let moveTo := parts[5]!.toNat!

  for _ in [:count] do
    let ch <- pop moveFrom
    push moveTo ch

def handleMove2 (line : String) : M Unit := do
  let parts := line.splitOn " "
  let count := parts[1]!.toNat!
  let moveFrom := parts[3]!.toNat!
  let moveTo := parts[5]!.toNat!

  for _ in [:count] do
    let ch <- pop moveFrom
    push 0 ch

  for _ in [:count] do
    let ch <- pop 0
    push moveTo ch

def buildTableau (ix : Nat) : List Char -> M Unit
| c :: cs => if ix % 4 == 1 && c != ' ' 
    then do
      push (1 + ix / 4) c
      buildTableau (ix + 1) cs
    else buildTableau (ix + 1) cs
| [] => pure ()

def reverseTableau : M Unit := do
  set ((<- get).map List.reverse)


def process (content : String) : M Unit := do
  let lines := (content.splitOn "\n")
  for line in lines do
    if line.startsWith "move" then handleMove line
    if line.contains '[' then buildTableau 0 line.toList
    if line.startsWith " 1" then reverseTableau

def process2 (content : String) : M Unit := do
  let lines := (content.splitOn "\n")
  for line in lines do
    if line.startsWith "move" then handleMove2 line
    if line.contains '[' then buildTableau 0 line.toList
    if line.startsWith " 1" then reverseTableau

def result : EStateM.Result String Tableau Unit -> String
| .ok _ st => (st.filterMap List.head?).toList.asString
| .error msg _ => msg

def main (args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let tableau := (process content).run (mkArray 10 [])
  let tableau2 := (process2 content).run (mkArray 10 [])
  println! "{fname} {result tableau} {result tableau2}"

#eval main ["day5/eg.txt"]
#eval main ["day5/input.txt"]