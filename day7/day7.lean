import Lean
import Init.Data.Array
import Lean.Data.PersistentHashMap

open Lean (PersistentHashMap)

def process : List String -> List (List String) -> List (List String ×  Int )
| _, [] => []
| _, (["$", "cd", "/"] :: cs) => process [] cs
| (_ :: ds), (["$", "cd", ".."] :: cs) => process ds cs
| cwd, (["$", "cd", dir] :: cs) => process (dir :: cwd) cs
| cwd, (["$", "ls"] :: cs) => process cwd cs
| cwd, (["dir", _] :: cs) => process cwd cs
| cwd, ([size, name] :: cs) => ((name :: cwd), size.toInt!) :: process cwd cs
| _, _ => []

abbrev DTree := PersistentHashMap (List String) Int

def update (tree : DTree) (size : Int) : (stuff : List String) -> DTree
| [] => tree
| (_ :: ns) => update (tree.insert ns (tree.findD ns 0 + size)) size ns
  
def main (args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let lines := (content.splitOn "\n").map (fun line => line.splitOn " ")
  let pairs := process [] lines
  let tree := pairs.foldl (fun tree (a,b) => update tree b a) .empty

  let dirs := tree.toList.filter (fun (_,size) => size <= 100000)
  let result := dirs.foldl (fun a (_,size) => a + size) 0

  println! "part1 {result}"
  let sorted := Array.qsort {data := tree.toList}  (fun (_,a) (_,b) => a < b)
  let (_,used) := sorted.back
  let need :=  used - 70000000 + 30000000
  println! "used {used} need {need}"
  for (path,sz) in sorted do
    if sz > need then do
      println! "part2 {path} {sz}"
      break

#eval main ["day7/eg.txt"]
#eval main ["day7/input.txt"]
