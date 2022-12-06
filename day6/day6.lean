import Lean

def process (goal : Nat) (cs : List Char) :=
  let rec loop : List Char -> Nat -> Nat
  | [], _ => 0
  | (c :: cs), pos => 
    if ((c :: cs).take goal).eraseDups.length == goal
      then pos + goal else loop cs (pos + 1)
  loop cs 0

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let line := (content.splitOn "\n").head!
  let mark := process 4 line.toList
  let mark2 := process 14 line.toList
  println! "{mark} {mark2}"

#eval main [ "day6/eg.txt" ]
#eval main [ "day6/eg2.txt" ]
#eval main [ "day6/eg3.txt" ]
#eval main [ "day6/eg4.txt" ]
#eval main [ "day6/input.txt" ]

