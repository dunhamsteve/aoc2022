import Lean

def decode (s : String) : Int := Id.run do
  let mut a : Int := 0
  for c in s.toList do
    match c with
    | '2' => a := a * 5 + 2
    | '1' => a := a * 5 + 1
    | '0' => a := a * 5
    | '-' => a := a * 5 - 1
    | '=' => a := a * 5 - 2
    | _ => unreachable!
  a

def Int.abs (n : Int) := if n < 0 then -n else n

def encode (num : Int) : IO String := do
  let mut n := num
  let mut i := 0
  let mut a := 1
  
  -- figure out how many digits we need
  while a < num do
    a := a * 5
    i := i + 1
  
  let mut chars : Array Char := .empty

  a := a / 5
  i := i - 1

  for _ in [0:i+1] do
    -- need to get within 2.5*a / 5 of 0
    if 2*(n - a) >= a then
      chars := chars.push '2'
      n := n - 2 * a
    else if 2 * (n - a) >= - a then
      chars := chars.push '1'
      n := n - a
    else if 2 * n >= - a then
      chars := chars.push '0'
    else if 2*(n + a) >= -a then
      chars := chars.push '-'
      n := n + a
    else if 2*(n + 2*a) >= -a then
      chars := chars.push '='
      n := n + 2*a
    else
      println! "fixme {n} {a}  {2*a} {2*a/5} pl {n + a}"
      break
    a := a / 5
    i := i - 1
  println! "remainder {a} {n}"  
  pure chars.toList.asString

def main (argv : List String) : IO Unit := do
  let fname := argv[0]!
  let content <- IO.FS.readFile fname
  let lines := content.trim.splitOn "\n"
  let nums := lines.map decode
  let total := nums.foldl (·+ ·) 0
  println! nums
  
  println! "total {total}"
  let x <- encode total
  
  let y := decode x
  println! "got {x} {y}"

#eval main ["day25/eg.txt"]
#eval main ["day25/input.txt"]
