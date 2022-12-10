import Lean

inductive Instr where
| noop : Instr
| skip : Instr
| addx : Int -> Instr
deriving Repr

instance : Inhabited Instr where 
  default := .noop

structure State where
  cycle : Int
  regx  : Int

instance : Inhabited State where
  default := { cycle := 1, regx := 1 }

abbrev M α := StateM State α

def parseLine (line : String) : Instr :=
match line.splitOn " " with
| [ "noop" ] => .noop
| [ "addx", v ] => .addx v.toInt!
| _ => unreachable!

def exec : State -> Instr -> State :=
fun st instr =>
  let st := {st with cycle := st.cycle + 1 }
  match instr with
    | .noop   => st
    | .skip   => st
    | .addx x => { st with regx := st.regx + x }

def pad : List Instr -> List Instr
| [] => []
| (.noop :: is) => .noop :: pad is
| (i :: is) => .skip :: i :: pad is

def step (ins : Instr) :  M Char := do
  let st <- get
  set (exec st ins)
  let beam := (st.cycle - 1) % 40
  let x := st.regx
  pure $ if beam >= x - 1 && beam <= x + 1 then '#' else '.'

partial
def display (cs : List Char) : IO Unit := do
  let row := cs.take 40
  println! row.asString
  match cs.drop 40 with
  | [] => pure ()
  | cs => display cs

def main(args: List String) : IO Unit := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let lines := (content.splitOn "\n").filter fun x => x.length > 0
  let instrs := pad (lines.map parseLine)

  let mut st : State := default
  let mut total := 0

  for ins in instrs do
    let cyc := st.cycle
    if cyc % 40 == 20 
    then 
      total := total + st.regx*cyc
      println! "{cyc}: regx={st.regx} {repr ins} {st.regx*cyc} {total}"
    st := (exec st ins)

  println! "{fname} part1: {total}"
  println! "{fname} part2:"
  let screen := ((instrs.mapM step).run default).fst
  display screen

#eval main ["day10/eg.txt"]
#eval main ["day10/eg2.txt"]
#eval main ["day10/input.txt"]
