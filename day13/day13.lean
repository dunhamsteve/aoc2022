import Lean
import Lean.Parser
open Lean.Parsec

inductive MyExpr where
| list : List MyExpr -> MyExpr
| val : Int -> MyExpr
deriving Repr, Inhabited, BEq

partial
def compareExpr : MyExpr -> MyExpr -> Ordering
| .val a, .val b => compare a b 
| .list (a :: as), .list (b :: bs) =>
      match compareExpr a b with
      | .lt => .lt
      | .gt => .gt
      | .eq => compareExpr (.list as) (.list bs)
| .val a, bs => compareExpr (.list [.val a]) bs
| as, .val b => compareExpr as (.list [.val b])
| .list [], .list [] => .eq
| .list [], _ => .lt
| _ , .list [] => .gt

open Lean

def pnum := do
  let digs <- many1 digit 
  pure digs.data.asString.toInt!

def someSep (p : Parsec α) (s : Parsec Unit) := do
  let a <- p
  let as <- many (s *> p)
  pure (a :: as.toList)

def manySep (p : Parsec α) (s : Parsec Unit) := do
  someSep p s <|> pure []

def plist (ptok : Parsec MyExpr): Parsec (List MyExpr) := do
  skipChar '['
  let stuff <- manySep ptok (skipChar ',')
  skipChar ']'
  pure stuff

partial
def ptok := (MyExpr.list <$> plist ptok ) <|> (MyExpr.val <$> pnum)
def pgroup := (· , · ) <$> ptok <* ws <*> ptok
def pall := many (ptok <* ws)

def main (args : List String) := do
  let fname := args.head!
  let content <- IO.FS.readFile fname
  let bits := content.splitOn "\n\n"
  let .ok blah := bits.mapM pgroup.run | return
  let mut result := 0
  for h : i in [:blah.length] do
    have : i < blah.length := h.2
    let (a,b) := blah[i]
    if compareExpr a b == Ordering.lt
      then 
        result := result + (i+1)
  println! "part1 {result}"

  let two : MyExpr := .list [(.list [(.val 2)])]
  let six : MyExpr := .list [(.list [(.val 6)])]
  let all := (pall.run content).toOption.get!
  let tosort := (all.push two).push six
  let tosort := tosort.qsort (compareExpr · · == Ordering.lt)
  let .some x := (tosort.indexOf? two) | return
  let .some y := (tosort.indexOf? six) | return
  let part2 : Nat := (1 + x) * (y+1)
  println! "part2 {part2}"
    
#eval main ["day13/eg.txt"]
#eval main ["day13/input.txt"]


