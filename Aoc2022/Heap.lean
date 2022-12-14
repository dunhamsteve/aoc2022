

structure Heap (α) (lt : α -> α -> Bool) where
  data : Array α
deriving Repr

private
def sink (a : Array α) (lt : α -> α -> Bool) (ix : Nat): Array α :=
  if ix == 0 then a else
  -- dunno how to show this
  have : ix/2 < ix := sorry
  if h : ix < a.size then
    have : ix/2 < a.size := Nat.lt_trans this h
    if lt a[ix/2] a[ix] then
      let x := a[ix]
      let a := (a.setD ix a[ix/2]).setD (ix/2) x
      sink a lt (ix/2)
    else a
  else a

def Heap.insert (h : Heap α lt) (a : α) : Heap α lt where
  -- this is subtle and I had to look at heapSort.lean
  -- having a h.data.size at the end held onto the h durning the h.data.push
  data := let size := h.data.size; sink (h.data.push a) lt size

def Heap.max (h : Heap α lt) : Option α := h.data[0]?

-- TODO terminates because ix is increasing and less than arr.size, but 
-- I don't know enough lean to show that.
private
partial
def bubble (arr : Array α) (lt : α -> α -> Bool) (ix : Nat) : Array α :=
  if h : ix < arr.size then
    let j := 2 * ix
    let k := 2 * ix + 1
    let a := arr[ix]
    if h : k < arr.size then
      -- is there a better way to write this?
      have : j < arr.size := Nat.lt_trans (Nat.lt_succ_self (2*ix)) h
      let b := arr[j]
      let c := arr[k]
      if lt b c && lt a c then
        -- bubble k side
        bubble ((arr.setD k a).setD ix c) lt k
      else if lt a b then
        bubble ((arr.setD j a).setD ix b) lt j
      else
        arr
    else if h : j < arr.size then
      let b := arr[j]
      if lt a b then
          bubble ((arr.setD j a).setD ix b) lt j
      else arr
    else arr
  else arr
-- termination_by _ arr ix => arr.size - ix

-- TODO sorry
private
def popAux (arr : Array α) (lt : α -> α -> Bool) : Array α :=
  if h : arr.size > 1 then
    have : arr.size - 1 < arr.size := sorry
    bubble (arr.set! 0 arr[arr.size-1]).pop lt 0
  else if arr.size == 1 then arr.pop
  else arr

def Heap.pop (h : Heap α lt) : Heap α lt where
  data := popAux h.data lt

def makeHeap (lt : α -> α -> Bool) : Heap α lt where
  data := #[]

def Heap.empty {lt : α -> α -> Bool} : Heap α lt where
  data := #[]

private
def ltNat (a b : Nat) : Bool := a < b

private
partial
def main (argv : List String) : IO Unit := do
  let x := [1,234,1,35,6,8,0,10,11,2,3,8,6,4,3,0,1,0,1,0,1]
  let h := makeHeap ltNat
  let h := x.foldl (Heap.insert) h
  println! "{h.data}"
  
  let rec loop (h : _) : IO Unit :=
    match h.max with
    | .none => pure ()
    | .some x => do
        println! "{h.data}"
        println! "{x}"
        loop h.pop
  loop h

#eval main []
