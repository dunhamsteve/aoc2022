/-
This file contains an exercise, implement a max heap in lean. It uses a zero-based
array, which makes some of the math (and proofs) trickier. But I think I've managed
to get it right and prove termination + indices.

TODO 
  - How hard is it to prove that it maintains the heap property?
-/

structure Heap (α) (lt : α -> α -> Bool) where
  data : Array α
deriving Repr

private
def sink (a : Array α) (lt : α -> α -> Bool) (ix : Nat) : Array α :=
  if h : 0 < ix then
    let ix' := (ix - 1)/2
    have : ix' < ix := by
      cases h1 : ix - 1
      . apply Nat.lt_of_le_of_lt 
        apply Nat.div_le_self
        rw [h1]
        assumption
      . apply Nat.lt_of_lt_of_le 
        apply Nat.div_lt_self
        rw [h1]
        apply Nat.zero_lt_succ
        apply Nat.lt_succ_self
        apply Nat.sub_le
    if h : ix < a.size then
      have : ix' < a.size := Nat.lt_trans this h
      if lt a[ix'] a[ix] then
        let x := a[ix]
        let a := (a.set ⟨ ix, h ⟩  a[ix']).set ⟨ ix', by simp [this] ⟩ x
        sink a lt ix'
      -- is there an elegant way to not have all of these elses?
      -- I couldn't and the if's up there because I needed dependent if
      else a
    else a
  else a
  
def Heap.insert (h : Heap α lt) (a : α) : Heap α lt where
  -- this is subtle and I had to look at heapSort.lean
  -- having a h.data.size at the end held onto the h durning the h.data.push
  data := let size := h.data.size; sink (h.data.push a) lt size

def Heap.max (h : Heap α lt) : Option α := h.data[0]?

private
def bubble (arr : Array α) (lt : α -> α -> Bool) (ix : Nat) : Array α :=  
  let rec go (arr : Array α) (ix : Nat) := 
    -- We can probably pass this constraint in.
    if h1 : ix < arr.size then
      have ha : arr.size - 2*ix <= arr.size - ix := by 
              simp [Nat.sub_add_eq,Nat.sub_le,Nat.succ_mul]
              
      let j := 2 * ix + 1
      let k := 2 * ix + 2
      let a := arr[ix]
      if h : k < arr.size then
        -- is there a better way to write this?
        have h2 : j < arr.size := Nat.lt_trans (2*ix + 1).lt_succ_self h
        have h3 : 2*ix < arr.size := Nat.lt_trans (2*ix).lt_succ_self h2
        let b := arr[j]
        let c := arr[k]
        if lt b c && lt a c then -- bubble k side          
          have : arr.size - k < arr.size - ix := by 
            apply Nat.lt_trans
            apply Nat.sub_succ_lt_self
            assumption
            apply Nat.lt_of_lt_of_le
            apply Nat.sub_succ_lt_self
            repeat assumption
            -- apply Nat.lt_trans (Nat.sub_succ_lt_self arr.size (2*ix+1) h2)
            -- apply Nat.lt_of_lt_of_le (Nat.sub_succ_lt_self arr.size (2*ix) h3)
            -- exact ha

          go ((arr.set ⟨k,h⟩ a).set ⟨ix, by simp [h1]⟩ c) k
        else if lt a b then
          have : arr.size - j < arr.size - ix := by 
            apply Nat.lt_of_lt_of_le
            apply Nat.sub_succ_lt_self
            repeat assumption
            -- apply Nat.lt_of_lt_of_le (Nat.sub_succ_lt_self arr.size (2*ix) h3)
            -- exact ha
          go ((arr.set ⟨ j, h2 ⟩  a).set ⟨ ix, by simp [h1] ⟩ b) j
        else
          arr
      else if h : j < arr.size then
        let b := arr[j]
        if lt a b then
            have : arr.size - j < arr.size - ix := by
              --have h3 : 2*ix < arr.size := Nat.lt_trans (2*ix).lt_succ_self h
              --apply Nat.lt_of_lt_of_le (Nat.sub_succ_lt_self arr.size (2*ix) h3)
              apply Nat.lt_of_lt_of_le
              apply Nat.sub_succ_lt_self
              apply Nat.lt_trans
              apply Nat.lt_succ_self
              repeat assumption
            go ((arr.set ⟨ j, h ⟩  a).set ⟨ ix, by simp [h1] ⟩  b) j
        else arr
      else arr
    else arr
  go arr ix
termination_by go arr ix => arr.size - ix


private
def popAux (arr : Array α) (lt : α -> α -> Bool) : Array α :=
  if h : 1 < arr.size then
    let last := arr.size - 1
    have hzero_lt_sz : 0 < arr.size := Nat.lt_trans Nat.zero_lt_one h
    have : last < arr.size := Nat.sub_lt hzero_lt_sz Nat.zero_lt_one
    bubble (arr.set ⟨ 0, hzero_lt_sz ⟩  arr[last]).pop lt 0
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
  let x := [1,400,8,7,6,5,6,5,7,8,9,234,1,35,6,8,0,10,11,2,3,8,6,4,3,0,1,0,1,0,1,500,1024]
  let h := makeHeap ltNat
  let h := x.foldl (Heap.insert) h
  println! "{h.data}"
  
  let rec loop (h : _) : IO Unit :=
    match h.max with
    | .none => pure ()
    | .some x => do
        -- println! "{h.data}"
        println! "{x}"
        loop h.pop
  loop h

#eval main []
