# Advent of Code 2022

This year I'm trying AoC in Tust to kick the tires a litte.  But I may backfill some of them in Lean4.  Caveat lector - I know neither language.

I went through day 3 in rust, then backported / redid each in lean4.


Day3 - I had one function that it wouldn't accept as total. And bit operations are kinda painful. I probably should define some operators to make it prettier.


Day 5 - I did this in rust initially. Lots of weird issues dealing with borrow checker. Also I accidently did part2 first.

**Day 8**

- List is not a monad in Lean.
- Implicits don't work like Idris - `{f : Forest}` was not found.
- I had to take a different approach for part 2, thought about going back and refining part1.

- In the Rust version, I got to learn about writing iterators with a directional tree iterator.
- Went back and reworked lean version to work similar to Rust version.

**Day 9**

- Went fairly quickly.  Still need to go back and do the Rust version. (Too busy with other things.)

**Day 10**

- I ended up just injecting instructions rather than dealing with the two cycle instructions.

**Day 11**

All the decisions appeared to be modular, so I went with `lcm` of every discriminator for part 2. Threaded through a function to refine the value rather than turn the state into a structure, because I was feeling lazy.

In some of these puzzles, I've been on the fence whether to use a State monad, or just thread through the value and `foldl`. For both Lean and Rust, I really should handle checking of indices at compile time, but it'd take more time to figure that out.  I feel like I'm cheating when I do `[x]!` in lean...

In Rust I had to implement `gcd` and `lcm`. I had issues with not being able to pass in a closure, like a did for lean (for part1 vs part2 value reduction). And finally I had an issue where mutability screwed me up. I resent the monkey count for part2, but forgot that there was other state in there that needed to be reset. I did realize that trimming will make my sketchy parser look better, so I went back and tweaked the lean code to do the same.

**Day 12**

I used Array (Array Nat) for the heights. At some point I'd like to get a better feel for how the index checking works in lean, so it could carry some proofs.

For the Rust version, I mutate the grid to mark visited cells rather than keep a HashMap.

**Day 13**

This went smoothly. I used Lean's parsec for this one.  We'll see what I do on the rust side when I get to it.

On the Rust side, I wrote a quick little recursive parser. I thought the comparison would be easier with cons lists, but it made everything else messier. I got to learn about Box and realize most of my stuff to date was on stack.

**Day 14**

Day 14 went fairly smoothly. I ended up just using a hashmap for the grid so I wouldn't have to figure out sizing.  It was fast enough.

Coming back for the Rust version, I see that I went with a proper parser in lean rather than just splitting. (And Parsec seems to be very basic, no manysep, etc.) I'll do the splitting thing in Rust, but I should see if what kind of parsing tools they have sometime.

For fun, the rust version uses a vector of bool _and_ it uses the same grid for both parts, continuing the drops. It turns out just running part2 rules and looking at the final resting space of a grain is sufficient for both parts.

**Day 15**

I got this working well enough, but I'd like to come up with a strategy to skip rows, so part 2 doesn't take 30s.

Rust was about 15s here, but I copied the bit where `union` builds new objects. I think I could do it in place in Rust. And maybe I could switch to a Vec in lean to get more speedup.  The recursion seems clearer when using `List`, but I'm curious if I can get lean to update the data in place that way.

**Day 16**

The night of, I got a part 1 solution in lean, but the eg.txt was off by one. I reworked it to prepare for part 2, fixing eg.txt and breaking input.txt.  Two things needed to happen, continue when we hit an end, until the "estimate" is below our best score. And fix a bug in estimate that was deprioritizing a local valve (after I separated move / open)

Oof. So the 1651 vs 1650 thing, which I regressed, was having to keep the open and step separate. I didn't get code that did both.  I barely got part2 working.  It's taking 3 min 22 seconds to run, after I compile it. I think I need a better estimation function, and maybe I can manage the size that state grows better. 

Got it down to 1:39 by replacing the string names with integers and bitmaps for the string lists.  And down to 1:10 by using UInt64. Switching to my Heap implementation made things much worse. I suspect the RBTree is collapsing duplicate state. But I learned some stuff while writing it and checked it in for posterity.

Down to 730 ms after noticing that the open valves are not adjacent in the input.  Also tweaked day16p2 to run both part1/part2.

**Day 17**

Second part took me a little while. I settled on the race loop detection thing, but had a lot of debugging before I figured out that I wasn't taking the padding off the top when calculating the height. I also kept wedging the lean process. There isn't a nice way to kill it in the editor. 

On the rust side, I didn't explicitly pad but still had padding from the short rocks.  It's a vector, so the rocks were dropped on the end.

**Day 18**

This one was quite easy.  I "cheated" a little and hardwired the max size of the space to
avoid having to calculate and pass it around.

**Day 19**

Day 19 took me a while.  I decided to write a heap instead of doing RBTree, hoping I could just use a LT without having a full compare on the State. (I want a priority queue, but RBTree only lets me have one of each value.)  It turns out it was subtly broken. I patched it into day16 and started getting wrong answers.

It took quite a bit of fine-tuning on the estimation function, and I needed a <= instead of < for cutting (oops). The second part was quite easy given the first part.

**Day 20**

Day 20 was a linked list one. This can be a pain in a language without pointers, you need to invent your own with an Array or map. I went with an array, because lean is not supposed to copy it.  It still took nine seconds for part 2.  I had to work around lean having the flavor of mod that returns negative numbers too.

I ran RBMap just to see, 10.6 sec for my Array version, 1:33.34 for RBMap, and about 24s for HashMap.

**Day 21**

Day 21 was a nice match for a functional programming language.

**Day 22**

This one was an ordeal. My initial version of part1 had start/end for each row and column. I then wrote some code to handle the wrapping for part 2 `eg.txt`, only to find that `input.txt` was a completely different shape. I should have just hacked together a version that worked for the input shape, but instead I wrote the code to figure how how stuff maps to the cube and does all of the rotation / translation as necessary. There are a few partials and ! in the lean version that I think are within my capability to handle, but I'll move on to Day 23 for now.

**Day 23**

This works.  Wasn't too bad in lean, but takes a little while (31s).  The data was sparse and coordinates keep getting bigger, so I used an RBMap. I might have been able to use an oversized vector.

**Day 24**

This one wasn't too bad. I started out with an search thing, and then decided it was easy enough to just step the grid through time and marking every possible position for the elves. I'm tempted to retry with just the elves and not update the wind.  (Or I could poke around with dimensions nd proofs.)

Ok, for fun I skipped updating the map and kept a RBTree of elves, and it was slower.

**Day 25**

An easy one, kinda reminded me of negabinary, but I didn't bother looking up the details of that. Code was a little imperative, but I'll save trying other approaches for another day.

## Rust Notes

- Editor tooling is nice:
    - Auto insert of imports
    - Good descriptions of what to do when I do something stupid
    - The run thing is nice, but Lean kinda has me spoiled on that front.
    - Add missing match cases
- Minus on editor tooling:
    - Showing the types is really annoying, makes the formatting of code look awful,
      I think I'd prefer on hover.
- I keep forgeting snake_case
- Writing > instead of >> can lead to some hard to follow syntax errors.
- When I create a new project (cargo init), it doesn't seem to be picked up in an existin vscode window without reloading.
- oh nice, I renamed `st` to `self`, and it rewrote the parameters correctly.
- BUG F2 rename doesn't get inside strings
- disabled annoying inllays
- It keeps whining about my camelCase names.

## Lean Notes

- Hard to find functions (e.g. how do I turn a List Char -> String, took a while to figure out)
- The List is not a monad thing threw me off a little.
- Not sure where the docs are for libraries, I've been searching the source.
- I really like the results panel in vscode tooling, it updates as I type code. 
  (Even better than jupyter)
- I don't get my magic Idris `[]` and `::`.
- Lean produces executables, but they're like 50 MB for a tiny program (day8) vs 572k
- BUG - If I try to complete IO.F, I get IO.IO.FS
- Like Idris, it is hard to google.  Searching for Idris2 helps, so I've been trying "lean4".
- How do I define something like `notation "‖ " a " ‖" => Int.abs a` that doesn't assume it's nesting when it hits the second deliminator.
- If I use an unknown type name (e.g. `Position` instead of `Pos`), I end up with weird universe level errors instead of unknown name.

I had a question on this:
```lean
  for r in [0:data.size] do
    -- lean doesn't figure out that r : Fin data.size
    match data[r]!.getIdx? needle with
    ...
```

It turns out that you can pull in a proof that `r ∈ [0:data.size]` with a colon:
```lean
for h : r in [0:data.size] do
  have : r < data.size := h.2
  match data[r].getIdx? needle with
  ...
```
(It suffices to just bring h.2 into scope by assigning it to a name.)

```lean
  -- I'd like to use Idris | notation here, is there an equiv?
  let start := (grid.find? 83).get!
```

I managed to figure out day13 that the notation is there, but it doesn't have a match in the `|` branch, so my blind attempt to use it didn't work.
