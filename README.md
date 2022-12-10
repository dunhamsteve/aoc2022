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

**Day 9**

- Went fairly quickly.  Still need to go back and do the Rust version. (Too busy with other things.)

**Day 10**

- I ended up just injecting instructions rather than dealing with the two cycle instructions.


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

## Lean Notes

- Hard to find functions (e.g. how do I turn a List Char -> String, took a while to figure out)
- The List is not a monad thing threw me off a little.
- Not sure where the docs are for libraries, I've been searching the source.
- I really like the results panel in vscode tooling, it updates as I type code. 
  (Even better than jupyter)
