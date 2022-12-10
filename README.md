# Advent of Code 2022

This year I'm trying AoC in Tust to kick the tires a litte.  But I may backfill some of them in Lean4.  Caveat lector - I know neither language.

I went through day 3 in rust, then backported / redid each in lean4.


## Lean

Day3 - I had one function that it wouldn't accept as total. And bit operations are kinda painful. I probably should define some operators to make it prettier.


Day 5 - I did this in rust initially. Lots of weird issues dealing with borrow checker. Also I accidently did part2 first.

Day 8

- List is not a monad.
- Implicits don't work like Idris {f : Forest} was not found.
- I had to take a different approach for part 2, thought about going back and refining part1.

Day 9

- Went fairly quickly.  Still need to go back and do the Rust version. (Too busy with other things.)

Day 10

- Ooh, I may get to write a VM again.




## General Lean Notes

- The List is not a monad thing threw me off a little.
- Not sure where the docs are for libraries, I've been searching the source.
