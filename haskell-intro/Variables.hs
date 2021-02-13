-- welcome to haskell!

-- this is a comment! it allows us to type
-- in whatever way we want, and it won't
-- change how the program runs.

-- the comments in these files will walk you
-- through the basics of haskell.

-- you can ignore this line for now, each file has 
-- a "module declaration" that looks like this.
module Variables where

-- we use "variables" to store values we want to remember.
-- haskell makes this simple, like this:
myAge = 27

-- or this
myName = "Sherley"

-- haskell is called a "typed language", because all 
-- values (and expressions) have a "type".
-- the type tells us more about what the value is
-- supposed to represent.

-- For example the value 27 has the type Integer
-- because it is a whole number. It represents a count.
-- In the case of myAge, 27 is how many years old I am!

-- we can tell haskell what type something is with this syntax:
-- name :: Type
-- take my dog Watson as an example:

watsonsAge :: Integer
watsonsAge = 7 * 7         -- 7 in human years, 49 in dog years!

-- haskell is very clever with types, and will often figure
-- out the type on its own (as it did with myAge and myName).
-- however, it's usually best to write our types out so 
-- that other people can read it too.

-- see if you can guess what the type of ageGap is:
ageGap = watsonsAge - myAge

-- Some final notes:
--   - once we give a variable a value, we can't make another variable
--      with the same name
--   - We saw * and - at work, there's also + and / like you might expect!
--   - myName has type String, which is the programmer's way of saying
--      a "string of characters" or simply "text". String is a very useful type!

-- Exercises:
--      1) make a variable for your name and age
--      2) make a variable for the number of years you've been in school
