module Functions where

-- this is all well and good, but I want to be able do something!
-- in haskell, we do things with "functions". 
-- functions in haskell look very similar to the functions in math!

-- consider the math equation 
--      f(x) = 1 + x
-- In programming, we say this is a function with one "argument", x,
-- which returns x + 1.

-- we are going to make the same function in haskell, but call it addOne.
addOne x = 1 + x
-- it's that easy! we give the function a name, just like a variable,
-- then write the argument and the rest of the equation!

-- now let's look at the type of addOne:
--    addOne :: Integer -> Integer

-- we read this as 
--   "addOne is a function from Integer to Integer"
-- this makes sense, because it takes an integer x, and makes a new
-- integer (x + 1).

-- note how when we talk about types, we aren't concerned that
-- x doesn't equal (x + 1), we only care that they are both integers!

-- adding one is nice, but what about adding 2 or 3? or adding three 
-- numbers? do I have to write a new function for each one?

-- no! that would be awful. fortunately, we can have as many
-- arguments as we want!

addFourNumbers :: Integer -> Integer -> Integer -> Integer -> Integer 
addFourNumbers a b c d = a + b + c + d

-- try writing a function that averages two numbers
-- hint: the type should be 
--    average :: Integer -> Integer -> Integer


