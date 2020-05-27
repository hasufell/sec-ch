# Test for Haskell Developer

Imagine a group of N bunnies and a left to right path made of M stepping
stones. Each stone is marked with a number which indicates how many stones
a bunny landing on that stone would jump afterwards. Positive values would
cause the bunny to jump forward on the path, negative values would cause
the bunny to jump backwards. The bunnies are thus placed each
on a stone, so that the first bunny starts on the first stone from the left,
the second bunny on the second stone and so on. All bunnies start jumping
simultaneously according to the number of jumps indicated on their respective
starting stone.

Once they start jumping, if two bunnies bump into each other, that is if they
happen to jump onto the same stone at the same time, they annihilate each
other (did I mention these are killer bunnies?). It is of course possible for
a poor bunny to end up trapped in an infinite loop, in which case it goes mad
and commits suicide. Finally some lucky bunnies (or maybe all but
perhaps none) will be able to escape this hellish path.

Your objective is to write a Haskell function with the signature:

```hs
runGame :: Int -> [Int] -> Int
```

Where the first argument is the number of bunnies participating in the game
and the second argument is the list of jump values on the stones from left
to right. The result of the function shall be the number of bunnies that would
escape the path (which might well be zero). So for example:

```
> runGame 6 [2, 1, 2, 7, 3, 9, 4, -5, 9, 0, -11]
> 2
```

(There are 6 bunnies, the first two bunnies would destroy each other on their
first jump, the third and fifth bunnies end up in an infinite loop and kill
themselves and the fourth and sixth bunnies jumps off to their freedom on the
left and right side of the path respectively.

A few important points:

- The type of the function should be exactly as specified above. No changes whatsoever.
- The time complexity of the solution with respect to the length of the path must be linear. Hint: this is the tricky part so thread carefully.
