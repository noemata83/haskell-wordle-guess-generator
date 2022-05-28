# A Haskell Wordle Guess Generator (v0.0.2)

This is my first toy Haskell program. It has been carefully designed with the intent take the simple and sometimes delightful word-guessing game, Wordle, and suck any remaining joy out of it.

But it was fun to build.

## Operating Instructions

Open Wordle at the New York Times website in your browser. Open this project in the root directory and run `cabal run`
Proceed to play predictable, mediocre, but competent Wordle with the Haskell Wordle Guess Generator as your guide.

## Caveats

The algorithm used to generate guesses is not particularly sophisticated, and aims to determine, at each step, which guess will eliminate the largest number of candidate words from the remaining list of possibilities. I'm not good at math, though.

I stole and then significantly truncated a list of five-letter words not from Wordle itself but some other random internet-hosted text file of 5-letter words. For the initial version of the Wordle Guesser, I used this dictionary (`words.txt`) to mediocre but competent results. In a subsequent revision, I managed to get a hold of the actual list of possible Wordle **answers**. Having a much more limited dictionary allows the Wordle guesser to converge to the correct answer a lot faster, making the game **even less fun**.
