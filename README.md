# Girls Just Want To Have Punctors

Puns on demand, in Haskell.

It takes in a word (like "heart") and generates puns based on popular phrases,
like:

* put the heart before the horse
* jump heart
* the godfather heart iii

## Setup

You must be using GHC 7.10 or higher because it uses its `DeriveAnyClass`
language extension.

Run:

    ./bin/setup

## Usage

Run the program with a word to pun on:

    cabal run heart

Open a REPL in the package's environment:

    ./bin/ghci

## Implementation

There are three implementations that take very different approaches to solving
the problem. They all share some common code like the `Pun` type, which is in
the `src/Common` directory. `Main.hs` takes care of hitting the Rhymebrain API,
parsing the received JSON into a list of `Common.Pun`, and passing the original
word, rhyme words, and phrases to each implementation.

The implementations work like this:

* `Implementation.Regex` uses regular expressions to find phrases that have a
   rhyme (like "put the cart before the horse") and replace the rhyme with the
   original word ("heart").
* `Implementation.Parsec` creates a `Data.Parsec`-based parser and parses each
   phrase, replacing a rhyme when it finds one.
* `Implementation.Split` splits phrases on rhymes (so you'd get `"put the "` and
  `" before the horse"`), then joins the pieces with the original word.

## Other languages

So far this library has been written in 3 (now 4) languages.

* Ian's original Ruby version: [girls_just_want_to_have_puns][ruby]
* My Go version: [gorls_just_want_to_have_puns][go]
* Draper and I co-wrote an Elixir version: [elixirls_just_want_to_have_puns][elixir]

[ruby]: https://github.com/iancanderson/girls_just_want_to_have_puns
[go]: https://github.com/gabebw/gorls_just_want_to_have_puns
[elixir]: https://github.com/drapergeek/elixirls_just_want_to_have_puns
