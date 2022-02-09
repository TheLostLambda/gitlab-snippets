## Instuctions

Though you consider yourself to be a quite competent mathematician, you've
always had a bit of a gripe with mathematics: all of those numerals seem
terribly redundant. After all, we already have english words to talking about
mathematics, why waste the effort on all of these symbols too?

You've taken it upon yourself to create a prototype language in which you can
describe simple numbers and calculations using their english names.

You will need to define at least the numbers 0-99:

```lisp
(zero) ; => 0
(one)  ; => 1
(two)  ; => 2
...snip...
(ninety-nine) ; => 99
```

And the english equivalents for `+`, `-`, `*`, and `/`:

```lisp
(nineteen plus three) ; => 22
(seventy-eight minus nineteen) ; => 59
(ten times forty-seven) ; => 470
(sixty-four divided-by four) ; => 16
```

As you may have noticed, that's 104 top-level definitions that are required so
it might be worth looking into some Lisp's plentiful meta-programming
facilities.
