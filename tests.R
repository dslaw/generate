# Quick sanity checks.
source("generator.R")

xs <- 1:10

a <- generator(xs)
stopifnot( nxt(a) == 1 )

b <- thunkify(xs)
b <- generator(b)
stopifnot( nxt(b) == 1 )


is_even <- function(x) x %% 2 == 0

t <- thunkify(xs)
t <- compose_thunks(t, is_even)
t <- generator(t)
stopifnot( !nxt(t) )


"%c%" <- function(fn, x) {
    compose_thunks(x, fn)
}

d <- thunkify(xs)
e <- identity %c% d

e <- identity %c% thunkify(xs)
nxt(generator(e))

