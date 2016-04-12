# Generators using closures.
# ----------------------------------------------
#
# Terms
# -----
# thunk     : Function wrapping an element that
#             takes no arguments.
# iterable  : Any object that can be iterated
#             over. Most common objects in R.
# iterator  : Object used to iterate over an
#             iterable. In this case, a function
#             with access to an iterable.
# generator : Returns an iterator that generates
#             values on demand.


mk_thunks <- function(x, fn) {
    lthunks <- Map(fn, x)
    cl <- class(lthunks)
    class(lthunks) <- c("thunks", cl)
    lthunks
}

#' Convert iterable `x` to thunks.
thunkify <- function(x) {
    # Wrap each element in a function to take advantage
    # of lazy evaluation.
    # Reference:
    # https://stackoverflow.com/questions/32846131/create-a-promise-lazily-evaluated-expression-in-r
    #
    # Using `call` may work as an alternative.
    wrap <- function(elem) function() { elem }
    mk_thunks(x, wrap)
}

#' Apply function `fn` to thunks `x` without evaluating.
compose_thunks <- function(x, fn) {
    rewrap <- function(elem) {
        delayedAssign("thunk", elem())
        function() fn(thunk)
    }

    mk_thunks(x, rewrap)
}


set_iter <- function(x) {
    cl <- class(x)
    class(x) <- c("iterator", cl)
    x
}


#' Signal that the generator has finished
generator_exit <- function() {
    stop("Generator consumed", call. = FALSE)
}

#' Convert an iterable to a generator.
generator <- function(x, ...) UseMethod("generator")

# Generic function to create a generator.
generator_ <- function(x, fn) {
    thunks <- fn(x)

    set_iter(function() {
        if (!length(thunks)) {
            generator_exit()
        }

        current <- head(thunks, 1L)
        thunks <<- tail(thunks, -1L)

        thunk <- current[[1]]
        thunk()
    })
}

# S3 methods
generator.default <- function(x) {
    generator_(x, thunkify)
}

generator.thunks <- function(x) {
    generator_(x, identity)
}


#' Generate the next value.
nxt <- function(x, ...) UseMethod("nxt")

# Only define for iterators.
nxt.iterator <- function(x) {
    tryCatch({
        x()
    }, error = function(e) {
        msg <- e[["message"]]
        if (msg != "Generator consumed") {
            stop(e, call.=FALSE)
        }

        invisible(NULL)
    })
}

