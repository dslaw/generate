
iter_csv <- function(file, chunksize, header = TRUE, ...) {
    # TODO: use a connection instead of filename
    # TODO: handle arguments `nrows`, `skip`
    # TODO: check colClasses etc

    if (chunksize <= 0L) {
        stop("Invalid value to 'chunksize'")
    }

    reader <- function(nr, sk) {
        read.csv(file, header = FALSE, nrows = nr, skip = sk, ...)
    }

    ix <- 0L
    header <- reader(1L, ix)
    header <- as.character(header)
    ix <- ix + 1L

    chunksize <- as.integer(chunksize)

    function() {
        x <- tryCatch({
                 reader(chunksize, ix)
             }, error = function(e) {
                 function() { stop('EOF', call. = FALSE) }
             })

        if (is.function(x)) {
            # Raise error
            x()
        }

        ix <<- ix + chunksize
        colnames(x) <- header
        x
    }
}


# Ad-hoc test
write.csv(data.frame(x = 1:3, y = letters[1:3]), 'sample.csv', row.names = FALSE)

a <- iter_csv('sample.csv', chunksize = 1L) # most simple case

stopifnot( a() == data.frame(x = 1L, y = 'a') )
stopifnot( a() == data.frame(x = 2L, y = 'b') )
stopifnot( a() == data.frame(x = 3L, y = 'c') )
eof <- try(a(), silent = TRUE)
stopifnot( class(eof) == 'try-error' )


# Example
summation <- function(x, colname) {
    acc <- 0

    while (TRUE) {
        record <- try(x(), silent = TRUE)
        if (class(record) == 'try-error') break
        acc <- acc + record[[colname]]
    }

    acc
}


a <- iter_csv(f, chunksize = 1L) # most simple case
summation(a, 'x')

