source('generator.R')

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

    set_iter(function() {
        rows <- tryCatch({
                 x <- reader(chunksize, ix)
                 colnames(x) <- header
                 x
             }, error = function(e) {
                 generator_exit()
             })

        ix <<- ix + chunksize
        rows
    })
}


# Ad-hoc test
write.csv(data.frame(x = 1:3, y = letters[1:3]), 'sample.csv', row.names = FALSE)

a <- iter_csv('sample.csv', chunksize = 1L) # most simple case

stopifnot( nxt(a) == data.frame(x = 1L, y = 'a') )
stopifnot( nxt(a) == data.frame(x = 2L, y = 'b') )
stopifnot( nxt(a) == data.frame(x = 3L, y = 'c') )
stopifnot( is.null(term <- nxt(a)))


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


a <- iter_csv('sample.csv', chunksize = 1L) # most simple case
stopifnot( summation(a, 'x') == sum(1:3) )

