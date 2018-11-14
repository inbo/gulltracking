#' Check the validity of the database connection
#'
#' @param connection A valid connection with the uvabits database.
#'
#' @importFrom methods is
#' @importFrom assertthat assert_that
#' @importFrom DBI dbIsValid dbGetInfo
#'
check_connection  <- function(connection) {

    if (is(connection, "PostgreSQL")) {
        assert_that(connection@info$dbname == "eecology")

    } else if (is(connection, "PqConnection")) {
        assert_that(dbIsValid(connection))
        assert_that(dbGetInfo(connection)$dbname == "eecology")
    } else{
        stop("The provided connection object is not a supported connection",
             "to the uvabits postgres database.")
    }
}

#' Write a error message about wrong input argument
#'
wrong_argument <- function(arg_name, options_to_print,
                           intro_string = "Valid inputs are") {
    glue("Not a valid input value for {arg_name} input argument.
          {intro_string}: {options_to_print*}.",
         .transformer = collapse_transformer(sep = ", ",
                                             last = " and "))
}

#' Support function for printing option help message
#'
#' @param regex A regular expression to parse
#' @param ... Additional arguments passed to the collapse
#'
#' @importFrom glue glue_collapse
collapse_transformer <- function(regex = "[*]$", ...) {
    function(code, envir) {
        if (grepl(regex, code)) {
            code <- sub(regex, "", code)
        }
        res <- eval(parse(text = code), envir)
        glue_collapse(res, ...)
    }
}