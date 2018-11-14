
#' Provide connection to the database using user credentials username and
#' password.
#'
#' The function require
#'
#' @param username (character) Username to use for the  connection.
#' @param password (character) Password to use for the connection.
#'
#' @return conn ODBC connection to ETN database.
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
uvabits_connect <- function(username, password) {

    psql_driver <- RPostgres::Postgres()

    conn <- DBI::dbConnect(psql_driver,
                           user = username,
                           password = password,
                           host = UVABITS_SERVER,
                           port = UVABITS_PORT,
                           dbname = UVABITS_DBNAME)
    return(conn)
}


#' Uvabits database informations
UVABITS_SERVER <- "pubserv.e-ecology.nl"
UVABITS_PORT <- "5432"
UVABITS_DBNAME <- "eecology"