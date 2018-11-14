
uvabits_vocabulary_attachment_type <- c("collar", "glue", "harness",
                                        "implant", "tape", "other")
uvabits_vocabulary_manipulation_type <- c("confined", "none", "relocated",
                                          "manipulated other")

#' Load metadata from uvabits in movebank compatible format
#'
#'
#' keyname -> project name
#' @importFrom glue glue_sql
#' @importFrom readr read_file
#' @importFrom methods is
uvabits_metadata_movebank <- function(connection,
                                      project, animal_life_stage = NA,
                                      attachment_type = "harness",
                                      manipulation_type = "none",
                                      bird_remarks_is_nickname = FALSE
                                      ) {
    # validate database connection
    check_connection(connection)

    # validate input arguments
    assert_that(is.character(animal_life_stage))
    assert_that(is.logical(bird_remarks_is_nickname))

    valid_projects <- uvabits_projects(connection)
    assert_that(project %in% valid_projects,
                msg = wrong_argument("project", valid_projects,
                            intro_string = "Projects you have access to"))
    assert_that(attachment_type %in% uvabits_vocabulary_attachment_type,
                msg = wrong_argument("attachment_type",
                                     uvabits_vocabulary_attachment_type))
    assert_that(manipulation_type %in% uvabits_vocabulary_manipulation_type,
                msg = wrong_argument("manipulation_type",
                                     uvabits_vocabulary_manipulation_type))

    # request the uvabits dbase
    sql_file <- system.file("sql", "uvabits_metadata_movebank.sql",
                            package = "bird-tracking-etl")

    metadata_movebank_query <-
        glue_sql(read_file(sql_file),
                 project = project,
                 animal_life_stage = animal_life_stage,
                 attachment_type = attachment_type,
                 manipulation_type = manipulation_type,
                 bird_remarks_is_nickname = bird_remarks_is_nickname,
                 .con = connection)
    movebank_reference_data <- dbGetQuery(connection, metadata_movebank_query)

    return(movebank_reference_data)

}

#' Get unique set of project names (key_name)
#'
#'
#' @param connection A valid connection to uvabits database.
#'
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#'
#' @return A vector of all transmitter present in vliz.tags.
uvabits_projects <- function(connection) {

    query <- glue_sql(
        "SELECT DISTINCT key_name FROM gps.ee_project_limited",
        .con = connection
    )
    data <- dbGetQuery(connection, query)
    data$key_name
}
