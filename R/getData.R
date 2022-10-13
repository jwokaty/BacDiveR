#' Get BacDive Data
#'
#' @param access_object BacDive Access Object BacDive access object
#' @param template_path character Path to the record template.
#'        Defaults to inst/extdata/template.csv.
#' @param output_directory character Path to the output csv. Defaults to current
#'        directory.
#' @param update bool Downloads the file again if TRUE. Defaults to FALSE.
#' @param verbose character if TRUE write messages to console. Defaults to
#'        FALSE.
#'
#' @return list of lists of BacDive Data
#'
#' @importFrom BacDive fetch refresh
#'
#' @export
#'
#' @examples
#' access_object <- authenticate('username', 'password')
#' getData(access_object)
getData <- function(access_object,
                    output_directory = getwd(),
                    template_path = "inst/extdata/template.csv",
                    update = FALSE,
                    verbose = FALSE) {

    if (verbose)
        print(paste("Retrieving data", Sys.Date(), Sys.time()))

    stopifnot(dir.exists(output_directory))
    output_path <- paste0(output_directory, "/bacdive_",
                          format(Sys.time(), "%Y%m%d_%H%M"),".csv")

    bacdive_data <- read.csv(file.path(getwd(), "bacdive_ids.csv"), skip = 2)
    number_of_ids <- length(bacdive_data$ID)
    retrieved_ids <- c()
    unretrieved_ids <- c()
    start_index <- 1
    increment <- 99
    wait_time <- 60

    while((length(retrieved_ids) + length(unretrieved_ids)) < number_of_ids) {
        formatted_records <- data.frame()
        end_index <- start_index + increment

        if (end_index >= number_of_ids)
            end_index <- number_of_ids

        if (!summary(access_object)["expired"] &&
            !summary(access_object)["refresh_expired"]) {
            access_object <- authenticate(Sys.getenv("BACDIVE_USERNAME"),
                                          Sys.getenv("BACDIVE_PASSWORD"),
                                          verbose)
        } else {
            stop("Invalid credentials")
        }

        ids <- bacdive_data$ID[start_index:end_index]
        ids <- ids[!is.na(ids)] # TODO: Ensure no NAs

        tryCatch({
            bacdive_records <- fetch(access_object, ids)
        }, error = function(e) {
            message(paste("Error fetching. Retrying in", wait_time, "seconds."))
            Sys.sleep(wait_time)
            bacdive_records <- fetch(access_object, ids)
        })

        for (response in bacdive_records$results) {

            bacdive_id <- response$General$`BacDive-ID`
            formatted_record <- .formatRecord(response,
                                              .readTemplate(template_path),
                                              verbose)

            tryCatch(
                formatted_records <- rbind(formatted_records, formatted_record)
            , error = function(e)
                message(paste("[ERROR]", e$message))
            , warning = function(w)
                message(paste("[WARNING]", w$message))
            )

            retrieved_ids <- append(retrieved_ids, bacdive_id)
        }

        unretrieved_ids <- append(unretrieved_ids,
                                  ids[!(ids %in% retrieved_ids)])
        start_index <- end_index + 1
        append_column_names <- length(retrieved_ids) <= (increment + 1)

        tryCatch(
            write.table(formatted_records, output_path, row.names = FALSE,
                        col.names = append_column_names,
                        append = !append_column_names),
            warning = function(w) {
                message(w)
            }
        )
    }

    if (verbose)
        message(paste(number_of_ids, "expected.\n", length(retrieved_ids),
                      "fetched.\n", length(unretrieved_ids),
                      "unable to be fetched.\nUnable to fetch BacDive IDs:",
                      unretrieved_ids, "\n", Sys.Date(), Sys.time()))

}
