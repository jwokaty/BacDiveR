#' Get BacDive Data
#'
#' @param access_object BacDive Access Object BacDive access object
#' @param template_path character Path to the record template.
#'        Defaults to inst/extdata/template.csv.
#' @param output_directory character Path to the output csv.
#' @param update bool Downloads the file again if TRUE. Defaults to FALSE.
#' @param verbose character if TRUE write messages to console. Defaults to
#'        FALSE.
#'
#' @return list of lists of BacDive Data
#'
#' @importFrom BacDive fetch
#'
#' @export
#'
#' @examples
#' access_object <- authenticate('username', 'password')
#' getData(access_object)
getData <- function(access_object,
                    output_directory,
                    template_path = "inst/extdata/template.csv",
                    update = FALSE,
                    verbose = FALSE) {

    stopifnot(dir.exists(output_directory))
    output_path <- paste0(output_directory, "/bacdive_",
                          format(Sys.time(), "%Y%m%d_%H%M"),".csv")

    bacdive_data <- read.csv(.downloadCSV(update), header = TRUE, skip = 2)
    number_of_ids <- length(bacdive_data$ID)
    retrieved_ids <- c()
    unretrieved_ids <- c()
    start_index <- 1
    increment <- 99
    wait_time <- 60

    while(length(retrieved_ids) < number_of_ids) {
        records <- data.frame()
        end_index <- start_index + increment

        if (end_index >= number_of_ids)
            end_index <- number_of_ids

        if (!summary(access_object)["expired"] &&
            !summary(access_object)["refresh_expired"]) {
            access_object <- authenticate(Sys.getenv("BACDIVE_USERNAME"),
                                          Sys.getenv("BACDIVE_PASSWORD"))
        } else {
            stop("Invalid credentials")
        }

        ids < bacdive_data$ID[start_index:end_index]
        ids <- ids[!is.na(ids)] # TODO: Ensure no NAs

        tryCatch({
            records <- fetch(access_object, ids)
        }, error = function(e) {
            message(paste("Error fetching. Retrying in", wait_time, "."))
            Sys.sleep(wait_time)
            records <- fetch(access_object, ids)
        })

        for (response in records$results) {

            if (response$count > 0) {
                formatted_record <- .formatRecord(response$results[[as.character(id)]],
                                                  .readTemplate(template_path),
                                                  verbose)
                bacdive_id <- response$General$`BacDive-ID`

                tryCatch(
                    formatted_records <- rbind(formatted_records,
                                               formatted_record),
                    error = function(e) {
                        message(paste("Error on BacDive ID", bacdive_id))
                    },
                    warning = function(w) {
                        message(paste("Warning on BacDive ID", bacdive_id))
                    }
                )

                retrieved_ids <- append(retrieved_ids, bacdive_id)
            }
        }

        unretrieved_ids <- ids[!(ids %in% retrieved_ids)]
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
                      unretrieved_ids))

}
