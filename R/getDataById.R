#' Get BacDive Data by a BacDive ID
#'
#' @param access_object BacDive Access Object BacDive access object
#' @param id integer BacDive ID
#' @param template_path character Path to the record template.
#'        Defaults to inst/extdata/template.csv.
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
#' getDataByBacDiveId(access_object, 12345)
getDataByBacDiveId <- function(access_object, id,
                               template_path = "inst/extdata/template.csv",
                               verbose = FALSE) {

    tryCatch({
        response <- fetch(access_object, id)

        if (verbose)
            message(paste("[DEBUG] Fetched", response$count, "records"))

        if (response$count > 0)
            .formatRecord(response$results[[as.character(id)]],
                          .readTemplate(template_path),
                          verbose)

    }, error = function(e) {
        message(paste("[ERROR]", e$message))
    }, warning = function(w) {
      message(paste("[WARNING]", w$message))
    })
}
