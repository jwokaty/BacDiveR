#' Get BacDive Data by a BacDive ID
#'
#' @param access_object BacDive Access Object BacDive access object
#' @param id integer BacDive ID
#' @param template_path character Path to the record template.
#'        Defaults to inst/extdata/template.csv.
#' @param debug character if TRUE write messages to console. Defaults to
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
#' getDataById(access_object, 12345)
getDataById <- function(access_object, id,
                        template_path = "inst/extdata/template.csv",
                        debug = FALSE) {

    tryCatch({
        response <- fetch(access_object, id)

        if (debug)
            message(paste("[DEBUG] Fetched", response$count, "records"))

        if (response$count > 0)
            .formatRecord(response$results[1], .readTemplate(template_path),
                          debug)

    }, error = function(e) {
        message(paste("[ERROR]", e$message))
    })
}
