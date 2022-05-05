# https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
.stripHtml <- function(s) {
    if (!is.null(s))
        gsub("<.*?>", "", s)
}

.replaceQuotes <- function(s) {
    stringr::str_replace_all(s, '\"', "'")
}

.cleanUp <- function(s) {
    replaceQuotes(stripHtml(s))
}

#' authenticate to BacDive
#'
#' Credentials should be stored in environment variables "BACDIVE_USERNAME" and
#' "BACDIVE_PASSWORD".
#'
#' @param username character(1) BacDive email
#' @param password character(1) BacDive password
#'
#' @return A BacDive access object
#'
#' @importFrom BacDive open_bacdive
#'
#' @examples
#' authenticate(
.authenticate <- function(username, password) {
    open_bacdive(username, password)
}

#' Download CSV with BacDive IDs
#'
#' @param bacdive_url character(1) URL to BacDive CSV. Defaults to
#'        https://bacdive.dsmz.de/advsearch/csv.
#' @param bacdive_csv character(1) CSV with all BacDive IDs. Defaults to
#'        writing BacDiveIDs.csv in the current working directory.
#' @param update bool Downloads the file again if TRUE. Defaults to FALSE.
#'
#' @examples
#' downloadCSV()
#' downloadCSV("https://bacdive.dsmz.de/advsearch/csv", "BacDiveIDs.csv")
.downloadCSV <- function(bacdive_url = "https://bacdive.dsmz.de/advsearch/csv",
                        bacdive_csv = "BacDiveIDs.csv",
                        update = FALSE) {
    if (update & file.exists(bacdive_csv))
        file.remove(bacdive_csv)
    if (!file.exists(bacdive_csv))
        download.file(bacdive_url, destfile = bacdive_csv)
}

.getValidRanks <- function() {
    c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus",
      "species", "strain")
}

#' Get value if it exists
#'
#' @param value A list of lists of data
#' @param ... A list
#' @param debug logical Message if TRUE. Defaults to FALSE.
#'
#' @return integer string character(0)
#'
#' @examples
#' getValue(result, 'General', 'BacDive-ID')
.getValue <- function(value, ..., debug = FALSE) {
    keys <- list(...)
    key <- ""
    for (key in keys) {
        if (debug)
            message(paste("[DEBUG]",  value, "[[", key, "]]"))
        if (key %in% names(value))
            value <- value[[key]]
    }

    if (is.null(value) || (is.list(value) && !(key %in% value))) {
        value <- ""
        if (debug)
            message(paste("[DEBUG]",  key, "not found in", value))
    }
    value
}

#' Read BacDive CSV and return a list of BacDive IDs
#'
#' @param bacdive_csv character(1) CSV with BacDive IDs. Defaults to
#'        BacDiveIDs.csv.
#'
#' @return list of integers BacDive IDs
#'
#' @examples
#' readCSV(bacdive_csv)
.readCSV <- function(bacdive_csv = "BacDiveIDs.csv") {
    bacdive_data <- read.csv(bacdive_csv, header = TRUE, skip = 2)
    bacdive_data$ID
}

#' Read Template CSV for creating rows from BacDive data
#'
#' @param path character(1) to template.csv
#'
#' @return data.frame of instructions per row
#'
#' @examples
#' template <- readTemplate()
.readTemplate <- function(path) {
    read.csv(path, header = TRUE)
}

#' Convert a string separated by commas into a vector of strings
#'
#' Attempts to parse items separated by double quotes
#'
#' @param a_string character representing a comma-separated vector
#'
#' @return vector of character
#'
#' @importFrom stringr str_detect str_extract str_sub str_trim str_remove
#'
#' @examples
#' toVector('“Isolation, sample, environment”, isolation')
.toVector <- function(a_string) {
    a_vector <- c()

    while (nchar(a_string) > 0) {
        if (str_detect(a_string, "^“.*”")) {
            quoted_string <- str_extract(a_string, "^“[^(“|”)]*”")
            unquoted_string <- str_sub(quoted_string, 2, nchar(quoted_string) - 1)
            a_vector <- c(a_vector, unquoted_string)
            remaining_substring <- str_sub(a_string, nchar(quoted_string) + 1)
            comma <- ifelse(str_detect(remaining_substring, "^,"), 1, 0)
            a_string <- str_trim(str_sub(remaining_substring, 1 + comma))
        } else if (str_detect(a_string, "^.*,")) {
            comma_string <- str_extract(a_string, "^[^,]*,")
            a_vector <- c(a_vector, str_remove(comma_string, ","))
            a_string <- str_trim(str_sub(a_string, nchar(comma_string) + 1))
        } else {
            a_vector <- c(a_vector, a_string)
            break
        }
  }

  a_vector
}

.getTaxonName <- function(bacdive_data) {
  message("[DEBUG] - getTaxonName")
}

.getNcbi <- function(bacdive_data) {
  message("[DEBUG] - getNcbi")
}

.getParent <- function(bacdive_data) {
  message("[DEBUG] - getParent")
}

.getRank <- function(bacdive_data) {
  message("[DEBUG] - getRank")
}

.getScientificName <- function(bacdive_data) {
  message("[DEBUG] - getScientificName")
}

#' Use template CSV to create functions to apply on BacDive records
#'
#' @param bacdive_data list of lists
#' @param record_template data.frame of instructions for each row of data
#' @param debug logical print debug messages. Default to FALSE.
#'
#' @return list of data
#'
#' @examples
#' formatRecord(bacdive_data, template)
.formatRecord <- function(bacdive_data, record_template, debug = FALSE) {
    record = list()
    for (i in 1:nrow(record_template)) {
        template_row <- record_template[i, ]
        value <- ''

        if (as.logical(template_row$is_function))
            value <- do.call(template_row$keys, bacdive_data)
        else
            key_list <- .toVector(template_row$keys)
            value <- .getValue(bacdive_data, key_list)

        if (as.logical(template_row$needs_clean))
            value <- cleanUp(value)

        bacdive_record[template_row$column] <- value

        if (debug)
            message(paste("[DEBUG]",  template_row$column, "=", value))
    }
    bacdive_record
}
