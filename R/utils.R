# https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
.stripHtml <- function(s) {
    if (!is.null(s))
        gsub("<.*?>", "", s)
}

.replaceQuotes <- function(s) {
    stringr::str_replace_all(s, '\"', "'")
}

.cleanUp <- function(s) {
    .replaceQuotes(.stripHtml(s))
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
authenticate <- function(username, password) {
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
#' @param verbose logical message if TRUE. Defaults to FALSE.
#'
#' @return integer string character(0)
#'
#' @examples
#' getValue(result, 'General', 'BacDive-ID')
.getValue <- function(value, ..., verbose = FALSE) {
    keys <- list(...)[[1]]
    key <- ""
    for (key in keys) {
        if (verbose)
            message(paste("[.getValue]",  value, "[[", key, "]]"))

        if (key %in% names(value))
            value <- value[[key]]
        else if (length(value) > 0 && key %in% names(value[[1]]))
            value <- value[[1]][[key]]
    }

    if (is.null(value) || (is.list(value) && !(key %in% value))) {
        value <- ""
        if (verbose)
            message(paste("[.getValue]",  key, "not found in", value))
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
#' Attempts to parse items separated by special double quotes
#'
#' @param a_string character representing a comma-separated vector
#'
#' @return vector of character
#'
#' @importFrom stringr str_detect str_extract str_sub str_trim str_remove
#'
#' @examples
#' .toVector('“Isolation, sample, environment”, isolation')
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

.getTaxonName <- function(bacdive_data, verbose = FALSE) {
  message("[.getTaxonName] - getTaxonName")
}

.getAccession <- function(sequence_info, pattern, verbose = FALSE) {
    for (s in sequence_info) {
        if (str_match(s$accession, pattern) &&
            (s$`assembly level` == "contig")) {
            sequence_id <- s$accession
            break
        }
    }
}

.getSequenceId <- function(bacdive_data, sequence_type, verbose = FALSE) {
    sequence_info <- bacdive_data$`Sequence information`$`Genome sequences`
    sequence_id <- ""
    key <- ""

    if (is.null(sequence_info))
        return("")

    if (sequence_type == "genome")
        sequence_id <- .getAccession(sequence_info, "^GCA_", verbose)
    else if (sequence_type == "assembly")
        sequence_id <- .getAccession(sequence_info, "^NC_", verbose)

    sequence_id
}

.getAccessionId <- function(bacdive_data, verbose = FALSE) {
    .getSequenceId(bacdive_data, "accession", verbose)
}

.getGenomeId <- function(bacdive_data, verbose = FALSE) {
    .getSequenceId(bacdive_data, "genome", verbose)
}

.getNcbi <- function(bacdive_data, verbose = FALSE) {

}

.getParent <- function(bacdive_data, verbose = FALSE) {
  message("[.getNcbi] - getParent")
}

.getRank <- function(bacdive_data, verbose = FALSE) {
  message("[.getRank] - getNcbi")
}

#' Get the Scientific Name
#'
#' Try the LPSN (https://lpsn.dsmz.de/) path first. Otherwise, choose first
#' reference
#'
#' @param bacdive_data list of a bacdive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return a character vector
.getScientificName <- function(bacdive_data, verbose = FALSE) {
    keys <- c("Name and taxonomic classification", "LPSN", "full scientific name")
    scientific_name <- .getValue(bacdive_data, keys, verbose)
    if (scientific_name == "") {
        keys <- c("Name and taxonomic classification", "full scientific name")
        scientific_name <- .getValue(bacdive_data, keys, verbose)
    }
    scientific_name
}

#' Use template CSV to get keys or functions to process BacDive data
#'
#' @param bacdive_data list of a bacdive data
#' @param record_template data.frame of instructions for each row of data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return list of data
#'
#' @examples
#' formatRecord(bacdive_data, template)
.formatRecord <- function(bacdive_data, record_template, verbose = FALSE) {
    record = list()
    for (i in 1:nrow(record_template)) {
        template_row <- record_template[i, ]
        value <- ''

        if (template_row$custom_function != "") {
            value <- do.call(template_row$custom_function,
                             list("bacdive_data" = bacdive_data,
                                  "verbose" = verbose))
        } else {
            key_vector <- .toVector(template_row$keys)
            value <- .getValue(bacdive_data, key_vector, verbose)
        }

        if (!is.na(as.logical(template_row$needs_clean)) &&
            as.logical(template_row$needs_clean))
            value <- .cleanUp(value)

        record[template_row$column] <- value

        if (verbose)
            message(paste("[.formatRecord]",  template_row$column, "=", value))
    }
    record
}
