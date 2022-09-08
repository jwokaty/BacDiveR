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
#' @param verbose bool
#'
#' @return A BacDive access object
#'
#' @importFrom BacDive open_bacdive
#'
#' @export
#'
#' @examples
#' Sys.setenv("BACDIVE_USERNAME" = "my.user.name@my.school.edu",
#'            "BACDIVE_PASSWORD" = "my password")
#' ao <- authenticate(Sys.getenv("BACDIVE_USERNAME"),
#'                    Sys.getenv("BACDIVE_PASSWORD"))
authenticate <- function(username, password, verbose = TRUE) {
    if (interactive() && verbose)
        message(paste("If you intend to fetch all records, set your",
                      "username and password in environment variables",
                      "with Sys.setenv() to allow BacDiveR to",
                      "reauthenticate with BacDive."))
    open_bacdive(username, password)
}

#' Download CSV with BacDive IDs
#'
#' @param bacdive_url character(1) URL to BacDive CSV. Defaults to
#'        https://bacdive.dsmz.de/advsearch/csv.
#' @param update bool Downloads the file again if TRUE. Defaults to FALSE.
#'
#' @importFrom BiocFileCache BiocFileCache bfcrpath bfcinfo
#'
#' @examples
#' downloadCSV()
#' downloadCSV("https://bacdive.dsmz.de/advsearch/csv")
.downloadCSV <- function(bacdive_url = "https://bacdive.dsmz.de/advsearch/csv",
                         update = FALSE) {
    bfc <- BiocFileCache()
    bacdive_csv_bfc_path <- bfcrpath(bfc, bacdive_url)
    if (update)
        bfcdownload(bfc, bfcinfo()$rid, ask = FALSE)
    bacdive_csv_bfc_path
}

.getValidRanks <- function() {
    c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus",
      "species", "strain")
}

#' Get values if any exist
#'
#' @param value A list of lists of data
#' @param ... A list
#' @param as_json logical if TRUE, return as JSON. Defaults to FALSE.
#' @param verbose logical message if TRUE. Defaults to FALSE.
#'
#' @return integer string character(0) list
#'
#' @examples
#' getValues(result, 'General', 'BacDive-ID')
.getValues <- function(value, ..., as_json = FALSE, verbose = FALSE) {
    elements <- unlist(list(...)) # TODO: Need better way to do this

    for (element in elements) {
        if (element %in% names(value))
            value <- value[[element]]
        else if (length(value) > 0 && element %in% names(value[[1]]))
            value <- value[[1]][[element]]
        else {
            value <- ""
            break
        }
    }

    value
}

#' Get all data and transform into JSON
#'
#' @param bacdive_data list of a BacDive data
#' @param elements vector of strings of elements in BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' .getAll(bacdive_data, elements, TRUE)
.getAll <- function(bacdive_data, elements, verbose = FALSE) {
    values <- .getValues(bacdive_data, elements, as_json = TRUE, verbose = verbose)
    values
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

.getSequenceDatabases <- function() {
    c("ena", "patric", "ncbi", "img", "nuccore")
}

#' Get sequence id and append to string separated by semicolons
#'
#' @param bacdive_data list of a BacDive data
#' @param elements character representing the elements column of the template
#' @param database character representing the sequence databases in
#'                 .getSequenceDatabases().
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getSequenceId(bacdive_data,"Sequence information, 16S sequences", "ena")
.getSequenceId <- function(bacdive_data, elements, database, verbose = FALSE) {
    ids <- ""
    sequences <- .getValues(bacdive_data, elements, verbose = verbose)

    if (length(sequences) == 1 && sequences == "")
        return(ids)

    if (!is.atomic(sequences[[1]])) {
        for (sequence in sequences) {
            if (sequence$database == database) {
                sep <- ifelse((ids == ""), "", ";")
                ids <- paste(ids, as.character(sequence$accession), sep = sep)
            }
            else if (!(sequence$database %in% .getSequenceDatabases()))
                message(paste(sequence$database, "not in databases!!!!!"))
        }
    } else if (sequences$database == database)
        ids <- as.character(sequences$accession)
    ids
}

.getSequenceEnaId <- function(bacdive_data, elements, verbose = FALSE) {
    .getSequenceId(bacdive_data, elements, "ena", verbose = verbose)
}

.getSequenceNcbiId <- function(bacdive_data, elements, verbose = FALSE) {
    .getSequenceId(bacdive_data, elements, "ncbi", verbose = verbose)
}

.getSequenceImgId <- function(bacdive_data, elements, verbose = FALSE) {
    .getSequenceId(bacdive_data, elements, "img", verbose = verbose)
}

.getSequenceNuccoreId <- function(bacdive_data, elements, verbose = FALSE) {
    .getSequenceId(bacdive_data, elements, "nuccore", verbose = verbose)
}

.getSequencePatricId <- function(bacdive_data, elements, verbose = FALSE) {
    .getSequenceId(bacdive_data, elements, "patric", verbose = verbose)
}

#' Get Taxon Name from General Description
#'
#' @param bacdive_data list of a BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @importFrom stringr str_split
#'
#' @examples
#' .getTaxonName(bacdive_data, TRUE)
.getTaxonName <- function(bacdive_data, verbose = FALSE) {
    description <- stringr::str_split(bacdive_data$General$description,
                                      " is ",
                                      simplify = TRUE)
    taxon_name <- description[1, 1]
    .cleanUp(taxon_name)
}

#' Get NCBI ID
#'
#' If BacDive doesn't have NCBI ID in 'General', try searching by the taxon_name
#' in taxizedb.
#'
#' @param bacdive_data list of a BacDive data
#' @param level character representing the order. Defaults to 'strain'.
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @importFrom taxizedb name2taxid
#'
#' @examples
#' .getTaxonName(bacdive_data, TRUE)
.getNcbiId <- function(bacdive_data, level = "strain", verbose = FALSE) {
    ncbi_id <- ""

    if (!is.null(bacdive_data$General$`NCBI tax id`)) {
        for (ncbi_tax_id in bacdive_data$General$`NCBI tax id`) {
            if (ncbi_tax_id$`Matching level` == level)
                ncbi_id <- as.character(ncbi_tax_id$`NCBI tax id`)
        }

    } else {
        taxon_name <- .getTaxonName(bacdive_data, verbose = verbose)
        ncbi_id <- taxizedb::name2taxid(taxon_name)
        ncbi_id <- ifelse(is.na(ncbi_id), "", paste(ncbi_id, "via taxizedb"))
    }

    ncbi_id

}

.getParentNcbiId <- function(bacdive_data, verbose = FALSE) {
    ncbi_id <- .getNcbiId(bacdive_data, "species", verbose = verbose)
    if (ncbi_id == "") {
        parent <- .getParent(bacdive_data, "Name and taxonomic classification",
                             verbose)
        parent_rank <- taxize::tax_rank(parent, db = "ncbi")
        ncbi_id <- .getNcbiId(bacdive_data, parent_rank, verbose)
    }
    ncbi_id

}

#' Get parent taxon name
#'
#' If the species doesn't exist, check the next rank up in .getValidRank()
#'
#'
#' @param bacdive_data list of a BacDive data
#' @param elements character representing the elements column of the template
#' @param database character representing the sequence databases in
#'                 .getSequenceDatabases().
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getParent(bacdive_data, "Name and taxonomic classification")
.getParent <- function(bacdive_data, elements, verbose = FALSE) {
    parent <- ""
    for (rank in rev(.getValidRanks()[1:8])) {
        parent <- .getValues(bacdive_data, c(elements, rank), verbose = verbose)
        if (!is.na(parent))
            break
    }
    parent
}

.getRank <- function(bacdive_data, elements, verbose = FALSE) {
    message("[.getRank] - getRank")
}

#' Use template CSV to get elements or functions to process BacDive data
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
    formatted_record = list()

    for (i in 1:nrow(record_template)) {
        template_row <- record_template[i, ]
        value <- ""
        custom_function <- template_row$custom_function

        if (template_row$elements != "")
            elements <- .toVector(template_row$elements)

        if (template_row$elements != "" && custom_function != "")  {
            value <- do.call(custom_function,
                             list("bacdive_data" = bacdive_data,
                                  "elements" = elements,
                                  "verbose" = verbose))
        } else if (custom_function != "") {
            value <- do.call(custom_function,
                             list("bacdive_data" = bacdive_data,
                                  "verbose" = verbose))
        } else {
            value <- .getValues(bacdive_data, elements, as_json = FALSE,
                                verbose = verbose)
        }

        if (!is.na(as.logical(template_row$needs_clean)) &&
            as.logical(template_row$needs_clean))
            value <- .cleanUp(value)

        formatted_record[template_row$column] <- ifelse(is.null(value), "",
                                                        as.character(value))

        if (verbose)
            message(paste("[.formatRecord]",  template_row$column, "=", value))
    }

    formatted_record
}
