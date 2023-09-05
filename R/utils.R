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

.getValidRanks <- function() {
    c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus",
      "species", "strain")
}

.getSequenceDatabases <- function() {
    c("ena", "patric", "ncbi", "img", "nuccore")
}

.getRank <- function(bacdive_data,verbose = FALSE) {
    "strain"
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
#' @param update_cache bool Downloads the file again if TRUE. Defaults to FALSE.
#'
#' @importFrom BiocFileCache BiocFileCache bfcadd bfcdownload
#'
#' @examples
#' downloadCSV()
#' downloadCSV("https://bacdive.dsmz.de/advsearch/csv")
.downloadCSV <- function(bacdive_url = "https://bacdive.dsmz.de/advsearch/csv",
                         update_cache = FALSE) {
    path <- tempfile()
    bfc <- BiocFileCache(path, ask = FALSE)
    bacdive_csv_cache <- bfcadd(bfc, "BacDive", fpath = bacdive_url)
    rid <- names(bacdive_csv_cache)
    if (update_cache)
        BiocFileCache::bfcdownload(bfc, rid, ask = FALSE)
    bacdive_csv_cache
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

#' Get halophily data
#'
#' Data will be in the format: salt growth tested_relation salt_concentration
#' Data will be separated by semicolons.
#'
#' @param bacdive_data list of a BacDive data
#' @param elements vector of strings of elements in BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getHalophily(bacdive_data, elements, TRUE)
.getHalophily <- function(bacdive_data, elements, verbose = FALSE) {
    halophily <- ""
    halophily_entries <- .getValues(bacdive_data, elements, verbose = verbose)
    if (class(halophily_entries) == "list") {
        if (class(halophily_entries[[1]]) == "list") {
            for (halophily_entry in halophily_entries) {
                sep <- ifelse((halophily == ""), "", ";")
                entry <- paste(halophily_entry$salt, halophily_entry$growth,
                               halophily_entry$`tested relation`,
                               halophily_entry$concentration)
                halophily <- paste(halophily, entry, sep = sep)
            }
        } else {
            halophily <- paste(halophily_entries$salt, halophily_entries$growth,
                               halophily_entries$`tested relation`,
                               halophily_entries$concentration)
        }
    }
    halophily
}

#' Get hemolysis data
#'
#' Data will be in the format: metabolite production
#' Data will be separated by semicolons.
#'
#' @param bacdive_data list of a BacDive data
#' @param elements vector of strings of elements in BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getHemolysis(bacdive_data, elements)
.getHemolysis <- function(bacdive_data, elements, verbose = FALSE) {
    hemolysis <- ""
    hemolysis_entries <- .getValues(bacdive_data, elements, verbose = verbose)
    if (class(hemolysis_entries) == "list") {
        if (class(hemolysis_entries[[1]]) == "list") {
            for (hemolysis_entry in hemolysis_entries) {
               sep <- ifelse((hemolysis == ""), "", ";")
               hemolysis <- paste(hemolysis, hemolysis_entry$`type of hemolysis`,
                                  sep = sep)
            }
        } else
            hemolysis <- hemolysis_entries$`type of hemolysis`
    }
    hemolysis
}

#' Get metabolite production data
#'
#' Data will be in the format: metabolite production
#' Data will be separated by semicolons.
#'
#' @param metabolite_entries list of metabolites entries
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getMetabolitesProduction(metabolites_entries, TRUE)
.getMetabolitesProduction <- function(metabolites_entries, verbose = verbose) {
    metabolites <- ""
    if (class(metabolites_entries[[1]]) == "list") {
        for (metabolite_entry in metabolites_entries) {
            sep <- ifelse((metabolites == ""), "", ";")
            production <- paste(metabolite_entry$metabolite,
                                metabolite_entry$production)
            metabolites <- paste(metabolites, production, sep = sep)
        }
    } else {
        metabolites <- paste(metabolites_entries$metabolite,
                             metabolites_entries$production)
    }
    metabolites
}

#' Get metabolite utilization data
#'
#' Data will be in the format: metabolite utilization_activity
#' kind_of_utilization_tested
#'
#' @param metabolite_entries list of metabolites entries
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getMetabolitesUtilization(metabolites_entries, TRUE)
.getMetabolitesUtilization <- function(metabolites_entries, verbose = verbose) {
    metabolites <- ""
    if (class(metabolites_entries[[1]]) == "list") {
        for (metabolite_entry in metabolites_entries) {
            sep <- ifelse((metabolites == ""), "", ";")
            utilization <- paste(metabolite_entry$metabolite,
                                 metabolite_entry$`utilization activity`,
                                 metabolite_entry$`kind of utilization tested`)
            metabolites <- paste(metabolites, utilization, sep = sep)
        }
    } else {
        metabolites <- paste(metabolites_entries$metabolite,
                             metabolites_entries$`utilization activity`,
                             metabolites_entries$`kind of utilization tested`)
    }
    metabolites
}

#' Get metabolite data, which are in lists of lists
#'
#' Separate multiple values by a semicolon
#'
#' @param bacdive_data list of a BacDive data
#' @param elements vector of strings of elements in BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getMetabolites(bacdive_data, elements, TRUE)
.getMetabolites <- function(bacdive_data, elements, verbose = FALSE) {
    metabolites <- ""
    metabolites_entries <- .getValues(bacdive_data, elements, verbose = verbose)
    if (class(metabolites_entries) == "list") {
        if ("utilization activity" %in% names(metabolites_entries[[1]])) {
            metabolites <- .getMetabolitesUtilization(metabolites_entries,
                                                     verbose)
        } else if ("production" %in% names(metabolites_entries[[1]])) {
            metabolites <- .getMetabolitesProduction(metabolites_entries,
                                                     verbose)
        }
    }
    metabolites
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
#' .getSequenceId(bacdive_data, "Sequence information, 16S sequences", "ena")
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
                message(paste(sequence$database, "should be added to databases"))
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

    if ("NCBI tax id" %in% names(bacdive_data$General)) {
        ncbi_tax_id_list <- bacdive_data$General$`NCBI tax id`
        if (class(ncbi_tax_id_list[[1]]) == "list") {
            for (ncbi_tax_id in ncbi_tax_id_list) {
                if (ncbi_tax_id$`Matching level` == level) {
                    ncbi_id <- as.character(ncbi_tax_id$`NCBI tax id`)
                    break
                }
            }
        } else if (ncbi_tax_id_list$`Matching level` == level)
            ncbi_id <- as.character(ncbi_tax_id_list$`NCBI tax id`)
    }

    if (ncbi_id == "") {
        taxon_name <- .getTaxonName(bacdive_data, verbose = verbose)
        ncbi_id <- taxizedb::name2taxid(taxon_name)
        ncbi_id <- ifelse(is.na(ncbi_id), "", ncbi_id)
    }

    ncbi_id

}

#' Use LPSN if available for classification
#'
#' @param bacdive_data list of a BacDive data
#' @param elements vector of strings of elements in BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return list
#'
#' @examples
#' .getClassification(bacdive_data, elements, TRUE)
.getClassification <- function(bacdive_data, elements, verbose = FALSE) {
    classification <- .getValues(bacdive_data, elements, verbose = verbose)
    if ("LPSN" %in% names(classification))
        classification <- classification$LPSN
    classification
}

#' Get parent taxon name
#'
#' If the species doesn't exist, check the next rank up in .getValidRank()
#'
#' @param bacdive_data list of a BacDive data
#' @param elements character representing the elements column of the template
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getParent(bacdive_data, "Name and taxonomic classification")
.getParent <- function(bacdive_data, elements, verbose = FALSE) {
    parent <- ""
    classification <- .getClassification(bacdive_data, elements, verbose = verbose)
    for (rank in rev(.getValidRanks()[1:8])) {
        parent <- classification[[rank]]
        if (!is.na(parent))
            break
    }
    parent
}

#' Get parent rank
#'
#' @param bacdive_data list of a BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @examples
#' .getParentRank(bacdive_data, , "Name and taxonomic classification")
.getParentRank <- function(bacdive_data, elements, verbose = FALSE) {
    parent_rank <- ""
    parent <- .getParent(bacdive_data, elements, verbose = verbose)
    if (parent != "") {
        classification <- .getClassification(bacdive_data,
                                             "Name and taxonomic classification",
                                             verbose = verbose)
        for (rank in rev(.getValidRanks()[1:8])) {
            if (classification[[rank]] == parent) {
                parent_rank <- rank
                break
            }
        }
    }
    parent_rank
}

#' Get parent NCBI id
#'
#' @param bacdive_data list of a BacDive data
#' @param verbose logical print messages. Default to FALSE.
#'
#' @return vector of character
#'
#' @importFrom taxizedb name2taxid
#'
#' @examples
#' .getParent(bacdive_data, "Name and taxonomic classification")
.getParentNcbiId <- function(bacdive_data, elements, verbose = FALSE) {
    parent_ncbi_id <- ""
    parent_taxon_name <- .getParent(bacdive_data, elements, verbose)

    tryCatch({
        parent_ncbi_id <- name2taxid(parent_taxon_name)
    }, error = function(e) {
        parent_rank <- .getParentRank(bacdive_data, elements, verbose)
        parent_ncbi_id <- .getNcbiId(bacdive_data, level = parent_rank, verbose)
    })

    if (is.na(parent_ncbi_id))
        parent_ncbi_id <- ""
    parent_ncbi_id
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
