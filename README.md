# BacDiveR

*This package is a work in progress.*

A package for downloading data from [BacDive](https://bacdive.dsmz.de/), The
Bacterial Diversity Metadatabase.

It can be used interactively to fetch data from BacDive given a BacDive ID and
a template. An example template is provided in `inst/extdata/template.csv` to
allow some flexibility in adding additional fields without additional
functions if you just need the first entry from a particular data point in
BacDive.

## Requirements

* BacDive credentials

## Install

    devtools::install_github("jwokaty/BacDiveR")
    devtools::load_all()

## Run

BacDiveR needs your BacDive credentials, which you can set as environment
variables

    Sys.setenv("BACDIVE_USERNAME"="my_user", "BACDIVE_PASSWORD"="my_pass")

Create an access object

    # Setting verbose to FALSE to avoid reminder message
    ao <- BacDiveR::authenticate(Sys.getenv("BACDIVE_USERNAME"),
                                 Sys.getenv("BACDIVE_PASSWORD"),
                                 verbose = FALSE)

Look up by BacDive ID

    entry <- BacDiveR::getDataByBacDiveId(ao, id = 132485)
    # Default template returns a list with the following data
    > names(entry)
     [1] "bacdive_id"                 "taxon_name"                
     [3] "ncbi_id"                    "rank"                      
     [5] "parent_taxon_name"          "parent_ncbi_id"            
     [7] "parent_rank"                "sequence_16S_ncbi_id"      
     [9] "sequence_genome_ncbi_id"    "type_strain"               
     [11] "gram_stain"                 "cell_shape"                
     [13] "motility"                   "hemolysis"                 
     [15] "colony_color"               "incubation_period"         
     [17] "cultivation_medium_used"    "culture_temperature_growth"
     [19] "culture_temperature_type"   "culture_temperature"       
     [21] "culture_temperature_range"  "oxygen_tolerance"          
     [23] "spore_formation"            "halophily"                 
     [25] "metabolite_utiilization"    "metabolite_production"     
     [27] "sample_type"                "geographic_location"       
     [29] "country"                    "biosafety_level"           
     [31] "biosafety_level_comment"    "pathogenicity_human"       
     [33] "pathogenicity_animal"

Save a CSV with all BacDive data based on your template to your working
directory

    BacDiveR::getData(access_object, verbose = TRUE)
