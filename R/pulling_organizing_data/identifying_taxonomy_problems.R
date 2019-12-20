#Identifying taxonomic problem-species
#Keaton Wilson
#keatonwilson@me.com
#2019-08-06

#packages
library(tidyverse)
library(spocc)

#' Identify names with fewer than \code{min_obs} on GBIF
#' 
#' @param infile   character path to input file with names of candidate species
#' @param outfile  character path to output file with GBIF record counts
#' @param min_obs  numeric minimum number of observations required
#' @param name_col numeric or character indicating column index or name, 
#' respectively, with species name to use in GBIF query
#' 
#' @return data frame with column indicating species with fewer than 
#' \code{min_obs} observations on GBIF; also writes to CSV file
#' 
#' @example 
#' \dontrun{
#' identify_min_obs()
#' }
identify_zero_obs <- function(infile = "./data/declines.csv",
                              outfile = "./data/declines_tax_probs.csv",
                              min_obs = 1,
                              name_col = "species") {
  declines = read_csv(file = infile)

  # Query GBIF for names in declines csv
  declines <- add_column(declines, gbif_occs = NA)
  for (i in 1:nrow(declines)) {
    ocs = occ(query = declines[[i, name_col]], from = "gbif", limit = 1)
    declines$gbif_occs[i] = ocs$gbif$meta$found
  }
  
  # Add column indicating if fewer than min_obs observations were found
  declines = declines %>%
    mutate(tax_prob = ifelse(gbif_occs < min_obs, TRUE, FALSE))
  
  write_csv(x = declines, path = outfile)
  return(declines)
}