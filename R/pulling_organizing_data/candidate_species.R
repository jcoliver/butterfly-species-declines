#Identifying candidate species
#Keaton Wilson
#keatonwilson@me.com
#2019-08-06

require(tidyverse)
require(spocc)

#' Identify candidate species for each quartile of decline data
#' 
#' @param infile   character path to input CSV file
#' @param outfile  character path to write output CSV of candidate species
#' @param top_num  integer indicating how many species to draw from each 
#' quartile
#' 
#' @return CSV of candidate species
#' 
#' @examples 
#' \dontrun{
#' candidate_species()
#' }
candidate_species <- function(infile = "./data/declines_tax_probs.csv", 
                              outfile = "./data/candidate_species.csv",
                              top_num = 5) {
  declines = read_csv(file = infile)

  # Split species into decline quartiles and then taking the top n species 
  # by occurences from each quartile
  candidate_species = declines %>%
    mutate(quartile = ntile(mean, 4)) %>%
    group_by(quartile) %>%
    top_n(n = top_num, wt = gbif_occs)
  
  write_csv(x = candidate_species, path = outfile)
}