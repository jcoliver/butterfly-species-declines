#Pulling data for canidate species
#Keaton Wilson
#keatonwilson@me.com
#2019-08-13

# packages
library(tidyverse)
library(spocc)
library(stringr)

#' Observation data for candidate species
#' 
#' @param infile    character path to input CSV file of candidate species
#' @param outfile   character path to write observation data of candidate 
#' species
#' @param namesfile character path to file with all species, in case some need 
#' to be renamed in order to match other taxonomies (i.e. iNaturalist, GBIF)
#' @param top_num  integer indicating how many species to draw from each 
#' quartile
#' 
#' @return CSV of candidate species
#' @example 
#' \dontrun{
#' candidate_obs()
#' }
candidate_obs = function(infile = "./data/candidate_species.csv",
                         outfile = "./data/candidate_occurences.csv",
                         namesfile = "./data/declines.csv") {
  
  # Load in candidate species names
  candidates = read_csv(file = infile)
  
  # Also load in the full species list to check whether any of the candidate
  # species require renaming
  declines = read_csv(file = namesfile)
  
  # Join two species names data and extract columns of interest
  candidates_joined = candidates %>%
    left_join(declines, by = "species") %>%
    select(species, gbif_name, mean = mean.x, gbif_occs, quartile, tax_prob)
  
  # Extract GBIF names of species to use
  names = candidates_joined$gbif_name
  
  # Perform query for each element in names
  butterfly_data = butt_obs(names)

  # Remove duplicate observations
  butterfly_data_clean = butterfly_data %>%
    mutate(longitude = as.numeric(longitude), 
           latitude = as.numeric(latitude),
           name = word(name, 1, 2)) %>%
    distinct(longitude, latitude, date, name) 
  
  # Write results to output file
  write_csv(x = butterfly_data_clean, path = outfile)
}

################################################################################
#' Query remote databases for species observation data
#' 
#' @param names        character vector of names to use in query of remote data 
#' sources
#' @param datasources  character vector of sources to query; passed to 
#' \code{from} parameter of \code{spocc::occ}
#' @param obs_limit    numeric maximum number of records to return for each 
#' species in \code{names}
#' @param gbifopts     list of named elements of arguments for GBIF query; see 
#' \link{rgbif::occ_search()}
#' @param geometry     character or numeric indicating geographic bounds of 
#' query; see \link{spocc::occ} 
#' 
#' @return data frame of observation records with geographic coordinates
#' 
#' @example 
#' \dontrun{
#' x = c("Pieris rapae", "Vanessa virginiensis")
#' observations = butt_obs(names = x)
#' }
butt_obs = function(names, datasources = c("gbif", "inat"), obs_limit = 10000,
                    gbifopts = list(continent = "north_america"), 
                    geometry = c(-140, 20, -90, 60)){
  
  # Data frame to hold results.
  obs_df = data.frame()
  
  # TODO: Change to vectored approach with *apply
  for (i in 1:length(names)){
    sub = occ(query = names[i], 
              from = datasources, 
              limit = obs_limit, 
              has_coords = TRUE, 
              gbifopts = gbifopts, 
              geometry = geometry)
    
    # Convert to data frame
    sub_df = occ2df(sub)
    
    # Add to results data frame
    obs_df = bind_rows(obs_df, sub_df)
  }
  return(obs_df)
}
