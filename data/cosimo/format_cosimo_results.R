

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)
library(testthat)

# Directories
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"


# Helper functions
################################################################################


strsplit_extract <- function(x, split, which){
  
  split_escaped <- paste0("\\", split)
  vals <- purrr::map(x,  function(x){
    split_val <- strsplit(x, split_escaped)
    extracted_val <- split_val[[1]][which]
  })
  
  vals <- vals %>% unlist() %>% as.character()
  
  return(vals)
  
}

# Format function
################################################################################

# Read data
file <- "AqCaHighScen"
format_data <- function(file){
  
  # Read data
  infile <- paste0(file, ".csv")
  data_orig <- read.csv(file=file.path(inputdir, infile), as.is=T, skip=1)
  
  # Format data
  data <- data_orig %>% 
    # Gather results
    gather(key="year", value="value", 5:ncol(.)) %>% 
    # Format year
    mutate(year=year %>% gsub("X|A", "", .) %>% as.numeric()) %>%
    # Rename columns
    rename(long_code=OUTPUT.0, description=Comment, location=Location, type=Type) %>% 
    # Eliminate useless columns: location (only one value), description (repetitive?)
    select(-c(location, description)) %>% 
    # Extract useful stuff from code
    mutate(iso3 = strsplit_extract(x=long_code, split="_", which=1),
           food_code = strsplit_extract(x=long_code, split="_", which=2),
           code3 = strsplit_extract(x=long_code, split="_", which=3),
           code4 = strsplit_extract(x=long_code, split="..", which=2),
           code5 = strsplit_extract(x=long_code, split="..", which=3),
           metric = strsplit_extract(x=long_code, split="..", which=4)) %>% 
    mutate(code3=strsplit_extract(x=code3, split="..", which=1)) %>% 
    # Arrange columns
    select(long_code, iso3,  food_code, code3, code4, code5, metric, year, type, value, everything()) %>%
    # Spread 
    select(iso3, food_code, code3, code4, code5, year, metric, value) %>%
    spread(key="metric", value="value") %>% 
    # Add country
    mutate(country=countrycode(iso3, "iso3c", "country.name"),
           country=ifelse(iso3=="EUN", "E27 Countries", country)) %>% 
    select(iso3, country, everything()) %>% 
    # Remove useless columns
    select(-c(code3, code4, code5))
  
  # Inspect data
  str(data)
  freeR::complete(data)
  
  # Export data
  outfile <- paste0(file, "_formatted.csv")
  write.csv(data, file=file.path(outputdir, outfile), row.names=F)
  
}

# Format files
format_data("AqCaHighScen")
format_data("AqCaHighLowScen")
format_data("AqCaHigBaseScen")

# Format other data
################################################################################

# Read data
data_orig <- read.csv(file.path(inputdir, "FoodConsBase.csv"), as.is=T, skip=1)

# Format data
data <- data_orig %>% 
  # Remove emoty column
  select(-X) %>% 
  # Gather years
  gather(key="year", value="value", 5:ncol(.)) %>% 
  # Format year 
  mutate(year=year %>% gsub("X|A", "", .) %>% as.numeric()) %>%
  # Rename columns
  rename(long_code=OUTPUT.0, description=Comment, location=Location, type=Type) %>% 
  # Remove useless columns
  select(-c(location, type)) %>% 
  # Extract useful stuff from code
  mutate(iso3 = strsplit_extract(x=long_code, split="_", which=1),
         food_code = strsplit_extract(x=long_code, split="_", which=2),
         code3=strsplit_extract(x=long_code, split="_", which=3),
         code3a=strsplit_extract(x=code3, split="..", which=1),
         code3b=strsplit_extract(x=code3, split="..", which=2)) %>% 
  # Arrange
  select(iso3, food_code, code3, code3a, code3b, year, everything()) %>% 
  arrange(iso3, food_code, year) %>% 
  # Remove useless columns
  select(-c(code3, code3a, code3b, long_code, description))

# Inspect data
str(data)
# table(data$location)
# table(data$type)

