

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)


# Directories
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/data/upsides/"
outputdir <- "data"

# Read data
data_orig <- read.csv(file.path(datadir, "Unlumped_ProjectionData.csv"), as.is=T)

# Format data
################################################################################

# Summarize globally
data_global <- data_orig %>% 
  # Format names
  janitor::clean_names("snake") %>% 
  # Look at forecast impacts
  filter(policy!="Historical") %>% 
  # Look at policy impacts on all stocks
  filter(scenario == "All Stocks") %>% 
  # Summarize by policy, country, species group, year
  group_by(policy, year) %>% 
  summarize(biomass_mt=sum(biomass),
            catch_mt=sum(catch),
            profits_usd=sum(profits),
            bbmsy_avg=mean(bv_bmsy),
            ffmsy_avg=mean(fv_fmsy))


# Plot check
g <- ggplot(data_global, aes(x=year, y=catch_mt/1e6, color=policy)) +
  geom_line() +
  theme_bw()
g

# Summarize by country
data_cntry <- data_orig %>% 
  # Format names
  janitor::clean_names("snake") %>% 
  # Look at forecast impacts
  filter(policy!="Historical") %>% 
  # Look at policy impacts on all stocks
  filter(scenario =="All Stocks") %>% 
  # Summarize by policy, country, species group, year
  group_by(policy, country, species_cat_name, year) %>% 
  summarize(biomass_mt=sum(biomass),
            catch_mt=sum(catch),
            profits_usd=sum(profits),
            bbmsy_avg=mean(bv_bmsy),
            ffmsy_avg=mean(fv_fmsy)) %>% 
  ungroup() %>% 
  # Add country info
  mutate(country=recode(country, "Micronesia"="Federated States of Micronesia"),
         iso3=countrycode(country, "country.name", "iso3c"),
         country_new=countrycode(iso3, "iso3c", "country.name"),
         country=ifelse(!is.na(country_new), country_new, country)) %>% 
  # Arrange columns
  select(policy, country, iso3, species_cat_name, year, biomass_mt:ffmsy_avg) %>% 
  rename(species_group=species_cat_name)

# Export data
write.csv(data_cntry, file=file.path(outputdir, "Costello_etal_2016_upsides_proj_by_cntry_group.csv"))



