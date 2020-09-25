

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)
library(testthat)

# Directories
datadir <- "data"
outputdir <- "output"

# Read data
data_orig <- read.csv(file=file.path(datadir, "Costello_etal_2016_upsides_proj_by_cntry_group.csv"), as.is=T)

# Read FAO catch
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")


# Build country key
################################################################################

# Read country key
cntry_key_orig <- readxl::read_excel(file.path(datadir, "AglinkCosimo2020countriesregions.xlsx"))

# Format country key 
cntry_key <- cntry_key_orig %>% 
  # Important columns
  select(1:6) %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(iso3=code, 
         group_code=group, 
         country=country_name) %>%
  # Eliminate blank rows
  filter(!is.na(unm49)) %>% 
  # Arrange
  select(iso3, country, unm49, group_code, group_name, continent_region, everything())
  

# Format FAO 2013 data
################################################################################

# Inspect
table(fao_orig$units)
table(fao_orig$area_type)

# ISSCAAPS in upsides
isscaaps_upsides <- sort(unique(data_orig$species_group))
expect_that(sum(!isscaaps_upsides %in% fao_orig$isscaap), is_equivalent_to(0))

# ISSCAAPS to exclude from FAO data
isscaaps_exclude <- c("Blue-whales, fin-whales", 
                      "Sperm-whales, pilot-whales", 
                      "Eared seals, hair seals, walruses", 
                      "Miscellaneous aquatic mammals",
                      "Crocodiles and alligators",
                      "Pearls, mother-of-pearl, shells", 
                      "Corals",
                      "Sponges",
                      "Brown seaweeds",
                      "Red seaweeds",
                      "Green seaweeds",
                      "Miscellaneous aquatic plants")

# ISSCAAPS used by FAO
isscaaps_fao <- fao_orig %>% 
  filter(area_type=="marine" & units=="t" & year==2013 & !isscaap%in%isscaaps_exclude) %>% 
  select(isscaap) %>% 
  unique() %>% arrange() %>% pull()

# ISSCAAPS not in upsides
isscaaps_missing <- isscaaps_fao[!isscaaps_fao %in% isscaaps_upsides]

# FAO 2013
fao2013 <- fao_orig %>% 
  # 2013 marine landings in tons in groups counting towards food (FAO rules)
  filter(area_type=="marine" & units=="t" & year==2013 & !isscaap%in%isscaaps_exclude) %>% 
  # Fill missing ISO3s
  mutate(iso3_orig=ifelse(country_orig=="Channel Islands", "830", iso3_orig),
         iso3_orig=ifelse(country_orig=="Other nei", "NEI", iso3_orig),
         iso3_orig=ifelse(country_orig=="Sudan (former)", "SDN-former", iso3_orig)) %>% 
  # Summarize by country
  group_by(iso3_orig, country_orig, isscaap) %>% 
  summarize(catch_mt_obs=sum(quantity)) %>% 
  ungroup()
  

# Format Costello data for scalar calculations
################################################################################

# Countries missing IS0s
data_orig %>% 
  select(country, iso3) %>% 
  unique() %>% 
  filter(is.na(iso3))

# Format to match FAO IS0s
data1 <- data_orig %>% 
  # Fill missing ISOs
  mutate(iso3=ifelse(country=="Channel Islands", "830", iso3),
         iso3=ifelse(country=="Netherlands Antilles", "ANT", iso3),
         iso3=ifelse(country=="Zanzibar", "TZA", iso3),
         iso3=ifelse(country%in%c("High Seas Tuna and Billfish", "Multinational", "Other nei"), "NEI", iso3)) %>% 
  # Recalculate based on new ISOs then add country
  group_by(policy, iso3, species_group, year) %>% 
  summarize(catch_mt=sum(catch_mt)) %>% 
  ungroup() %>% 
  # Rename and add
  rename(isscaap=species_group) %>% 
  mutate(country=countrycode(iso3, "iso3c", "country.name"), 
         country=ifelse(iso3=="830", "Channel Islands", country),
         country=ifelse(iso3=="ANT", "Netherlands Antilles", country),
         country=ifelse(iso3=="NEI", "Other nei", country)) %>% 
  select(policy, iso3, country, isscaap, year, everything())


# Calculate scalar (simple)
################################################################################

# Totals and scalar
fao13_tot <- sum(fao2013$catch_mt_obs)
costello13_tot <- data1 %>% 
  filter(policy=="BAU" & year==2013) %>% 
  pull(catch_mt) %>% sum()
scalar <- fao13_tot / costello13_tot


# Calculate scalars (complex)
################################################################################

# Build scalar key
scalar_key <- data1 %>% 
  # 2013 values
  filter(policy=="BAU" & year==2013) %>% 
  # Important rows and rename
  select(iso3, country, isscaap, catch_mt) %>% 
  rename(catch_mt_sim=catch_mt) %>% 
  # Fill missing ISO3s for matching to FAO data
  left_join(fao2013 %>% select(-country_orig), by=c("iso3"="iso3_orig", "isscaap")) %>% 
  # Calculate scalar
  mutate(scalar=catch_mt_obs/catch_mt_sim,
         scalar=ifelse(is.na(scalar) | is.infinite(scalar), 0, scalar),
         catch_mt_check=catch_mt_sim*scalar) 
  

# Perform scaling
################################################################################

# Scale data
data2 <- data1 %>% 
  # Add scalar (if doing complex method)
  # left_join(scalar_key %>% select(iso3, isscaap, scalar), by=c("iso3", "isscaap")) %>% 
  mutate(catch_mt_scaled=catch_mt*scalar)

# Build plot to confirm scaling
data2_global <- data2 %>% 
  group_by(policy, year) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T),
            catch_mt_scaled=sum(catch_mt_scaled, na.rm=T)) %>% 
  ungroup()

# Plot global
g <- ggplot(data2_global, aes(x=year, y=catch_mt_scaled/1e6, color=policy)) +
  geom_line() +
  # FAO 2013 reference line
  geom_hline(yintercept=sum(fao2013$catch_mt_obs)/1e6, linetype="dashed") +
  # Labels
  labs(x="Year", y="Catch (millions mt)") +
  scale_y_continuous(lim=c(0,120)) +
  # Theme
  theme_bw()
g


# Format data for FAO model
################################################################################

# Format Costello et al. (2016) data
data3 <- data2 %>% 
  # Reduce to scenario and years of interest
  filter(policy=="Fmsy" & year <= 2030) %>% 
  # Add country group
  left_join(cntry_key %>% select(iso3, group_code, group_name), by="iso3") %>% 
  # Fill in group code and group name for other NEI
  mutate(group_code=ifelse(country=="Other nei", "NEI", group_code),
         group_name=ifelse(country=="Other nei", "Other nei", group_name)) %>% 
  # Summarize by group
  group_by(group_code, group_name, isscaap, year) %>% 
  summarize(catch_mt=sum(catch_mt_scaled)) %>% 
  ungroup()

# Stats
data3_stats <- data3 %>% 
  group_by(group_name, year) %>% 
  summarize(catch_mt=sum(catch_mt))

# Plot check
g <- ggplot(data3_stats, aes(x=year, y=catch_mt/1e6, fill=group_name)) +
  geom_area() +
  labs(x="Year", y="Catch (millions mt)") +
  lims(x=c(2013, 2050), y=c(0,120)) +
  theme_bw()
g

# Export data
################################################################################

# Export
write.csv(data3, file=file.path(outputdir, "Costello_etal_2016_upsides_proj_for_fao_fish_model.csv"), row.names=F)