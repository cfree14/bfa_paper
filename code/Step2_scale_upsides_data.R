

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


# Format FAO 2012 data
################################################################################

# Inspect
table(fao_orig$units)
table(fao_orig$area_type)

# ISSCAAPS in upsides
isscaaps_upsides <- sort(unique(data_orig$species_group))
expect_that(sum(!isscaaps_upsides %in% fao_orig$isscaap), is_equivalent_to(0))

# Summ
data_global <- data_orig %>% 
  group_by(policy, year) %>% 
  summarize(catch_mt=sum(catch_mt))

# FAO 2012
fao2012 <- fao_orig %>% 
  # 2012 marine landings in tons in groups described by upsides
  filter(area_type=="marine" & units=="t" & isscaap %in% isscaaps_upsides & year==2012)
  
# Plot global
g <- ggplot(data_global, aes(x=year, y=catch_mt/1e6, color=policy)) +
  geom_line() +
  # FAO 2012 reference line
  geom_hline(yintercept=sum(fao2012$quantity)/1e6, linetype="dashed") +
  # Labels
  labs(x="Year", y="Catch (millions mt)") +
  scale_y_continuous(lim=c(0,120)) +
  # Theme
  theme_bw()
g

# Export simple
data_simple <- data_orig %>% 
  filter(policy=="Fmsy")

# Export
write.csv(data_simple, file=file.path(outputdir, "Costello_etal_2016_upsides_proj_by_cntry_group.csv"), row.names=F)
