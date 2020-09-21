

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision/outputs/"
outputdir <- "data"

# Read data
data_orig <- readRDS(file.path(datadir, "subset_supply_curves.rds"))


# Setup
################################################################################

# Inspect data
colnames(data_orig)
table(data_orig$scenario)
table(data_orig$scenario2)
table(data_orig$inl_scen)
table(data_orig$cap_scen)
table(data_orig$aq_spp)
table(data_orig$aq_scen)
table(data_orig$aq_cost_scalar)
table(data_orig$sector)


# Reduce to BAU scenario
data_bau <- data_orig %>% 
  # Reduce to marine sectors
  filter(sector %in% c("Capture fisheries", "Bivalve aquaculture", "Finfish aquaculture")) %>% 
  # Filter to AQ BAU scenario
  filter(aq_scen=="Scenario 3 - byproducts + directed") %>% 
  # Reduce columns
  select(sector:meat_yr) %>% 
  # Format for clarity
  rename(price_usd_mt=price) %>% 
  mutate(sector=recode(sector, 
                       "Capture fisheries"="Marine fisheries",
                       "Bivalve aquaculture"="Bivalve mariculture",
                       "Finfish aquaculture"="Finfish mariculture"))


# Plot
g <- ggplot(data_bau, mapping=aes(x=meat_yr/1e6, y=price_usd_mt, color=sector)) +
  facet_wrap(~sector, scales="free_x") +
  labs(x="Production (millions mt per year)", y="Price (USD/mt)") +
  geom_line() +
  theme_bw()
g 

# Export
write.csv(data_bau, file=file.path(outputdir, "Costello_etal_2020_bau_supply_curves_global.csv"))



