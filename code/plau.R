

# Package
remotes::install_github("danovando/sraplus")

# Package
library(ggplot2)
library(tidyr)
library(dplyr)
library(tmbstan)
library(sraplus)
Sys.unsetenv("PKG_CXXFLAGS")


# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")

# Example
example_taxa <- "gadus morhua"

data(cod)

head(cod)
catch_only_driors <- format_driors(
  taxa = example_taxa,
  catch = cod$catch,
  years = cod$year,
  use_heuristics = TRUE
)
plot_driors(catch_only_driors)
sfs <- purrr::safely(fit_sraplus)

catch_only_fit <- fit_sraplus(driors = catch_only_driors,
                              engine = "sir",
                              draws = 1e5,
                              n_keep = 4000,
                              estimate_proc_error = TRUE, 
                              estimate_shape = TRUE,
                              max_time = Inf,
                              tune_prior_predictive = FALSE)

sraplus::plot_sraplus(catch_only = catch_only_fit, years = catch_only_driors$years)