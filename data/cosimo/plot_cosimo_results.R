

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




# Setup
################################################################################

# Plot data
file <- "AqCaHighScen"
plot_data <- function(file){
  
  # Read data
  infile <- paste0(file, "_formatted.csv")
  data <- read.csv(file=file.path(outputdir, infile), as.is=T)
  
  # Format data for plotting
  data_plot <- data %>% 
    filter(food_code=="TOT" & year==2030)
  
  # World countries
  world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% 
    mutate(iso3_use=countrycode(sovereignt, "country.name", "iso3c"))
  
  # Add data to plot
  data_sf <- world %>% 
    left_join(data_plot %>% select(iso3, Diff), by=c("iso3_use"="iso3"))
  
  # Plot data
  g <- ggplot(data_sf) +
    geom_sf(mapping=aes(fill=Diff), lwd=0.15) +
    # Legend
    scale_fill_gradient2(name="Δ kcal/person/day in 2030:\nTargeted investements relative to BAU", midpoint = 0, 
                         low="darkred", high="navy", mid="white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          plot.title=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position = "bottom")
  g
  
  # Export plot
  outfig <- paste0(file, "_2030_calorie_diff_hi_v_bau.png")
  ggsave(g, filename=file.path(plotdir, outfig), 
         width=6.5, height=4, units="in", dpi=600)

}

plot_data(file="AqCaHighScen")
plot_data("AqCaHighLowScen")
plot_data("AqCaHigBaseScen")



