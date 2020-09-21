
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(grid)
library(gridExtra)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/data/feed_params/processed/"
outputdir <- "output"

# Read data
load(file.path(datadir, "Tacon_Metian_2008_and_2015_fcr_fmfo_data.Rdata"))
data_orig <- tm08_t4
rm(tm15_t1, tm08_t3, tm08_t4)


# Format data
################################################################################

# Plot data
g <- ggplot(data_orig, aes(x=year, y=value)) +
  facet_grid(ingredient ~ group) +
  geom_point() +
  labs(x="Year", y="Percent of feed(%)") +
  theme_bw()
g

# Groups
groups <- sort(unique(data_orig$group))

# Loop through groups
preds <- purrr::map_df(groups, function(x) {
  
  # Projection years  
  pred_yrs <- 2005:2050

  # Subset data
  #########################
  
  # Subset data
  group_i <- x
  fm_data <- data_orig %>% 
    filter(group==group_i & ingredient=="Fishmeal" & year>=2005)
  fo_data <- data_orig %>% 
    filter(group==group_i & ingredient=="Fish oil" & year>=2005)
  
  # Fish meal
  #########################
  
  # Plot data
  plot(value ~ year, fm_data, xlim=c(2000, 2050), ylim=c(0,30),
       main=group_i, xlab="", ylab="% fishmeal")

  # Fit model
  fm_fit <- lm(log(value) ~ year, fm_data)
  fm_a <- exp(coef(fm_fit)[1])
  fm_b <- coef(fm_fit)[2]
  
  # Plot fit
  curve(fm_a*exp(fm_b*x), from=2000, to=2050, n=100, add=T)
  
  # Plot and record prediction
  fm_pred <- as.numeric(exp(predict(fm_fit, data.frame(year=pred_yrs))))
  points(x=pred_yrs, y=fm_pred, pch=16, cex=2)

  
  # Fish oil
  #########################
  
  # Don't project if 0
  if(fo_data$value[fo_data$year==2020]==0){
    
    fo_pred <- rep(0, length(pred_yrs))
    
  }else{
    
    # Plot data
    plot(value ~ year, fo_data, xlim=c(2000, 2050), ylim=c(0,5),
         main=group_i, xlab="", ylab="% fishoil")
    
    # Fit model
    fo_fit <- lm(log(value) ~ year, fo_data)
    fo_a <- exp(coef(fo_fit)[1])
    fo_b <- coef(fo_fit)[2]
    
    # Plot fit
    curve(fo_a*exp(fo_b*x), from=2000, to=2050, n=100, add=T)
    
    # Plot and record prediction
    fo_pred <- as.numeric(exp(predict(fo_fit, data.frame(year=pred_yrs))))
    points(x=pred_yrs, y=fo_pred, pch=16, cex=2)
    
  }
  

  
  # Record
  #########################
  
  # Out
  out_fm <- data.frame(group=group_i,
                       ingredient="Fishmeal",
                       year=pred_yrs,
                       value=fm_pred)
  out_fo <- data.frame(group=group_i,
                       ingredient="Fish oil",
                       year=pred_yrs,
                       value=fo_pred)
  out <- bind_rows(out_fm, out_fo)
  
})
  

# Plot data
################################################################################

# # Plot data
g <- ggplot(data_orig, aes(x=year, y=value)) +
  facet_grid(group ~ ingredient, scales="free_y") +
  geom_point() +
  geom_line(data=preds) +
  labs(x="Year", y="Percent of feed(%)") +
  theme_bw()
g




# Export data
################################################################################

# SImplify
preds_simple <- preds %>% 
  filter(year>=2020)

# Export
write.csv(preds_simple, file=file.path("2020_2050_fmfo_projections.csv"), row.names=F)


