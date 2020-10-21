

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")


# Calculate recent trends
################################################################################

# Inspect data
range(data_orig$year)
table(data_orig$environment)
table(data_orig$major_group)

# Last 10 years
length(2008:2017)

# Build trends
ts <- data_orig %>% 
  # Reduce to recent years
  filter(year>=2008) %>% 
  # Recode environment
  mutate(environment=recode(environment, 
                            "Marine"="Marine/bracksish",
                            "Brackishwater"="Marine/bracksish")) %>% 
  # Summarize production by country, environment, group, year
  group_by(country_use, iso3_use, environment, major_group, year) %>% 
  summarize(quantity_mt=sum(quantity_mt)) %>% 
  ungroup() %>% 
  # Remove years with 0 production
  filter(quantity_mt>0)

# Build key
trends <- ts %>% 
  # Setup trend key
  group_by(country_use, iso3_use, environment, major_group) %>% 
  summarize(nyears=n(),
            base_mt=quantity_mt[year==2017]) %>% 
  ungroup() %>% 
  # Add columns 
  mutate(trend_perc=NA) %>% 
  # Remove combos with fewer than 5 data points
  filter(nyears>=5)

# Loop through key and perform regressions
for(i in 1:nrow(trends)){
  
  # Key
  iso3_do <- trends$iso3_use[i]
  env_do <- trends$environment[i]
  group_do <- trends$major_group[i]
  
  # Subset time series
  ts1 <- ts %>% 
    filter(iso3_use==iso3_do & environment==env_do & major_group==group_do)
  
  # Fit exponential regression
  expfit <- lm(log(quantity_mt)~year, ts1)
  trend_perc <- coef(expfit)[[2]] * 100
  trends$trend_perc[i] <- trend_perc
  
}


# Visualize
################################################################################

# Reduce to used trends
trends_use <- trends %>% 
  filter(major_group%in%c("Crustacea", "Mollusca", "Pisces"))

# Trend use key
trends_use_key <- trends_use %>% 
  select(environment, major_group) %>% 
  unique() %>% 
  as.tibble() %>% 
  mutate(quantiles=NA)

# Loop through environment-groups and calculate quantiles
for(i in 1:nrow(trends_use_key)){
  
  # Subset data
  env_do <- trends_use_key$environment[i]
  group_do <- trends_use_key$major_group[i]
  quants <- trends_use %>% 
    filter(environment==env_do, major_group==group_do & trend_perc>=0) %>% 
    pull(trend_perc) %>% 
    quantile(., probs=c(0.25,0.5,0.75))

  # Classify using these quantile
  trends_use1 <- trends_use %>% 
    filter(environment==env_do, major_group==group_do) %>% 
    mutate(trend_perc_catg=cut(trend_perc, breaks = c(-Inf, 0, quants, Inf), right=T,
           labels=c("Decreasing", "1st quartile", "2nd quartile", "3rd quartile", "4th quartile")))
  
  # Merge results
  if(i==1){trends_use2 <- trends_use1}else{trends_use2 <- rbind(trends_use1, trends_use2)}
  
}

# Trends
g <- ggplot(trends_use2, aes(x=trend_perc, fill=trend_perc_catg)) +
  facet_grid(environment ~ major_group, scales="free_y") +
  geom_histogram(binwidth=2, closed="right", boundary=0) +
  geom_vline(xintercept=0, lty=1, lwd=0.2) +
  # Axis
  scale_x_continuous(breaks=seq(-100,100,25)) +
  # Labels
  labs(x="Annual percent change\nin production (2008-2017)", y="Number of countries") +
  scale_fill_manual(name="Trend category", values=c("darkred", RColorBrewer::brewer.pal(4,"Blues"))) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        plot.title=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_aquaculture_growth_scenarios.png"), 
       width=6.5, height=4, units="in", dpi=600)






