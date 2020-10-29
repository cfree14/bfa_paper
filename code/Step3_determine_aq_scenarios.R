

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
outputdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")

# Read capture data
wc_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")


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



# Build key
################################################################################

# Format trends to add to key
trends3 <- trends_use2 %>% 
  rename(country=country_use, iso3=iso3_use, aq_type=environment, sector=major_group) %>% 
  mutate(aq_type=recode(aq_type,
                     "Freshwater"="inland",
                     "Marine/bracksish"="marine"),
         sector=recode(sector, 
                       "Mollusca"="bivalves",
                       "Pisces"="finfish",
                       "Crustacea"="crustaceans"))

# Calculate medians
trends3_meds <- trends3 %>% 
  group_by(aq_type, sector, trend_perc_catg) %>% 
  summarize(base_mt_min=min(base_mt),
            inv_trend_perc=median(trend_perc))

# Baseline values for countries with no production
trends3_b0s <- trends3_meds %>% 
  filter(trend_perc_catg=="1st quartile") %>% 
  select(-c(inv_trend_perc, trend_perc_catg))

# Build key
cntry_key1 <- wc_orig %>% 
  # Build country list
  group_by(iso3_use, country_use) %>% 
  summarize(country_type=ifelse("marine" %in% area_type, "coastal", "inland")) %>% 
  # Format key
  rename(iso3=iso3_use,
         country=country_use) %>% 
  mutate(iso3=ifelse(country=="Other nei", "NEI", iso3),
         iso3=ifelse(country=="Channel Islands", "830", iso3))

# Percent growth cap 
perc_cap <- 10

# Build sector key
cntry_aq_key <- expand_grid(cntry_key1, aq_type=c("inland", "marine"), sector=c("bivalves", "finfish", "crustaceans")) %>% 
  # Remove marine AQ from inland countries
  filter(!(country_type=="inland" & aq_type=="marine")) %>% 
  # Add base value and current trend
  left_join(trends3 %>% select(-nyears)) %>% 
  rename(curr_trend_perc=trend_perc,
         curr_trend_catg=trend_perc_catg) %>% 
  # Add trend category with investment
  mutate(curr_trend_catg=as.character(curr_trend_catg),
         inv_trend_catg=recode(curr_trend_catg, 
                               "Decreasing"="1st quartile",
                               "1st quartile"="2nd quartile",
                               "2nd quartile"="2nd quartile",
                               "3rd quartile"="2nd quartile",
                               "4th quartile"="2nd quartile"),
         inv_trend_catg=ifelse(is.na(inv_trend_catg), "1st quartile", inv_trend_catg)) %>% 
  # Add trend value with investment (make sure 4th quartile growth maintained)
  left_join(trends3_meds %>% select(-base_mt_min), by=c("aq_type", "sector", "inv_trend_catg"="trend_perc_catg")) %>% 
  mutate(inv_trend_perc=ifelse(curr_trend_catg=="2nd quartile" & !is.na(curr_trend_catg), curr_trend_perc, inv_trend_perc)) %>% 
  # CAP TREND VALUES
  mutate(inv_trend_perc=pmin(inv_trend_perc, perc_cap)) %>% 
  # Add base production for countries without historic production
  left_join(trends3_b0s) %>% 
  mutate(prod2018_mt=ifelse(!is.na(base_mt), base_mt*exp(inv_trend_perc/100*1), base_mt_min)) %>% 
  # Remove inland molluscs
  mutate(sector_long=paste(aq_type, sector, sep="-")) %>% 
  filter(sector_long!="inland-bivalves") %>% 
  # Arrange
  select(-base_mt_min) %>% 
  select(iso3, country, country_type, aq_type, sector, sector_long, everything()) %>% 
  # Calculate 2030 volume
  mutate(prod2030_mt=prod2018_mt*exp(inv_trend_perc/100*(2030-2018)))

# Did any get capped?
sum(cntry_aq_key$inv_trend_perc==perc_cap)

# Check out totals
prod2017 <- sum(cntry_aq_key$base_mt, na.rm=T)/1e6
sum(cntry_aq_key$prod2018_mt)/1e6
prod2030 <- sum(cntry_aq_key$prod2030_mt)/1e6
  
prod2030 - prod2017

# Project forward
################################################################################

# Loop through and project
proj <- purrr::map_df(1:nrow(cntry_aq_key), function(x) {
  
  # Row
  row_do <- cntry_aq_key[x,]
  
  # Calculate production volumes
  years <- 2018:2030
  t_vals <- years-min(years)
  prod_mt <- row_do$prod2018_mt * exp(row_do$inv_trend_perc/100*t_vals)
  
  # Build production
  df <- row_do %>% 
    select(iso3:sector_long) %>% 
    expand_grid(year=years) %>% 
    mutate(prod_mt=prod_mt) %>% 
    mutate(source="Projected") %>% 
    select(source, everything())
    
})

# Build historical data
hist <- data_orig %>% 
  # Groups of interest
  filter(major_group%in%c("Crustacea", "Mollusca", "Pisces")) %>% 
  rename(sector=major_group) %>% 
  mutate(sector=recode(sector,
                       "Mollusca"="bivalves",
                       "Pisces"="finfish",
                       "Crustacea"="crustaceans")) %>% 
  # Rename environment
  mutate(environment=recode(environment, 
                            "Freshwater"="inland",
                            "Marine"="marine",
                            "Brackishwater"="marine")) %>% 
  rename(aq_type=environment, iso3=iso3_use, country=country_use) %>% 
  group_by(iso3, country, aq_type, sector, year) %>% 
  summarize(prod_mt=sum(quantity_mt)) %>% 
  # Add source
  mutate(source="Historical",
         sector_long=paste(aq_type, sector, sep="-"))

# Merge historical and projected
data <- bind_rows(hist, proj)

# Annual stats
stats <- data %>% 
  group_by(source, aq_type, sector, sector_long, year) %>% 
  summarize(prod_mt=sum(prod_mt))

# Plot
g <- ggplot(stats, aes(x=year, y=prod_mt/1e6, fill=sector, alpha=aq_type)) +
  geom_area() +
  geom_vline(xintercept=2017) +
  # Labels
  labs(x="", y="Production (millions mt)") +
  scale_x_continuous(breaks=seq(1950, 2030, 10)) +
  # Legends
  scale_alpha_manual(name="Environment", values=c(1,0.7)) +
  scale_fill_discrete(name="Taxanomic group") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        plot.title=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_aquaculture_growth_global.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


# Project forward
################################################################################

# Build output
output <- proj %>% 
  # Merge Zanzibar and Tanzania
  mutate(iso3=ifelse(iso3=="EAZ", "TZA", iso3),
         country=ifelse(country=="Zanzibar", "Tanzania", country)) %>% 
  group_by(iso3, country, country_type, aq_type, sector, sector_long, year) %>% 
  summarize(prod_mt=sum(prod_mt)) %>% 
  ungroup() %>% 
  # Add country group
  left_join(cntry_key %>% select(iso3, group_code, group_name), by="iso3") %>% 
    # Fill in group code and group name for other NEI
    mutate(group_code=ifelse(country=="Other nei", "NEI", group_code),
           group_name=ifelse(country=="Other nei", "Other nei", group_name)) %>% 
  # Arrange
  select(group_code, group_name, iso3, country, everything()) %>% 
  # Erase countries not assigned to group
  filter(!is.na(group_code))

# Summarize by group
output_group <- output %>% 
  group_by(group_code, group_name, aq_type, sector, sector_long, year) %>% 
  summarize(prod_mt=sum(prod_mt)) %>% 
  ungroup()
  
# Complete?
freeR::complete(proj)

# Export
write.csv(output, file=file.path(outputdir, "2018_2030_aq_proj_for_fao_fish_model_country.csv"), row.names=F)
write.csv(output_group, file=file.path(outputdir, "2018_2030_aq_proj_for_fao_fish_model_country_group.csv"), row.names=F)


