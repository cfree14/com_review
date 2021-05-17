

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")


# Build stock key
################################################################################

# Identify most recent assessments
assessments <- assessment %>% 
  # Reduce to most recent
  filter(mostrecent %in% c(999, -1)) %>% 
  # Simplify
  select(stockid, assessid, assessorid, assessmethod)

# All unique? Yes!
anyDuplicated(assessments$stockid)
anyDuplicated(assessments$assessid)

# Build stock key
stocks <- stock %>% 
  # Most recent assessment
  filter(stockid %in% assessments$stockid) %>% 
  # Simplify
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>%  
  # Add assessment info
  left_join(assessments, by="stockid") %>% 
  # Rearrange columns
  select(stockid, stocklong, assessid, assessorid, assessmethod, country, region, area, species, comm_name)

# Inspects
freeR::complete(stocks)
anyDuplicated(stocks$stockid)
anyDuplicated(stocks$assessid)

# Build catch data
################################################################################

# Calculate catch stats
catch_df <- timeseries_values_views %>% 
  # Simplify columns
  select(stockid, year, TC, TL) %>% 
  # Only years with catch data
  filter(!is.na(TC) | !is.na(TL)) %>% 
  # Rename catch columns
  rename(tc=TC, tl=TL) %>% 
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TC, TL), by="stockid") %>% 
  rename(tc_units=TC, tl_units=TL) %>% 
  # Calculate stats
  group_by(stockid) %>% 
  mutate(tc_max=max(tc, na.rm=T),
         tc_prop=tc/tc_max,
         tl_max=max(tl, na.rm=T),
         tl_prop=tl/tl_max) %>% 
  ungroup() %>% 
  # Finalize
  filter(!is.na(tc_prop) | !is.na(tl_prop)) %>% 
  mutate(c_type=ifelse(!is.na(tc_prop), "TC", "TL"),
         c_units=ifelse(!is.na(tc_prop), tc_units, tl_units),
         c_val=ifelse(!is.na(tc_prop), tc, tl),
         c_max=ifelse(!is.na(tc_prop), tc_max, tl_max), 
         c_prop=ifelse(!is.na(tc_prop), tc_prop, tl_prop)) %>% 
  # Arrange
  select(stockid, year, c_type, c_units, c_max, c_val, c_prop) 

# Inspect
str(catch_df)
freeR::complete(catch_df)


# Build biomass data
################################################################################

# Build B0 dataframe
b0_df <- bioparams_values_views %>% 
  # Simplify
  select(stockid, TB0, SSB0) %>% 
  filter(!is.na(TB0) | !is.na(SSB0)) %>% 
  rename(tb0=TB0, ssb0=SSB0) %>% 
  # Add units
  left_join(bioparams_units_views %>% select(stockid, TB0, SSB0), by="stockid") %>%
  rename(tb0_units=TB0, ssb0_units=SSB0) %>% 
  # Arrange
  select(stockid, tb0, tb0_units, ssb0, ssb0_units)

# Build biomass dataframe
b_df <- timeseries_values_views %>% 
  # Simplify
  select(stockid, year, TB, SSB) %>% 
  filter(!is.na(TB) | !is.na(SSB)) %>% 
  rename(tb=TB, ssb=SSB) %>% 
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TB, SSB), by="stockid") %>%
  rename(tb_units=TB, ssb_units=SSB) %>% 
  # Arrange
  select(stockid, year, tb, tb_units, ssb, ssb_units)

# Build B/B0 dataframe
bprop_df <- b_df %>%
  # Add B0 values
  left_join(b0_df) %>% 
  # Compute B/B0
  mutate(tb_prop=tb/tb0,
         ssb_prop=ssb/ssb0) %>% 
  # Select best
  filter(!is.na(tb_prop) | !is.na(ssb_prop)) %>% 
  mutate(b_type=ifelse(!is.na(tb_prop), "TB", "SSB"),
         b_units=ifelse(!is.na(tb_prop), tb_units, ssb_units),
         b0=ifelse(!is.na(tb_prop), tb0, ssb0),
         b_val=ifelse(!is.na(tb_prop), tb, ssb),
         b_prop=ifelse(!is.na(tb_prop), tb_prop, ssb_prop)) %>% 
  # Reduce to 
  select(stockid, year, b_type, b_units, b0, b_val, b_prop)
  
# Inspect
freeR::complete(bprop_df)



# Merge data
################################################################################

# Merge data
data <- catch_df %>% 
  # Merge
  full_join(bprop_df) %>% 
  # Reduce to years with data
  filter(!is.na(c_prop) & !is.na(b_prop)) %>% 
  # Most recent year with data
  group_by(stockid) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Arrange
  arrange(stockid)
  

# Export data
################################################################################



# Plot data
################################################################################

# CMSY-2017


if(cr>=0.8){s_prior <- c(0.4,0.8)}
if(cr<0.8&cr>=0.5){s_prior <- c(0.2,0.6)}
if(cr<0.5&cr>=0.35){s_prior <- c(0.01,0.4)}
if(cr<0.35&cr>=0.15){s_prior <- c(0.01,0.3)}
if(cr<0.15&cr>=0.05){s_prior <- c(0.01,0.2)}
if(cr<0.05){s_prior <- c(0.01,0.1)}
return(s_prior)

cmsy_priors <- matrix(data=c(0.00, 0.00,
                             0.05, 0.00,
                             0.),
                      ncol=2, byrow=T)

# Plot data
g <- ggplot(data, aes(x=c_prop, y=b_prop)) +
  # Plot shading
  geom_polygon()
  # Plot points
  geom_point() +
  # Plot reference line
  geom_hline(yintercept = 1, linetype="dotted") +
  # Labels
  labs(x="Catch ratio\n(recent / maximum catch)",
       y="Biomass ratio\n(recent / unexploited biomass)",
       title="Final depletion") +
  # Theme
  theme_bw()
g


