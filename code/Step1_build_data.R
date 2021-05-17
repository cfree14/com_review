

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

# Read SP fits
sp_fits <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/trawl_impacts/approach2/data/RAM_sp_model_fits.csv", as.is=T)


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

# Check names
freeR::check_names(stocks$species)

# Inspects
freeR::complete(stocks)
anyDuplicated(stocks$stockid)
anyDuplicated(stocks$assessid)

# Get resilience
spp_info <- freeR::fishbase(dataset="stocks", species=sort(unique(stocks$species)))
res_info <- spp_info %>%
  janitor::clean_names("snake") %>% 
  select(species, resilience) %>% 
  unique() %>% 
  arrange(species) %>% 
  filter(!is.na(resilience)) %>% 
  group_by(species) %>% 
  slice(1) %>% 
  ungroup()

# Inspect resilience values
table(res_info$resilience)


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

# Build final B/B0 dataframe
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


# Final depletion
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
saveRDS(data, file.path(datadir, "depletion_final.Rds"))

# Plot data
g <- ggplot(data, aes(x=c_prop, y=b_prop)) +
  # Plot points
  geom_point() +
  # Plot reference line
  geom_hline(yintercept = 1, linetype="dotted") +
  # Legend
  scale_fill_discrete(name="Method") +
  # Labels
  labs(x="Catch ratio\n(recent / maximum catch)",
       y="Biomass ratio\n(recent / unexploited biomass)",
       title="Final depletion",
       subtitle="Default priors vs. observations") +
  # Theme
  theme_bw()
g

# Initial depletion
################################################################################

# Build 
data1 <- catch_df %>% 
  # Merge
  full_join(bprop_df) %>% 
  # Reduce to years with data
  filter(!is.na(c_val) & !is.na(b_prop)) %>% 
  # Most recent year with data
  group_by(stockid) %>% 
  arrange(year) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Arrange
  arrange(stockid)

# Export data
saveRDS(data1, file.path(datadir, "depletion_initial.Rds"))

# Plot data
g <- ggplot(data1, mapping=aes(x=year, y=b_prop)) +
  geom_point() +
  # Plot reference line
  geom_hline(yintercept = 1, linetype="dotted") +
  # Labels
  labs(x="", y="Biomass ratio\n(recent / unexploited biomass)") +
  # Theme
  theme_bw()
g

# Intrinsic growth rate
################################################################################

# Build resilience data
data4 <- sp_fits %>%
  # Add species
  left_join(stocks %>% select(stockid, species)) %>% 
  # Add resilience
  left_join(res_info %>% select(species, resilience)) %>% 
  # Simplify
  select(stockid, r, resilience) %>% 
  # Order resilience
  mutate(resilience=factor(resilience, levels=c("Very low", "Low", "Medium", "High"))) %>% 
  # Remove NA
  filter(!is.na(resilience))

# Export data
saveRDS(data4, file.path(datadir, "growth_rate.Rds"))

# Plot growth rate
g <- ggplot(data4, aes(x=resilience, y=r)) +
  geom_boxplot() +
  # Labels
  labs(x="Resilience", y="Intrinsic growth rate, r") +
  # Theme
  theme_bw()
g

# Carrying capacity
################################################################################

data3 <- catch_df %>% 
  select(stockid, c_type, c_units, c_max) %>% 
  unique() %>% 
  # TC over TL
  group_by(stockid) %>% 
  arrange(stockid, c_type) %>% 
  slice(1) %>% 
  # Add B0
  left_join(b0_df) %>% 
  # Which to use?
  mutate(b0=ifelse(!is.na(tb0), tb0, ssb0),
         b0_units=ifelse(!is.na(tb0), tb0_units, ssb0_units),
         b0_type=ifelse(!is.na(tb0), "TB", "SSB")) %>% 
  # Reduce
  filter(!is.na(b0) & !is.na(c_max)) %>% 
  # Simplify
  select(stockid, c_type, c_units, c_max, b0_type, b0_units, b0) %>% 
  # Scalar
  mutate(scalar=b0/c_max) %>% 
  # Eliminate non-matching units
  filter(b0_units==c_units) %>% 
  # Add species
  left_join(stocks %>% select(stockid, species)) %>% 
  # Add resilience
  left_join(res_info %>% select(species, resilience)) %>% 
  # Arrange
  select(stockid, species, resilience, everything())
 
# Check
sum(data3$c_units!=data3$b0_units)

# Export data
saveRDS(data3, file.path(datadir, "carrying_capacity.Rds"))


