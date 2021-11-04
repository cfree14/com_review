

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

# Setup
################################################################################

# Number of assessments
n_distinct(assessment$assessid)
n_distinct(assessment$stockid)

# Number of stocks
n_distinct(stock$stockid)
