

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/saup_ssp/Global_stock-status v48-0"
plotdir <- "figures"

# Read data
# From here: http://www.seaaroundus.org/data/#/global/stock-status
data_orig <- read.csv(file=file.path(datadir, "Global_stock-status v48-0.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(dataset=Data_Set, year=Year) %>% 
  # Gather
  gather(key="status", value="proportion", 3:ncol(.)) %>% 
  # Recode status
  mutate(status=recode(status, 
                       "Over.exploited"="Overexploited"),
         status=factor(status, levels=c("Collapsed", "Overexploited", "Exploited", "Developing", "Rebuilding"))) %>%
  # Recode dataset
  mutate(dataset=recode(dataset, "css"="Percent of catch", "nss"="Percent of stocks"))



# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.key.size = unit(0.5, "cm"))

# Plot data
g <- ggplot(data, aes(x=year, y=proportion, fill=status)) +
  facet_wrap(~dataset) +
  geom_area() +
  # Labels
  labs(x="Year", y="Percentage (%)") +
  # Legend
  scale_fill_manual(name="Stock status", values=c("#b42726", "#ff8e05", "#f4dfb4", "#9ccf36", "#076604")) +
  # Axis
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_saup_stock_status_plot.png"), 
       width=6.5, height=2.5, units="in", dpi=600)
