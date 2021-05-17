

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
plotdir <- "figures"

# Read data
sat2 <- readRDS(file.path(datadir, "depletion_final.Rds"))
sat1 <- readRDS(file.path(datadir, "depletion_initial.Rds"))
k <- readRDS(file.path(datadir, "carrying_capacity.Rds"))
r <- readRDS(file.path(datadir, "growth_rate.Rds"))

# Read CMSY final depletion priors
sat2_priors <- readxl::read_excel(file.path(datadir, "depletion_priors.xlsx"), sheet="Final") %>%
  # Eliminate CMSY-v1
  filter(method!="CMSY-v1") %>% 
  # Rename CMSY-v2
  mutate(method=recode(method, "CMSY-v2"="CMSY"))

# Read CMSY initial depletion priors
sat1_priors <- readxl::read_excel(file.path(datadir, "depletion_priors.xlsx"), sheet="Initial") %>%
  mutate(method=factor(method, levels=c("Catch-MSY", "CMSY")))

# Read CMSY r priors
r_priors <- readxl::read_excel(file.path(datadir, "r_priors.xlsx")) %>% 
  mutate(resilience=factor(resilience, levels=c("Very low", "Low", "Medium", "High"))) %>% 
  mutate(xpos=as.numeric(resilience), 
         xpos=ifelse(method=="Catch-MSY", xpos-0.5, xpos-0.45))

# Build CMSY K priors
k_priors <- tibble(method="Catch-MSY",
                   k_lo=1,
                   k_hi=100) %>% 
  mutate(method=factor(method, levels=c("Catch-MSY", "CMSY")))

# Expand K data
k_all <- k %>% 
  mutate(resilience="All stocks")
k_use <- bind_rows(k, k_all) %>% 
  filter(!is.na(resilience)) %>% 
  mutate(resilience=factor(resilience, 
                           levels=c("All stocks", "Very low", "Low", "Medium", "High")))


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  legend.text=element_text(size=7),
                  legend.title=element_blank(),
                  strip.text=element_text(size=9),
                  plot.title=element_text(size=10),
                  plot.tag = element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position="bottom")

# Plot data
g1 <- ggplot(sat2, aes(x=c_prop, y=b_prop)) +
  # Plot shading
  geom_polygon(data=sat2_priors, mapping=aes(x=cr, y=br, fill=method), alpha=0.6) +
  # Plot points
  geom_point() +
  # Plot reference line
  geom_hline(yintercept = 1, linetype="dotted") +
  # Legend
  scale_fill_discrete(name="") +
  # Labels
  labs(x="Catch ratio\n(recent / maximum catch)",
       y="Depletion\n(recent / unexploited biomass)",
       title="Final depletion",
       tag="A") +
  scale_x_continuous(breaks=seq(0, 1, 0.1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.key.size=unit(0.3, "cm"))
g1

# Plot initial depletion
g2 <- ggplot(sat1, mapping=aes(x=year, y=b_prop)) +
  # Plot shading
  geom_polygon(data=sat1_priors, mapping=aes(x=year, y=br, fill=method),
               alpha=0.6, show.legend=F) +
  # Plot points
  geom_point() +
  # Plot reference line
  geom_hline(yintercept = 1, linetype="dotted") +
  # Legend
  scale_fill_discrete(name="Method", drop=F) +
  # Labels
  labs(x="Year\n ", 
       y="Depletion\n(recent / unexploited biomass)",
       title="Initial depletion",
       tag="B") +
  scale_x_continuous(breaks=seq(1880, 2000, 20)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot growth rate
g3 <- ggplot(r, aes(x=resilience, y=r)) +
  geom_boxplot() +
  # Plot priors
  geom_segment(data=r_priors,
               mapping=aes(y=r_lo, yend=r_hi, 
                           x=xpos, xend=xpos, 
                           color=method), show.legend = F) +
  # Labels
  labs(x="Resilience", 
       y="Intrinsic growth rate",
       title="Intrinsic growth rate, r",
       tag="C") +
  # Theme
  theme_bw() + my_theme
g3

# Plot carrying capacity
g4 <- ggplot(k_use, mapping=aes(x=resilience, y=scalar)) +
  geom_boxplot() +
  # Add prior
  geom_segment(data=k_priors, 
               mapping=aes(x=0.52, xend=0.52, y=k_lo, yend=k_hi, color=method),
               show.legend=F) +
  # Labels
  labs(x="Resilience", 
       y="B0 / maximum catch",
       title="Carrying capacity, K",
       tag="D") +
  # Axis
  scale_y_continuous(trans="log2", 
                     breaks=c(0.1, 0.2, 0.5, 
                              1, 2, 5, 
                              10, 20, 50, 100), lim=c(NA, 100)) +
  theme_bw() + my_theme
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_cmsy_prior_performance.png"), 
       width=6.5, height=5, units="in", dpi=600)

