

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
plotdir <- "figures"

# Final saturation data
sat2 <- readRDS(file.path(datadir, "depletion_final.Rds"))  %>% 
  # Mark whether prior is correct
  mutate(true_yn1 = (c_prop>=0.5 & b_prop>=0.4 & b_prop<=0.7) | 
           (c_prop<0.5 & b_prop>=0.1 & b_prop<=0.5)) %>% 
  mutate(true_yn2 = (c_prop>=0.7 & b_prop>=0.5 & b_prop<=0.9) | 
           (c_prop>=0.3 & c_prop<=0.7 & b_prop>=0.2 & b_prop<=0.6) | 
           (c_prop<=0.3 & b_prop>=0.01 & b_prop<=0.4)) %>% 
  mutate(true_yn3 = (c_prop>=0.8 & b_prop>=0.4 & b_prop<=0.8) | 
           (c_prop>=0.5 & c_prop<=0.8 & b_prop>=0.2 & b_prop<=0.6) | 
           (c_prop>=0.35 & c_prop<=0.5 & b_prop>=0.01 & b_prop<=0.4) | 
           (c_prop>=0.15 & c_prop<=0.35 & b_prop>=0.01 & b_prop<=0.3) | 
           (c_prop>=0.05 & c_prop<=0.15 & b_prop>=0.01 & b_prop<=0.2) | 
           (c_prop<=0.05 & b_prop>=0.01 & b_prop<=0.1))


# Initial saturation data
sat1 <- readRDS(file.path(datadir, "depletion_initial.Rds")) %>% 
  # Mark whether prior is correct
  mutate(true_yn=(year<1960 & b_prop >= 0.5 & b_prop <=0.9) | 
           (year>=1960 & b_prop >= 0.2 & b_prop <=0.6))

# r/K data
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
  # Order methods
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


# Stats for manuscript
################################################################################

# Final depletion stats
sum(sat2$true_yn1) / nrow(sat2) * 100 # Catch-MSY
sum(sat2$true_yn3) / nrow(sat2) * 100 # CMSY

sum(sat2$c_prop<0.5) / nrow(sat2) *100

# % of stocks with CR<0.5 but B/K>0.5
n_cr_low <- sat2 %>% 
  filter(c_prop<0.5) %>% 
  nrow()
n_cr_low_b_hi <- sat2 %>% 
  filter(c_prop<0.5 & b_prop<0.5) %>% 
  nrow()
n_cr_low_b_hi / n_cr_low *100

# Initial depletion stats
###############################

# % correct
sum(sat1$true_yn) / nrow(sat1) * 100

# % after 1960 that are lightly exploited (b/k > 0.6)
n <- sat1 %>% filter(year>=1960) %>% nrow()
n_hi <- sat1 %>% filter(year>=1960 & b_prop>=0.6) %>% nrow()
n_hi / n

# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
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
n1 <- nrow(sat2)
g1 <- ggplot(sat2, aes(x=c_prop, y=b_prop, color=true_yn3)) +
  # Plot shading
  geom_polygon(data=sat2_priors, mapping=aes(x=cr, y=br, fill=method), 
               alpha=0.6, inherit.aes = F) +
  # Plot reference lines
  geom_hline(yintercept = 1, linetype="dashed") +
  geom_hline(yintercept = 0.5, linetype="dotted") +
  # Plot points
  geom_point() +
  # Plot sample size
  annotate("text", x=1, y=max(sat2$b_prop), label=paste0("n=", n1),
           hjust=1, size=2.5) +
  # Legend
  scale_fill_discrete(name="Method") +
  scale_color_manual(name="Prior", values=c("grey50", "black")) +
  guides(color=F) +
  # Labels
  labs(x="Catch ratio\n(recent / maximum catch)",
       y="Depletion\n(recent / unexploited biomass)",
       title="Final depletion",
       tag="A") +
  scale_x_continuous(breaks=seq(0, 1, 0.1)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.6,0.9),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.key.size=unit(0.3, "cm"))
g1

# Plot initial depletion
n2 <- nrow(sat1)
g2 <- ggplot(sat1, mapping=aes(x=year, y=b_prop, color=true_yn)) +
  # Plot shading
  geom_polygon(data=sat1_priors, mapping=aes(x=year, y=br, fill=method),
               alpha=0.6, show.legend=F, inherit.aes = F) +
  # Plot reference lines
  geom_hline(yintercept = 1, linetype="dashed") +
  geom_hline(yintercept = 0.5, linetype="dotted") +
  # Plot points
  geom_point() +
  # Plot sample size
  annotate("text", x=2000, y=max(sat1$b_prop), label=paste0("n=", n2),
           hjust=1, size=2.5) +
  # Legend
  scale_fill_discrete(name="Method", drop=F) +
  scale_color_manual(name="Prior", values=c("grey50", "black")) +
  guides(fill=F) +
  # Labels
  labs(x="Year\n ", 
       y="Depletion\n(recent / unexploited biomass)",
       title="Initial depletion",
       tag="B") +
  scale_x_continuous(breaks=seq(1880, 2000, 20)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot growth rate
n3 <- nrow(r)
g3 <- ggplot(r, aes(x=resilience, y=r)) +
  geom_boxplot() +
  # Plot priors
  geom_segment(data=r_priors,
               mapping=aes(y=r_lo, yend=r_hi, 
                           x=xpos, xend=xpos, 
                           color=method), show.legend = F) +
  # Plot sample size
  annotate("text", x=4.5, y=max(r$r), label=paste0("n=", n3),
           hjust=1, size=2.5) +
  # Labels
  labs(x="Resilience", 
       y="Intrinsic growth rate",
       title="Intrinsic growth rate, r",
       tag="C") +
  # Theme
  theme_bw() + my_theme
g3

# Plot carrying capacity
n4 <- nrow(k_all)
g4 <- ggplot(k_use, mapping=aes(x=resilience, y=scalar)) +
  geom_boxplot() +
  # Add prior
  geom_segment(data=k_priors, 
               mapping=aes(x=0.52, xend=0.52, y=k_lo, yend=k_hi, color=method),
               show.legend=F) +
  # Plot sample size
  annotate("text", x=5.5, y=100, label=paste0("n=", n4),
           hjust=1, size=2.5) +
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
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2, heights=c(0.6, 0.4))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_cmsy_prior_performance.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

# PDF
ggsave(g, filename=file.path(plotdir, "FigX_cmsy_prior_performance.pdf"), 
       width=180, height=180 / 6.5 * 5.5, units="mm", dpi=600)


