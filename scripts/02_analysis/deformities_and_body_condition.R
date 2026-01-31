################################################################################
# Analyzing the Effect of Deformities on Body Condition
################################################################################
#
# Aidan O'Brien
# aeo83@miami.edu
# 1/13/2026
#
# This is a set of oneway ANOVAs designed to test if the presence of deformities
# in larval and juvenile Santa Cruz Long-toed Salamanders results in differences
# in body condition (measured as residuals of log-mass vs. log-snout-to-vent-
# length). We test this effect at two different ponds, called Prospect and
# Ellicott.
#

# Load Packages ################################################################
library(tidyverse)
library(readxl)
library(car)
library(cowplot)
library(ggpubr)


################################################################################
# 1. Ellicott Juveniles
################################################################################

# 1.1 Read and transform data --------------------------------------------------
ellicott_juv <- read_excel("data/processed/ellicott_juveniles_with_measurements.xlsx") |>
  dplyr::select(mass, svl, deformities) |>
  mutate(log_mass = log(mass),
         log_svl = log(svl),
         deformities = as.factor(deformities))

# 1.2 Calculate body condition -------------------------------------------------
ellicott_juv$body_condition <- residuals(lm(log_mass ~ log_svl, ellicott_juv))

# 1.3 Analyze deformity effect on body condition -------------------------------
## Test equality of variance
leveneTest(body_condition ~ deformities, data = ellicott_juv)

## Variances not equal. Use Welch's ANOVA
ellicott_juv_anova <- oneway.test(body_condition ~ deformities,
                          data = ellicott_juv,
                          var.equal = F)

ellicott_juv_anova
################################################################################




################################################################################
# 2. Prospect Juveniles
################################################################################
# 2.1 Read and transform data --------------------------------------------------
prospect_juv <- read_excel("data/processed/prospect_juveniles_with_measurements.xlsx") |>
  dplyr::select(mass, svl, deformities) |>
  mutate(log_mass = log(mass),
         log_svl = log(svl),
         deformities = as.factor(deformities))

# 2.2 Calculate body condition -------------------------------------------------
prospect_juv$body_condition <- residuals(lm(log_mass ~ log_svl, prospect_juv))

# 2.3 Analyze deformity effect on body condition -------------------------------
## Test equality of variance
leveneTest(body_condition ~ deformities, data = prospect_juv)

## Variances equal. Use Oneway ANOVA
prospect_juv_anova <- aov(body_condition ~ deformities,
                          data = prospect_juv)

summary(prospect_juv_anova)
################################################################################




################################################################################
# 3. Ellicott Larvae
################################################################################
# 3.1 Read and transform data --------------------------------------------------
ellicott_larv <- read_excel("data/raw/ellicott_larvae_raw.xlsx") |>
  dplyr::select(mass, svl, deformed) |>
  drop_na() |>
  mutate(log_mass = log(as.numeric(mass)),
         log_svl = log(as.numeric(svl)),
         deformities = as.factor(deformed))

# 3.2 Calculate body condition -------------------------------------------------
ellicott_larv$body_condition <- residuals(lm(log_mass ~ log_svl, ellicott_larv))

# 3.3 Analyze deformity effect on body condition -------------------------------
## Test equality of variance
leveneTest(body_condition ~ deformities, data = ellicott_larv)

## Variances equal. Use Oneway ANOVA
ellicott_larv_anova <- aov(body_condition ~ deformities,
                          data = ellicott_larv)

summary(ellicott_larv_anova)
TukeyHSD(ellicott_larv_anova)
################################################################################




################################################################################
# 4. Prospect Larvae
################################################################################
# 4.1 Read and transform data --------------------------------------------------
prospect_larv <- read_excel("data/raw/prospect_larvae_raw.xlsx") |>
  dplyr::select(mass, svl, deformed) |>
  drop_na() |>
  mutate(log_mass = log(as.numeric(mass)),
         log_svl = log(as.numeric(svl)),
         deformities = as.factor(deformed))

# 4.2 Calculate body condition -------------------------------------------------
prospect_larv$body_condition <- residuals(lm(log_mass ~ log_svl, prospect_larv))

# 4.3 Analyze deformity effect on body condition -------------------------------
## Test equality of variance
leveneTest(body_condition ~ deformities, data = prospect_larv)

## Variances equal. Use Oneway ANOVA
prospect_larv_anova <- aov(body_condition ~ deformities,
                          data = prospect_larv)

summary(prospect_larv_anova)
################################################################################




################################################################################
# 5 Data Visualization
################################################################################

# 5.1 Ellicott Juvenile Boxplot ------------------------------------------------
levels(ellicott_juv$deformities) <- c('Healthy', 'Deformed')

ellicott_juv_plot <- ggplot(data = ellicott_juv,
                            aes( x = deformities,
                                 y = body_condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  xlab("Deformity Status") +
  ylab("Body Condition") +
  labs(title = "Ellicott Juveniles") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0))  +
  stat_compare_means(method = "anova",
                     label = "p.signif",
                     size = 8,
                     hide.ns = T)

ellicott_juv_plot

# 5.2 Prospect Juvenile Boxplot ------------------------------------------------
levels(prospect_juv$deformities) <- c('Healthy', 'Deformed')

prospect_juv_plot <- ggplot(data = prospect_juv,
                             aes( x = deformities,
                                  y = body_condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  xlab("Deformity Status") +
  ylab("Body Condition") +
  labs(title = "Prospect Juveniles") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0)) +
  stat_compare_means(method = "anova",
                     label = "p.signif",
                     size = 8,
                     hide.ns = T)

prospect_juv_plot

# 5.3 Ellicott Larvae Boxplot --------------------------------------------------
levels(ellicott_larv$deformities) <- c('Healthy', 'Deformed')

ellicott_larv_plot <- ggplot(data = ellicott_larv,
                             aes( x = deformities,
                                  y = body_condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  xlab("Deformity Status") +
  ylab("Body Condition") +
  labs(title = "Ellicott Larvae") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0)) +
  stat_compare_means(method = "anova",
                     label = "p.signif",
                     size = 8,
                     hide.ns = T)

ellicott_larv_plot

# 5.4 Prospect Larvae ----------------------------------------------------------
levels(prospect_larv$deformities) <- c('Healthy', 'Deformed')

prospect_larv_plot <- ggplot(data = prospect_larv,
                             aes( x = deformities,
                                  y = body_condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  xlab("Deformity Status") +
  ylab("Body Condition") +
  labs(title = "Prospect Larvae") +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0)) +
  stat_compare_means(method = "anova",
                     label = "p.signif",
                     size = 8,
                     hide.ns = T)

prospect_larv_plot


# 5.5 Multi-Plots ---------------------------------------------------------------

## For juveniles
juvenile_plots <- plot_grid(ellicott_juv_plot,
                            prospect_juv_plot,
                            ncol = 2,
                            labels = c("A", "B"),
                            label_size = 20)

ggsave("results/img/juvenile_plots.jpg", plot = juvenile_plots,
       width = 25,
       height = 15)

## For larvae
larval_plots <- plot_grid(ellicott_larv_plot,
                          prospect_larv_plot,
                          ncol = 2,
                          labels = c("A", "B"),
                          label_size = 20)

ggsave("results/img/larval_plots.jpg", plot = larval_plots,
       width = 25,
       height = 15)

## All four plots
full_plots <- plot_grid(ellicott_juv_plot,
                        prospect_juv_plot,
                        ellicott_larv_plot,
                        prospect_larv_plot,
                        ncol = 2,
                        labels = c("A", "B", "C", "D"),
                        label_size = 20)

full_plots
ggsave("results/img/all_body_condition_plots.jpg", plot = full_plots,
       width = 20,
       height = 15)
################################################################################










