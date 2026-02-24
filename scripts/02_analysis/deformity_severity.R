################################################################################
# Analyzing the Effect of Deformity Severity on Body Condition
################################################################################
#
# Aidan O'Brien
# aeo83@miami.edu
# 1/16/2026
#
# This is a set of analyses designed to compare body condition within deformed
# Santa Cruz Long-toed Salamanders based on the deformity type.


# Load Packages ################################################################
library(tidyverse)
library(readxl)
library(cowplot)
library(car)
library(rstatix)
library(cld)
library(ggpubr)

################################################################################
# 1. Ellicott Juvenile Severity Effect
################################################################################

# 1.1 Filter Ellicott data ----------------------------------------------------

## This filters Ellicott data where deformity severity and  body condition
## could be calculated. It also calculates a severity index and body condition.
ellicott_severity <- read_csv("data/raw/severity_dataset.csv") |>
  dplyr::select(pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Ellicott (inner)" | pond == "Ellicott (outer)") |>
  drop_na() |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0
  ),
  log_mass = log(mass),
  log_svl = log(svl)) |>
  mutate(body_condition = residuals(lm(log_mass ~ log_svl)),
         severity_index = as.character(severity_index))




# 1.2 ANOVA of severity and body condition -------------------------------------
## Levene test is significant. Variances not equal, so use Welch's ANOVA.
leveneTest(body_condition ~ severity_index, data = ellicott_severity)

## Welch's ANOVA
ellicott_juv_anova <- oneway.test(body_condition ~ severity_index,
                                  data = ellicott_severity,
                                  var.equal = F)

ellicott_juv_anova

# 1.3 Post-Hoc Test -----------------------------------------------------------
juvenile_multiple_comparison <- games_howell_test(body_condition ~ severity_index,
                  data = ellicott_severity,
                  conf.level = 0.95)

## Connecting Letters Report
cld::make_cld(juvenile_multiple_comparison)

# 1.4 Visualization ------------------------------------------------------------
ellicott_deformity_severity <- ggplot(data = ellicott_severity,
       aes(x = severity_index,
           y = body_condition)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width = 0.1) +
  xlab("Severity Index") +
  ylab("Body Condition") +
  labs(title = "Deformity Severity Effects on Ellicott Juveniles") +
  theme_classic() +
  ylim(-0.55,0.55) +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Healthy", "Extra Digits", "Extra Limbs", "Fused Limbs"))



ellicott_deformity_severity
ggsave("results/img/ellicott_juvenile_severity.jpg", plot = ellicott_deformity_severity,
       width = 27,
       height = 15)

################################################################################







################################################################################
# 2. Ellicott Larvae Severity Effect
################################################################################

# 2.1 Filter Ellicott data ----------------------------------------------------

## This filters Ellicott data where deformity severity and  body condition
## could be calculated. It also calculates a severity index and body condition.
ellicott_larvae_severity <- read_csv("data/raw/larval_deformity_severity.csv") |>
  dplyr::select(pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Ellicott") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0
  ),
  svl = as.numeric(svl),
  mass = as.numeric(mass)) |>
  drop_na() |>
  mutate(log_mass = log(mass),
  log_svl = log(svl)) |>
  mutate(body_condition = residuals(lm(log_mass ~ log_svl)),
         severity_index = as.character(severity_index))

# 2.2 ANOVA of severity and body condition -------------------------------------
## Levene test is significant. Variances are equal, so use oneway ANOVA.
leveneTest(body_condition ~ severity_index, data = ellicott_larvae_severity)

## Oneway ANOVA is significant
ellicott_larvae_anova <- aov(body_condition ~ severity_index,
                                  data = ellicott_larvae_severity)

summary(ellicott_larvae_anova)

# 2.3 Post-Hoc Test -----------------------------------------------------------
larval_multiple_comparisons <- tukey_hsd(ellicott_larvae_anova,
          conf.level = 0.95)

larval_multiple_comparisons

cld::make_cld(larval_multiple_comparisons)

# 2.4 Visualization ------------------------------------------------------------
ellicott_larval_deformity_severity <- ggplot(data = ellicott_larvae_severity,
                                      aes(x = severity_index,
                                          y = body_condition)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width = 0.1) +
  xlab("Severity Index") +
  ylab("Body Condition") +
  labs(title = "Deformity Severity Effects on Ellicott Larvae") +
  theme_classic() +
  ylim(-0.5,0.5)+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22),
        title = element_text(size = 24),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Healthy", "Extra Digits", "Extra Limbs", "Fused Limbs"))


ellicott_larval_deformity_severity
ggsave("results/img/ellicott_larval_severity.jpg", plot = ellicott_larval_deformity_severity,
       width = 27,
       height = 15)
################################################################################







################################################################################
# 3. Prospect Larvae Severity Effect
################################################################################

# 3.1 Filter Prospect data ----------------------------------------------------

## This filters Prospect data where deformity severity and  body condition
## could be calculated. It also calculates a severity index and body condition.
prospect_larvae_severity <- read_csv("data/raw/larval_deformity_severity.csv") |>
  select(pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Prospect") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0
  ),
  svl = as.numeric(svl),
  mass = as.numeric(mass)) |>
  drop_na() |>
  mutate(log_mass = log(mass),
         log_svl = log(svl)) |>
  mutate(body_condition = residuals(lm(log_mass ~ log_svl)),
         severity_index = as.character(severity_index))



# 3.2 ANOVA of severity and body condition -------------------------------------
## Levene test is significant. Variances are equal, so use oneway ANOVA.
leveneTest(body_condition ~ severity_index, data = prospect_larvae_severity)

## Oneway ANOVA is significant
prospect_larvae_anova <- aov(body_condition ~ severity_index,
                             data = prospect_larvae_severity)

summary(prospect_larvae_anova)


# 3.3 Visualization ------------------------------------------------------------
prospect__larval_deformity_severity <- ggplot(data = prospect_larvae_severity,
                                              aes(x = severity_index,
                                                  y = body_condition)) +
  geom_boxplot(outliers = F) +
  geom_jitter(width = 0.1) +
  xlab("Severity Index Score") +
  ylab("Body Condition") +
  labs(title = "Deformity Severity Effects on Prospect Larvae") +
  theme_classic() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

prospect__larval_deformity_severity

################################################################################






################################################################################
# 4. Ellicott Figure
################################################################################
severity_plots <- plot_grid(ellicott_deformity_severity,
                            ellicott_larval_deformity_severity,
                            ncol = 2,
                            labels = c("A)", "B)"),
                            label_size = 28)
severity_plots

ggsave("results/img/severity_plots.jpg", plot = severity_plots,
       width = 20,
       height = 10)












