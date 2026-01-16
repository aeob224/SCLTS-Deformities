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

################################################################################
# 1. Ellicott Juvenile Severity Effect
################################################################################

# 1.1 Filter Ellicott data ----------------------------------------------------

## This filters Ellicott data where deformity severity and  body condition
## could be calculated.
ellicott_severity <- read_csv("data/raw/simplified_deformities.csv") |>
  select(pond, svl, mass, extra_toes, extra_limbs, fused_limbs, fused_toes) |>
  filter(pond == "Ellicott (inner)" | pond == "Ellicott (outer)") |>
  drop_na() |>
  mutate(log_mass = log(mass),
         log_svl = log(svl),
         extra_toes = as.factor(as.numeric(extra_toes > 0)),
         extra_limbs = as.factor(as.numeric(extra_limbs > 0)),
         fused_limbs = as.factor(as.numeric(fused_limbs > 0)),
         fused_toes = as.factor(as.numeric(fused_toes > 0)))


# 1.2 Calculate body condition -------------------------------------------------

ellicott_severity$body_condition <- residuals(lm(log_mass ~ log_svl, ellicott_severity))


# 1.3 Compare extra toes to those without extra toes ---------------------------
## Test equality of variance
leveneTest(body_condition ~ extra_toes, data = ellicott_severity)

## Variances equal. Use oneway ANOVA
extra_toes_anova <- oneway.test(body_condition ~ extra_toes,
                                  data = ellicott_severity,
                                  var.equal = T)

extra_toes_anova


# 1.4 Compare extra limbs to those without extra limbs -------------------------
## Test equality of variance
leveneTest(body_condition ~ extra_limbs, data = ellicott_severity)

## Variances equal. Use oneway ANOVA
extra_limbs_anova <- oneway.test(body_condition ~ extra_limbs,
                                data = ellicott_severity,
                                var.equal = T)

extra_limbs_anova


# 1.5 Compare fused limbs to those without fused limbs -------------------------
## Test equality of variance
leveneTest(body_condition ~ fused_limbs, data = ellicott_severity)

## Variances equal. Use oneway ANOVA
fused_limbs_anova <- oneway.test(body_condition ~ fused_limbs,
                                 data = ellicott_severity,
                                 var.equal = T)

fused_limbs_anova









# Test boxplot
levels(ellicott_severity$fused_limbs) <- c('Not Fused', 'Fused')

fused_limb_plot <- ggplot(data = ellicott_severity,
                             aes( x = fused_limbs,
                                  y = body_condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  xlab("Fused Limbs?") +
  ylab("Body Condition") +
  labs(title = "Ellicott Juveniles") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

fused_limb_plot


