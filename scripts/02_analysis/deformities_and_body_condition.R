################################################################################
# Analyzing the Effect of Deformities on Body Condition
################################################################################
#
# Aidan O'Brien
# aeo83@miami.edu
# 1/13/2026
#
# Description
#

# Load Packages ################################################################
library(tidyverse)
library(readxl)
library(car)

################################################################################
# 1. Ellicott Juveniles
################################################################################

# 1.1 Read and transform data --------------------------------------------------
ellicott_juv <- read_excel("data/processed/ellicott_juveniles_with_measurements.xlsx") |>
  select(mass, svl, deformities) |>
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


