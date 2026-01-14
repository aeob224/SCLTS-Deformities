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
################################################################################



################################################################################
# 2. Prospect Juveniles
################################################################################
# 2.1 Read and transform data --------------------------------------------------
prospect_juv <- read_excel("data/processed/prospect_juveniles_with_measurements.xlsx") |>
  select(mass, svl, deformities) |>
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
  select(mass, svl, deformed) |>
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
################################################################################




################################################################################
# 4. Prospect Larvae
################################################################################
# 4.1 Read and transform data --------------------------------------------------
prospect_larv <- read_excel("data/raw/prospect_larvae_raw.xlsx") |>
  select(mass, svl, deformed) |>
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
