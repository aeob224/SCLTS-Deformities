################################################################################
# SCLTS Deformity Contigency Testing
################################################################################
#
# Aidan O'Brien
# aeo83@miami.edu
# 6/16/2026
#
# This is a set of contingency tests designewd to see if the proportion of
# deformity categories changes between Ellicott and Prospect or between larva
# and juvenile life stages.
#

# Load Packages ################################################################
library(tidyverse)

# Read and Process Data ########################################################

## Ellicott Juveniles ----------------------------------------------------------
ellicott_juv_severity <- read_csv("data/raw/severity_dataset.csv") |>
  dplyr::select(pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Ellicott (inner)" | pond == "Ellicott (outer)") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0)) |>
  mutate(severity_index = as.factor(severity_index))

### Get counts of each severity type
ellicott_juv_severity |>
  group_by(severity_index) |>
  count()


## Prospect Juveniles ----------------------------------------------------------
prospect_juv_severity <- read_csv("data/raw/severity_dataset.csv") |>
  dplyr::select(date, pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Prospect") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0)) |>
  mutate(severity_index = as.factor(severity_index))

### Get counts of each severity type
prospect_juv_severity |>
  group_by(severity_index) |>
  count()


## Ellicott Larvae -------------------------------------------------------------
ellicott_larvae_severity <- read_csv("data/raw/larval_deformity_severity.csv") |>
  dplyr::select(date, pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Ellicott") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0)) |>
  mutate(severity_index = as.factor(severity_index))

### Get counts of each severity type
ellicott_larvae_severity |>
  group_by(severity_index) |>
  count()


## Prospect Larvae -------------------------------------------------------------
prospect_larvae_severity <- read_csv("data/raw/larval_deformity_severity.csv") |>
  select(date, pond, svl, mass, extra_toes, extra_limbs, fused_limbs) |>
  filter(pond == "Prospect") |>
  mutate(severity_index = case_when(
    fused_limbs > 0 ~ 3,
    extra_limbs > 0 ~ 2,
    extra_toes > 0 ~ 1,
    TRUE ~ 0)) |>
  mutate(severity_index = as.factor(severity_index))

### Get counts of each severity type
prospect_larvae_severity |>
  group_by(severity_index) |>
  count()




################################################################################
# Pooled Larva vs. Juvenile Test
################################################################################

## Extract Counts for Larvae ---------------------------------------------------

ellicott_larvae_severity |>
  group_by(severity_index) |>
  count()
### 24, 6, 13, 11

prospect_larvae_severity |>
  group_by(severity_index) |>
  count()
### 49, 2, 9, 3

### Total Larvae: 73, 8, 22, 14


## Extract Counts for Juveniles ------------------------------------------------
ellicott_juv_severity |>
  group_by(severity_index) |>
  count()
### 150, 54, 40, 70

prospect_juv_severity |>
  group_by(severity_index) |>
  count()
### 22, 7, 4, 2

### Total Juveniles: 172, 61, 44, 72


## Set up table ----------------------------------------------------------------
observed_table <- matrix(c(73, 172, 8, 61, 22, 44, 14, 72), nrow = 2, ncol = 4)
rownames(observed_table) <- c('larvae', 'juveniles')
colnames(observed_table) <- c('healthy', 'extra digits', 'extra limbs', 'fused limbs')
observed_table


## Run contingency test ---------------------------------------------------------
test <- chisq.test(observed_table)
test

### Extract standardized residuals to determine what is contributing to the
### contingency test results.
test$stdres
################################################################################





################################################################################
# Ellicott Larvae vs. Juveniles
################################################################################

## Extract Counts --------------------------------------------------------------
ellicott_larvae_severity |>
  group_by(severity_index) |>
  count()
### 24, 6, 13, 11

ellicott_juv_severity |>
  group_by(severity_index) |>
  count()
### 150, 54, 40, 70

## Set up table ----------------------------------------------------------------
observed_table <- matrix(c(24,150,6,54,13,40,11,70), nrow = 2, ncol = 4)
rownames(observed_table) <- c('Larvae', 'Juveniles')
colnames(observed_table) <- c('healthy', 'extra digits', 'extra limbs', 'fused limbs')
observed_table

## Run Fisher's exact test due to small samples --------------------------------
test <- chisq.test(observed_table)
test
################################################################################





################################################################################
# Prospect Larvae vs. Metamorphs
################################################################################

## Extract Counts --------------------------------------------------------------
prospect_larvae_severity |>
  group_by(severity_index) |>
  count()
### 49, 2, 9, 3


prospect_juv_severity |>
  group_by(severity_index) |>
  count()
### 22, 7, 4, 2


## Set up table ----------------------------------------------------------------
observed_table <- matrix(c(49, 22, 2, 7, 9, 4, 3, 2), nrow = 2, ncol = 4)
rownames(observed_table) <- c('Larvae', 'Juveniles')
colnames(observed_table) <- c('healthy', 'extra digits', 'extra limbs', 'fused limbs')
observed_table

## Run Fisher's exact test due to small samples --------------------------------
test <- fisher.test(observed_table)
test
################################################################################





################################################################################
# Ellicott vs. Prospect Larvae
################################################################################

## Extract Counts --------------------------------------------------------------
ellicott_larvae_severity |>
  group_by(severity_index) |>
  count()
### 24, 6, 13, 11

prospect_larvae_severity |>
  group_by(severity_index) |>
  count()
### 49, 2, 9, 3

## Set up table ----------------------------------------------------------------
observed_table <- matrix(c(24, 49, 6, 2, 13, 9, 11, 3), nrow = 2, ncol = 4)
rownames(observed_table) <- c('Ellicott', 'Prospect')
colnames(observed_table) <- c('healthy', 'extra digits', 'extra limbs', 'fused limbs')
observed_table

## Run Fisher's exact test due to small samples --------------------------------
test <- fisher.test(observed_table)
test
################################################################################





################################################################################
# Ellicott vs. Prospect Juveniles
################################################################################
## Extract Counts --------------------------------------------------------------
ellicott_juv_severity |>
  group_by(severity_index) |>
  count()
### 150, 54, 40, 70

prospect_juv_severity |>
  group_by(severity_index) |>
  count()
### 22, 7, 4, 2

## Set up table ----------------------------------------------------------------
observed_table <- matrix(c(150, 22, 54, 7, 40, 4, 70, 2), nrow = 2, ncol = 4)
rownames(observed_table) <- c('Ellicott', 'Prospect')
colnames(observed_table) <- c('healthy', 'extra digits', 'extra limbs', 'fused limbs')
observed_table

## Run Fisher's exact test due to small samples --------------------------------
test <- fisher.test(observed_table)
test
################################################################################












