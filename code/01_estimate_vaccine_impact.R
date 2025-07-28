###############################################################################
# Title: Modeling the health impact of discontinuing COVID-19 vaccination
#        during pregnancy in the United States
# Draft by: Nathan Lo
# Origin Date: 6/12/25
# Last updated: 7/14/25
###############################################################################

## Import ----
library(here)
library(tidyverse)
library(readxl)

## Data ----
# Data downloaded from the US CDC COVID-Net surveillance database on June 11,
# 2025
#       URL: https://www.cdc.gov/covid/php/covid-net/index.html.
#
# Data curated to weeks of 1/6/24-4/26/25 using age-specific weekly COVID
# hospitalization incidence (per 100,000) We used 0-<6 months and 18-49 years.
# Additionally, we obtained data on percent of hospitalizations were
# attributable to COVID based on clinical symtoms (only available for <18 years
# and 18-49 years group)
COVID_Net <- read_excel(here("data_raw", "COVID-Net.xlsx"))

## Split data ----
data_infant <- subset(COVID_Net, AgeCategory_Legend == "0-<6 months")
data_preg <- subset(COVID_Net, AgeCategory_Legend == "18-49 years")

## Model parameters ----
# VE against hospitalization in young infants by 35% (95% 15-51%) during
# their first 6 months of life
VE_infant <- 0.35
VE_infant_UI <- c(0.15, 0.51)

# VE against ED / UC encounters in general younger population by
# 33% (95% 28–38) lasting about 4 months
VE_preg <- 0.33
VE_preg_UI <- c(0.28, 0.38)

# Relative risk during pregnancy
RR_preg <- 2.65
RR_preg_UI <- c(2.41, 2.88)

# Vaccine coverage
vacc_cov <- COVID_Net$PregVaccCov[1]

# Population estimates for 0-6 months and pregnant population
#
# Approximately 3.65M infants <1 y/o in the US between 2020 and 2023:
# http://wonder.cdc.gov/controller/saved/D196/D442F807. We use 1.83M (1/2).
#
# Approximately ~3M pregnant women in the US at any given point in time:
# https://pubmed.ncbi.nlm.nih.gov/39361960/. We use 3M.
#
pop_06months <- 1830000
pop_preg <- 3000000

VACCINE_UPTAKE <- c(0, 1, .5, .15)

## Main analysis ----
# Estimate true risk in both populations, accounting for COVID as cause
# of hospitalization and removing historic impact of vaccination
# (albeit with low uptake)
data_infant$adj_inc <- (data_infant$WeeklyRate * data_infant$COVIDcause) /
    (vacc_cov * (1 - VE_infant) + (1 - vacc_cov))

# Given pregnant persons are a small fraction of the 18-49 population,
# we did not do further adjustments
data_preg$adj_inc <- (data_preg$WeeklyRate * data_preg$COVIDcause * RR_preg)
data_preg$adj_inc_lower <- (data_preg$WeeklyRate * data_preg$COVIDcause * RR_preg_UI[1])
data_preg$adj_inc_upper <- (data_preg$WeeklyRate * data_preg$COVIDcause * RR_preg_UI[2])

infant_holder <- vector("list", NROW(VACCINE_UPTAKE))
preg_holder <- vector("list", NROW(VACCINE_UPTAKE))

for (i in 1:NROW(VACCINE_UPTAKE)) {
    VU <- VACCINE_UPTAKE[i]

    if (VU == 0) {
        infant_holder[[i]] <- tibble(
            scenario = "observed",
            date = as.Date(data_infant$`_WeekendDate`),
            estimate = data_infant$adj_inc / 1e5 * pop_06months,
            lower = NA_real_,
            upper = NA_real_,
            averted = NA_real_,
            averted_lower = NA_real_,
            averted_upper = NA_real_,

        )

        preg_holder[[i]] <- tibble(
            scenario = "observed",
            date = as.Date(data_preg$`_WeekendDate`),
            estimate = data_preg$adj_inc / 1e5 * pop_preg,
            lower = NA_real_,
            upper = NA_real_,
            averted = NA_real_,
            averted_lower = NA_real_,
            averted_upper = NA_real_,
        )
    } else {
        infant_holder[[i]] <- tibble(
            scenario = sprintf("vaccine%i", round(VU * 100)),
            date = as.Date(data_infant$`_WeekendDate`),
            estimate = (data_infant$adj_inc / 1e5 * pop_06months * (1 - VE_infant * VU)),
            lower = (data_infant$adj_inc / 1e5 * pop_06months * (1 - VE_infant_UI[2] * VU)),
            upper = (data_infant$adj_inc / 1e5 * pop_06months * (1 - VE_infant_UI[1] * VU)),
            averted = data_infant$adj_inc / 1e5 * pop_06months * VU * VE_infant,
            averted_lower = data_infant$adj_inc / 1e5 * pop_06months * VU * VE_infant_UI[1],
            averted_upper = data_infant$adj_inc / 1e5 * pop_06months * VU * VE_infant_UI[2],
        )

        preg_holder[[i]] <- tibble(
            scenario = sprintf("vaccine%i", round(VU * 100)),
            date = as.Date(data_preg$`_WeekendDate`),
            estimate = (data_preg$adj_inc / 1e5 * pop_preg * (1 - VE_preg * VU)),
            lower = (data_preg$adj_inc / 1e5 * pop_preg * (1 - VE_preg_UI[2] * VU)),
            upper = (data_preg$adj_inc / 1e5 * pop_preg * (1 - VE_preg_UI[1] * VU)),
            averted = data_preg$adj_inc / 1e5 * pop_preg * VU * VE_preg * (4 / 9),
            averted_lower = data_preg$adj_inc_lower / 1e5 * pop_preg * VU * VE_preg_UI[1] * (4 / 9),
            averted_upper = data_preg$adj_inc_upper / 1e5 * pop_preg * VU * VE_preg_UI[2] * (4 / 9),
        )
    }
}

full_data <- bind_rows(
    infant_holder |> 
        bind_rows() |> 
        mutate(population = "infant", .before = 1) |> 
        arrange(scenario, date) |> 
        group_by(scenario) |> 
        mutate(cume_averted = cumsum(averted),
               cume_averted_lower = cumsum(averted_lower),
               cume_averted_upper = cumsum(averted_upper)) |> 
        ungroup(),
    preg_holder |> 
        bind_rows() |> 
        mutate(population = "pregnant", .before = 1) |> 
        arrange(scenario, date) |> 
        group_by(scenario) |> 
        mutate(cume_averted = cumsum(averted),
               cume_averted_lower = cumsum(averted_lower),
               cume_averted_upper = cumsum(averted_upper)) |> 
        ungroup()
) |> 
    as_tibble()


## Save ----
write_csv(full_data,
          here("data", "vaccine_impact.csv"))
write_rds(full_data,
          here("data", "vaccine_impact.RDS"))
