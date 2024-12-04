library(tidyverse)
library(geographr)
library(IMD)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# ---- UK-wide deprivation deciles ----
uk_imd <-
  bind_rows(
    IMD::imd_england_lsoa |> select(lsoa11_code = lsoa_code, IMD_decile),
    IMD::imd_wales_lsoa |> select(lsoa11_code = lsoa_code, IMD_decile),
    IMD::imd_scotland_dz |> select(lsoa11_code = dz_code, IMD_decile),
    IMD::imd_northern_ireland_soa |> select(lsoa11_code = soa_code, IMD_decile)
  ) |>
  mutate(IMD_quintile = ceiling(IMD_decile / 2))

# ---- Load BRC data with referral notes/comments ----
referrals_raw <- read_csv("data/BRM_data_anon.csv")

# Combine referral reasons and comments into a single field
# and remove rows without any notes
referrals <-
  referrals_raw |>
  select(-`...1`, -referral_comments_anon) |>
  filter(`Referral reason` != "NULL" & `Referral comments` != "NULL") |>
  mutate(Notes = paste(`Referral reason`, `Referral comments`, sep = "\n")) |>
  filter(!is.na(Notes))

# Look up deprivation deciles
referrals <-
  referrals |>
  mutate(postcode = str_remove_all(Postcode, " ")) |>
  left_join(lookup_postcode_oa11_lsoa11_msoa11_ltla20, by = "postcode") |>
  left_join(uk_imd, by = "lsoa11_code")

#TODO: Add Northern Ireland postcodes --> SOA 2011 lookup, then add their IMD data

# ---- Load keywords ----
# Keywords to identify social determinants of health - among people with Alzheimer's disease
# From eTable 2. A lexicon of terms used to identify social determinants of health
# in Wu et al. (2023) - Natural language processing to identify social determinants of health in Alzheimer's disease and related dementia from electronic health records
# https://doi.org/10.1111/1475-6773.14210
sdoh_keywords_alz <- read_csv("data-public/sdoh-keywords-alzheimers.csv")

sdoh_keywords_alz_isolation <-
  sdoh_keywords_alz |>
  filter(`Social isolation` == 1) |>
  pull(Term)

# Convert to a regular expression
sdoh_keywords_alz_isolation <- paste(sdoh_keywords_alz_isolation, collapse = "|")

sdoh_keywords_alz_long <-
  sdoh_keywords_alz |>
  pivot_longer(cols = where(is.double), names_to = "Determinant")

# Create a regular expression for each social determinant
sdoh_regex <-
  unique(sdoh_keywords_alz_long$Determinant) |>
  map_dfr(~ {
    determinant <- .x
    regex_string <- sdoh_keywords_alz_long |>
      filter(Determinant == determinant, value == 1) |>
      pull(Term) |>
      paste(collapse = "|")
    tibble(Determinant = determinant, regex_string = regex_string)
  })

# ---- Rule-based approach to identifying social determinants ----
referrals_sdoh <-
  referrals |>
  mutate(`Social isolation` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "isolation")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Housing` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Housing")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Transport` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Transport")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Food` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Food")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Medication` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Medication")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Abuse` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Abuse")) |> pull(regex_string)) ~ 1,
    .default = 0
  )) |>
  mutate(`Financial difficulty` = case_when(
    str_detect(Notes, sdoh_regex |> filter(str_detect(Determinant, "Financial difficulty")) |> pull(regex_string)) ~ 1,
    .default = 0
  ))

# Save
write_csv(referrals_sdoh, "data/referrals-sdoh.csv")
