library(tidyverse)
library(janitor)
library(geographr)
library(IMD)
library(broom)

# Install package to explore Relative Importance of Regressors in Linear Models
# https://cran.r-project.org/web/packages/relaimpo/relaimpo.pdf
# The package maintainer says there's functionality on a version available via her website that isn't on CRAN.
# First, install from CRAN, then install from source, downloaded via https://prof.bht-berlin.de/groemping/software/relaimpo2-1
# install.packages("relaimpo")
# install.packages("C:/Users/040026704/Downloads/relaimpo_2.2-5.zip", repos = NULL, type="source")
library(relaimpo)

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

# ---- Rule-based approach to identifying social determinants ----
referrals_sdoh <-
  referrals |>
  mutate(`Social isolation` = case_when(
    str_detect(Notes, sdoh_keywords_alz_isolation) ~ 1,
    .default = 0
  ))

# ---- Explore social determinants ----
# How many records are labelled as socially isolated?
referrals_sdoh |>
  count(`Social isolation`) |>
  mutate(prop = n/sum(n))

referrals_sdoh |>
  filter(`Social isolation` == 1) |>
  select(Notes) |>
  View()

# - Compare people with and without the SDOH label -
sdoh_table <- function(d, x, sdoh) {
  d |>
    tabyl({{ x }}, {{ sdoh }}) |>
    adorn_percentages("col") |>
    adorn_pct_formatting() |>
    adorn_ns()
}

# Age
referrals_sdoh |>
  ggplot(aes(x = factor(`Social isolation`), y = Age)) +
  geom_violin()

# Gender
referrals_sdoh |> sdoh_table(Gender, `Social isolation`)

# Ethnicity
referrals_sdoh |> sdoh_table(Ethnicity, `Social isolation`)

# Living arrangements
referrals_sdoh |> sdoh_table(`Living Arrangement`, `Social isolation`)

# Disability status
referrals_sdoh |> sdoh_table(`Reported Disability`, `Social isolation`)

# Neighbourhood deprivation
referrals_sdoh |> sdoh_table(IMD_quintile, `Social isolation`)

# ---- What factors are the strongest predictors of social isolation? ----
# Standardise covariates over 2 standard deviations so numeric and categorical predictors can be directly compared
referrals_sdoh_model <-
  referrals_sdoh |>
  select(`Social isolation`, Age, Gender, Ethnicity, `Living Arrangement`, `Reported Disability`, IMD_quintile) |>
  # mutate(across(-`Social isolation`, \(x) (x - mean(x, na.rm = TRUE)) / 2 * sd(x, na.rm = TRUE))) |>
  drop_na() |>

  # Create reference categories for logistic regression
  mutate(
    Gender = fct_relevel(Gender, "No Data"),
    Ethnicity = fct_relevel(Ethnicity, "No Data"),
    `Living Arrangement` = fct_relevel(`Living Arrangement`, "No Data"),
    `Reported Disability` = fct_relevel(`Reported Disability`, "No data")
  )

mod_isolation <- glm(`Social isolation` ~ ., data = referrals_sdoh_model, family = binomial)

glance(mod_isolation)

mod_isolation |>
  tidy(exponentiate = TRUE)

#TODO: plot ORs with confidence intervals

#TODO: use relaimpo to quantify relative importance of predictors

#TODO in future, when we have several social determinants labelled: multinomial logistic regression?
