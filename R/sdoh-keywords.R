library(tidyverse)
library(janitor)
library(geographr)
library(IMD)
library(broom)
library(ggstats)
library(arrow)

# Install package to explore Relative Importance of Regressors in Linear Models
# https://cran.r-project.org/web/packages/relaimpo/relaimpo.pdf
# The package maintainer says there's functionality on a version available via her website that isn't on CRAN.
# First, install from CRAN, then install from source, downloaded via https://prof.bht-berlin.de/groemping/software/relaimpo2-1
# install.packages("relaimpo")
# install.packages("C:/Users/040026704/Downloads/relaimpo_2.2-5.zip", repos = NULL, type="source")
library(relaimpo)

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
arrow::write_csv_arrow(referrals_sdoh, "data/referrals-sdoh.csv")

# ---- Explore social determinants ----
referrals_sdoh <- arrow::read_csv_arrow("data/referrals-sdoh.csv")

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
  tidy(exponentiate = TRUE, conf.int = TRUE)

#TODO: plot ORs with confidence intervals
coef_isolation <- ggcoef_model(mod_isolation, exponentiate = TRUE, conf.int = FALSE, return_data = TRUE)

coef_isolation$conf.low <- coef_isolation$estimate - coef_isolation$std.error * 1.96
coef_isolation$conf.high <- coef_isolation$estimate + coef_isolation$std.error * 1.96

ggcoef_plot(coef_isolation, exponentiate = TRUE)

#TODO: use relaimpo to quantify relative importance of predictors

#TODO in future, when we have several social determinants labelled: multinomial logistic regression?

# ---- People with multiple social determinants ----
referrals_sdoh <-
  referrals_sdoh |>
  mutate(total_sdoh = rowSums(across(`Social isolation`:`Financial difficulty`)))

referrals_sdoh |>
  count(total_sdoh) |>
  mutate(prop = n/sum(n))


# Co-occurrence matrix of social determinants
# Source: https://stackoverflow.com/a/10623146
referrals_sdoh_matrix <-
  referrals_sdoh |>
  select(`Social isolation`:`Financial difficulty`) |>
  as.matrix()
sdoh_co_occurence <- crossprod(referrals_sdoh_matrix)  # Same as: t(X) %*% X
diag(sdoh_co_occurence) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
sdoh_co_occurence

# Correlation matrix for social determinants
referrals_sdoh_cor <- round(cor(referrals_sdoh_matrix),2)

# Plot co-occurrence / correlation matrix
# Source: https://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# Use correlation between variables as distance
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

referrals_sdoh_cor |>
  reorder_cormat() |>
  get_lower_tri() |>
  as.table() |>
  as.data.frame() |>

  ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1,1), space = "Lab", na.value = NA,
    name="Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)
  ) +
  coord_fixed()
