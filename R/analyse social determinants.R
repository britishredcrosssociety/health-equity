# ---- Load and wrangle social determinants data ----
referrals_sdoh <- arrow::read_csv_arrow("data/referrals-sdoh.csv")

referrals_sdoh |>
  select(PSN, `Social isolation`:`Financial difficulty`) |>
  distinct()

# ---- Explore social determinants ----
# How many records are labelled as socially isolated?
referrals_sdoh |>
  count(`Social isolation`) |>
  mutate(prop = n/sum(n))

referrals_sdoh |>
  filter(`Social isolation` == 1) |>
  select(Notes) |>
  View()

# ---- Compare people with and without specific SDOHs ----
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

# ---- How many people experienced which social determinants? ----
sdoh_people_total <-
  referrals_sdoh |>
  select(PSN, `Social isolation`:`Financial difficulty`) |>
  distinct() |>

  pivot_longer(cols = -PSN, names_to = "Determinant") |>

  count(Determinant, value)

sdoh_people_total |>
  pivot_wider(names_from = value, values_from = n) |>
  mutate(prop = `1` / (`0` + `1`))



sdoh_people_total |> arrange(desc(count))

# ---- People with multiple social determinants ----
referrals_sdoh <-
  referrals_sdoh |>
  mutate(total_sdoh = rowSums(across(`Social isolation`:`Financial difficulty`)))

referrals_sdoh |>
  distinct(PSN, total_sdoh) |>
  count(total_sdoh) |>
  mutate(prop = n/sum(n))

# ---- Co-occurrence matrix of social determinants ----
# Source: https://stackoverflow.com/a/10623146
referrals_sdoh_matrix <-
  referrals_sdoh |>
  select(`Social isolation`:`Financial difficulty`) |>
  as.matrix()

sdoh_co_occurence <- crossprod(referrals_sdoh_matrix)  # Same as: t(X) %*% X
diag(sdoh_co_occurence) <- 0  # (b/c you don't count co-occurrences of an aspect with itself)

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

cor.test(referrals_sdoh_matrix)

Hmisc::rcorr(referrals_sdoh_matrix)

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

# ---- Explore the correlated / co-occurring social determinants ----
# Count the number of these social determinants for each record
referrals_sdoh_selected <-
  referrals_sdoh |>
  mutate(total_selected_sdoh = rowSums(across(c(`Social isolation`, Housing, Food, `Financial difficulty`))))

# How many people have more than one of: social isolation, housing, food, and financial difficulty?
referrals_sdoh_selected |>
  filter(total_selected_sdoh > 1) |>
  distinct(PSN) |>
  count()

referrals_sdoh_selected |>
  distinct(PSN, total_selected_sdoh) |>
  count(total_selected_sdoh)

# ---- Trends in social determinants ----
referrals_sdoh_trends <-
  referrals_sdoh |>
  filter(year(`Referral Date`) > 2017) |>
  mutate(month_year = ym(paste0(year(`Referral Date`), " ", month(`Referral Date`)))) |>
  relocate(month_year) |>

  select(month_year, PSN, `Social isolation`:`Financial difficulty`) |>
  distinct() |>
  pivot_longer(cols = -c(month_year, PSN), names_to = "Determinant") |>

  group_by(month_year, Determinant) |>
  summarise(count = sum(value)) |>
  ungroup() |>

  group_by(month_year) |>
  mutate(total = sum(count)) |>
  ungroup() |>

  mutate(prop = count / total)

referrals_sdoh_trends |>
  mutate(Determinant = fct_relevel(Determinant, "Medication", "Abuse", "Transport", "Food", "Housing", "Financial difficulty", "Social isolation")) |>

  ggplot(aes(x = month_year, y = prop, group = Determinant, colour = Determinant, fill = Determinant)) +
  geom_area() +
  # facet_wrap(~Determinant) +
  theme_classic()

# ---- How many people have different numbers of social determinants at different times? ----
referrals_sdoh |>
  select(PSN, `Social isolation`:`Financial difficulty`) |>
  pivot_longer(cols = -PSN, names_to = "Determinant") |>
  filter(value == 1) |>

  group_by(PSN, Determinant) |>
  summarise(count = sum(value))
