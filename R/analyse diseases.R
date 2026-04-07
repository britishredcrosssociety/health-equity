library(tidyverse)
library(jsonlite)

diseases <-
  list.files("data/Disease classification/", full.names = TRUE) |>
  map(\(path) read_csv(path, col_select = c(`Referral Date`, disease))) |>
  list_rbind()


# t=read_csv("data/Disease classification/disease0.csv", col_select = c(`Referral Date`, disease))

# diseases |>
#   mutate(disease = str_remove_all(disease, "\\[|\\]|\\(|\\)|\\'")) |>
#   separate_wider_delim(disease, delim = ", ", names_sep = "_")   #names = c("disease", "probability"))

# Convert Python-like string to valid JSON string
convert_to_json <- function(x) {
  x <- gsub("\\(", "[", x)
  x <- gsub("\\)", "]", x)
  x <- gsub("'", "\"", x)
  # x <- gsub('""', '"', x)
  x <- gsub('"', '\"', x)
  x
}

diseases_parsed <-
  diseases |>
  mutate(disease = convert_to_json(disease)) |>
  # mutate(disease = case_when(
  #   disease == 'alzheimer \"s' ~ 'alzheimers',
  #   .default = disease
  # )) |>
  mutate(disease = str_replace(disease, " \"s", "'s")) |>
  mutate(
    disease_parsed = map(disease, \(json_string) fromJSON(json_string, flatten=TRUE))
  ) |>
  unnest(disease_parsed)

fromJSON('[["alzheimer s", 0.3901746]]')



