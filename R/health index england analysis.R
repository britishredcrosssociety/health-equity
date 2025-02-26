library(tidyverse)
library(healthyr)
library(geographr)
library(plotly)
library(DescTools)

# Trends in inequalities in health outcomes between Local Authorities
england_health_index |>
  group_by(year) |>
  summarise(gini = Gini(healthy_people_domain_score, unbiased = FALSE, na.rm = TRUE)) |>
  ungroup() |>

  ggplot(aes(x = year, y = gini, group = 1)) +
  geom_line() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 1))


# ----
england_averages <-
  england_health_index |>
  filter(year == max(year)) |>
  summarise(
    mean_places = mean(healthy_places_domain_score),
    mean_people = mean(healthy_people_domain_score)
  )

england_health_index |>
  filter(year == max(year)) |>
  left_join(lookup_ltla21_region21) |>

  ggplot(aes(x = healthy_places_domain_score, y = healthy_people_domain_score)) +
  geom_hline(yintercept = england_averages$mean_people, linetype = 2, colour = "grey70") +
  geom_vline(xintercept = england_averages$mean_places, linetype = 2, colour = "grey70") +
  geom_point(aes(text = ltla21_name)) +
  geom_smooth(method = "lm")

ggplotly()

# ---- Trends in health outcomes and social determinants ----
# Local Authorities whose health outcomes are worse in 2021 than in 2015
england_health_index |>
  select(ltla21_code, year, healthy_people_domain_score) |>
  filter(year %in% c(min(year), max(year))) |>
  pivot_wider(names_from = "year", values_from = healthy_people_domain_score) |>
  filter(`2021` < `2015`)

#TODO: Filter LAs whose avoidable mortality worsened over time (as a proxy for increasing health ineqs)

england_health_index |>
  left_join(lookup_ltla21_region21) |>
  filter(region21_name == "London") |>

  ggplot(aes(x = healthy_places_domain_score, y = healthy_people_domain_score, group = ltla21_code)) +
  geom_path(
    aes(
      colour = ltla21_code,
      text = str_glue("{ltla21_name} in {year}:\nHealthy places score: {healthy_places_domain_score}\nHealthy people score: {healthy_people_domain_score}")
    ),
    arrow = arrow(type = "closed", length = unit(1, "mm")),
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 100, linetype = 2, colour = "grey70") +
  geom_vline(xintercept = 100, linetype = 2, colour = "grey70") +
  facet_wrap(~region21_name) +
  theme_classic()

ggplotly(tooltip = "text")
