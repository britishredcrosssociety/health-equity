library(tidyverse)
library(geographr)
library(healthindexscotland)
library(ggbeeswarm)
library(GGally)
library(brcplot)
library(sf)

# healthindexscotland::scotland_health_index |>
#   select(ends_with("_rank")) |>
#   pivot_longer(cols = everything(), names_to = "domain", values_to = "rank") |>
#   mutate(domain = str_remove(domain, "_rank"))

council_area_names <-
  geographr::boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^S")) |>
  st_drop_geometry()

# ---- Which Council Areas are lowest-performing on social determinants / Healthy Places? ----
# Lower quantile (and lower rank) = worse performance

scotland_health_index |>
  select(ltla24_code, healthy_places_quantile) |>
  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code)) |>
  arrange(healthy_places_quantile)

# Make a map
geographr::boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^S")) |>

  left_join(
    scotland_health_index |> select(ltla24_code, healthy_places_quantile),
    by = join_by(ltla21_code == ltla24_code)
  ) |>

  # Convert decile to quintile
  mutate(healthy_places_quantile = ceiling(healthy_places_quantile / 2)) |>

  ggplot() +
  geom_sf(aes(fill = factor(healthy_places_quantile))) +
  scale_fill_brewer(type="seq", palette = "Reds", direction = -1, labels = c("Worst 20%", "2", "3", "4", "Best 20%")) +
  theme_brc_map() +
  labs(
    fill = "Social determinants"
  )

ggsave("outputs/scotland-healthy-places.png", height = 200, width = 200, units = "mm")

# ---- Which Council Areas have better/worse health outcomes (People) than expected given their social determinants (Places)? ----
# ... and why: i.e. which subdomains do well/badly?
scotland_health_index |>
  select(healthy_places_rank, healthy_people_rank, healthy_lives_rank) |>
  # select(ends_with("_rank")) |>
  ggpairs(columns = 1:3, lower = list(continuous = "smooth"))

# ---- Sub-domain scorecard for the worst-performing Council Area on social determinants (Places) ----
# Which Council Area scores worst overall for the Healthy Places domain?
# We'll highligh this place on the chart
scotland_health_index |>
  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code)) |>
  filter(healthy_places_rank == 1) |>
  select(ltla24_code, ltla21_name)
#--> Glasgow (S12000049)

# Wrangle Healthy Places subdomains
subdomains <-
  scotland_health_index_subdomains |>
  select(ltla24_code, starts_with("places_")) |>
  pivot_longer(cols = -ltla24_code, names_to = "subdomain", values_to = "score") |>

  # Prettify subdomain names
  mutate(
    subdomain = str_remove(subdomain, "^places_") |>
      str_replace_all("_", " ") |>
      str_to_sentence()
  ) |>
  mutate(subdomain = fct_rev(subdomain)) |>

  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code))

subdomains |>
  ggplot(aes(x = score, y = subdomain)) +

  geom_vline(xintercept = 0, linetype = 2) +  # Show line for average
  geom_beeswarm(aes(text = ltla21_name)) +

  scale_x_continuous(
    breaks = c(-2.7, 0, 1.4),
    labels = c("← Worse than average", "Average", "Better than average →"),
    position = "top"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = brc_colours$black_shadow),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +

  labs(
    title = "Scores for each social determinant of health",
    x = NULL,
    y = NULL
  )

ggsave("outputs/scotland-healthy-places-subdomains.png", width = 200, height = 100, units = "mm")

# Use interactive plot to manually check which Council Areas rank where
library(plotly)
ggplotly()
