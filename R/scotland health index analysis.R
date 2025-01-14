library(tidyverse)
library(geographr)
library(demographr)
library(healthindexscotland)
library(ggbeeswarm)
library(brcplot)
library(plotly)
library(GGally)
library(IMD)
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
scotland_map_data <-
  geographr::boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^S")) |>

  left_join(
    scotland_health_index |> select(ltla24_code, healthy_places_quantile),
    by = join_by(ltla21_code == ltla24_code)
  ) |>

  # Convert decile to quintile
  mutate(healthy_places_quantile = ceiling(healthy_places_quantile / 2))

scotland_map_data |>
  ggplot() +
  geom_sf(aes(fill = factor(healthy_places_quantile))) +
  scale_fill_brewer(type="seq", palette = "Reds", direction = -1, labels = c("Worst 20%", "2", "3", "4", "Best 20%")) +
  theme_brc_map() +
  labs(
    fill = "Social determinants"
  )

ggsave("outputs/scotland-healthy-places.png", height = 200, width = 200, units = "mm")

# Map highlighting Argyll and W. Dunbartonshire
scotland_map_data |>
  mutate(ltla_subset = if_else(str_detect(ltla21_name, "Argyll|West Dunbarton"), ltla21_name, "Other")) |>
  ggplot() +
  geom_sf(aes(fill = ltla_subset), show.legend = FALSE) +
  scale_fill_manual(values = c(brc_colours$sky, "white", brc_colours$red_dunant)) +
  theme_brc_map()

ggsave("outputs/scotland-map-argyll-dunbartonshire.png", height = 200, width = 200, units = "mm")

# ---- Which Council Areas have better/worse health outcomes (People) than expected given their social determinants (Places)? ----
# ... and why: i.e. which subdomains do well/badly?
# Plot correlations between domains
scotland_health_index |>
  select(healthy_places_rank, healthy_people_rank, healthy_lives_rank) |>
  ggpairs(columns = 1:3, lower = list(continuous = "smooth"))

# Plot Healthy Places x Healthy People
scotland_health_index |>
  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code)) |>

  ggplot(aes(x = healthy_places_score, y = healthy_people_score)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(text = ltla21_name)) +
  geom_smooth(method = "lm")

# Use interactive plot to manually check which Council Areas rank where
ggplotly()

# West Dunbartonshire (S12000039) has worse-than-expected health outcomes, even for its poor social determinants
#... explore why: what subdomains of Healthy People and Healthy Places are driving this?
# Conversely, Edinburgh (S12000036) has much better health outcomes than expected, given its level of social determinaints
#... explore why
subdomains_and_outcomes <-
  scotland_health_index_subdomains |>
  select(ltla24_code, starts_with("people_"), starts_with("places_")) |>
  pivot_longer(cols = -ltla24_code, names_to = "subdomain", values_to = "score") |>

  # Prettify subdomain names
  mutate(
    subdomain = str_remove(subdomain, "^places_|^people_") |>
      str_replace_all("_", " ") |>
      str_to_sentence()
  ) |>
  mutate(subdomain = fct_rev(subdomain)) |>

  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code))

# Plot people and places subdomain scores, highlighting Argyll & Bute and W. Dunbartonshire
# This will be used to write a narrative analysis; the chart won't be published so not putting in effort to make it look nice
subdomains_and_outcomes |>
  ggplot(aes(x = score, y = subdomain)) +

  geom_vline(xintercept = 0, linetype = 2) +  # Show line for average
  geom_beeswarm(aes(text = ltla21_name)) +
  # Plot West Dunbartonshire
  geom_beeswarm(data = subdomains_and_outcomes |> filter(ltla24_code == "S12000039"), colour = "red") +
  # Plot Argyll & Bute
  geom_beeswarm(data = subdomains_and_outcomes |> filter(ltla24_code == "S12000035"), colour = "green") +

  scale_x_continuous(
    breaks = c(-2.7, 0, 1.4),
    labels = c("← Worse than average", "Average", "Better than average →"),
    position = "top"
  )


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
ggplotly()

# ---- Compare health outcomes for places with poor social determinants ----
# Wrangle Healthy People subdomains
subdomains_people <-
  scotland_health_index_subdomains |>
  select(ltla24_code, starts_with("people_")) |>
  pivot_longer(cols = -ltla24_code, names_to = "subdomain", values_to = "score") |>

  # Prettify subdomain names
  mutate(
    subdomain = str_remove(subdomain, "^people_") |>
      str_replace_all("_", " ") |>
      str_to_sentence()
  ) |>
  mutate(subdomain = fct_rev(subdomain)) |>

  left_join(council_area_names, by = join_by(ltla24_code == ltla21_code))

subdomains_people |>
  mutate(highlighted_places = if_else(str_detect(ltla21_name, "Argyll|West Dunbarton"), ltla21_name, "no")) |>

  ggplot(aes(x = score, y = subdomain)) +

  geom_vline(xintercept = 0, linetype = 2) +  # Show line for average
  geom_beeswarm(
    aes(text = ltla21_name, colour = highlighted_places),
    show.legend = FALSE
  ) +

  scale_x_continuous(
    breaks = c(-1.4, 0, 1.8),
    labels = c("← Worse than average", "Average", "Better than average →"),
    position = "top"
  ) +
  scale_colour_manual(values = c(brc_colours$sky, brc_colours$grey_fog, brc_colours$red_dunant)) +

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
    title = "Health status and outcomes",
    x = NULL,
    y = NULL
  )

ggsave("outputs/scotland-healthy-people-subdomains.png", width = 200, height = 100, units = "mm")

ggplotly()

# Calcualte mean absolute deviation for each subdomain
#... to work out whether an area is properly above/below average
subdomains_people |>
  group_by(subdomain) |>
  summarise(mad = mad(score, center = mean(subdomain)))

# ---- Social infrastructure in Argyll and W. Dunbartonshire ----
cni_comparison <-
  IMD::cni_scotland_iz11 |>
  left_join(
    geographr::lookup_dz11_iz11_ltla20 |> distinct(iz11_code, ltla20_name)
  ) |>
  left_join(demographr::population21_iz11, by = join_by(iz11_code))

cni_comparison |>
  mutate(ltla_subset = if_else(str_detect(ltla20_name, "Argyll|West Dunbarton"), ltla20_name, "Other")) |>
  select(ltla_subset, ends_with("rank")) |>
  pivot_longer(cols = ends_with("rank"), names_to = "domain", values_to = "rank") |>

  ggplot(aes(x = rank, y = ltla_subset)) +
  geom_beeswarm() +
  facet_wrap(~domain)

cni_ltla <-
  cni_comparison |>
  compositr::calculate_extent(
    higher_level_geography = ltla20_name,
    var = `Community Needs Index score`,
    population = total_population,
    invert_percentiles = TRUE
  )

civic_assets_ltla <-
  cni_comparison |>
  compositr::calculate_extent(
    higher_level_geography = ltla20_name,
    var = `Civic Assets score`,
    population = total_population,
    invert_percentiles = TRUE
  )
