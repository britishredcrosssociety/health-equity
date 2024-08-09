# Map barriers to access hotspots
# And numbers of income-deprived people in these hotspots
library(tidyverse)
library(compositr)
library(geographr)
library(brcplot) # Needs to be loaded locally
library(DEPAHRI)
library(IMD)
library(sf)
library(statswalesr)
library(ggtext)
library(readODS)

# ---- Calculate normalised GP access indicators ----
england_gp_access <- 
  IMD::imd2019_england_lsoa11_indicators |> 
  mutate(gp_access_score = DEPAHRI::calculate_score(`Road distance to a GP surgery indicator (km)`)) |> 
  select(area_code = lsoa11_code, gp_access_score) |> 
  mutate(
    gp_access_quintile = ntile(-gp_access_score, n = 5),
    gp_access_decile = ntile(-gp_access_score, n = 10)
  )

scotland_gp_access <- 
  IMD::imd2020_dz11_scotland_indicators |> 
  mutate(gp_access_score = DEPAHRI::calculate_score(PT_GP)) |> 
  select(area_code = dz11_code, gp_access_score = PT_GP) |> 
  mutate(
    gp_access_quintile = ntile(-gp_access_score, n = 5),
    gp_access_decile = ntile(-gp_access_score, n = 10)
  )

wales_gp_access_raw <- statswales_get_dataset("wimd1909")

wales_gp_access <-
  wales_gp_access_raw |>
  as_tibble() |>
  filter(Indicator_ItemName_ENG == "Average public return travel time to a GP surgery (minutes)") |>
  filter(LSOA_Code != "Wales") |>
  mutate(gp_access_score = DEPAHRI::calculate_score(Data)) |> 
  select(area_code = LSOA_Code, gp_access_score) |> 
  mutate(
    gp_access_quintile = ntile(-gp_access_score, n = 5),
    gp_access_decile = ntile(-gp_access_score, n = 10)
  )

# Combine into GB-wide index
gb_gp_access <- bind_rows(england_gp_access, wales_gp_access, scotland_gp_access)

write_csv(gb_gp_access, "analysis/marmot/outputs/gp-access-eng-sco-wal.csv")

# ---- Aggregate into Local Authorities (for map) ----
# Income deprivation by age in Wales
# Source: https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Welsh-Index-of-Multiple-Deprivation/WIMD-Indicator-Data-By-Age/Income-Deprivation-By-Age
tf <- download_file("https://statswales.gov.wales/Download/File?fileId=636", ".ods")

wales_income_deprivation_raw <- read_ods(tf, sheet = "Income_LSOA", skip = 3)

wales_income_deprivation <- 
  wales_income_deprivation_raw |> 
  select(area_code = `LSOA code`, people_income_deprived = `Total number of people in income deprivation (2016-17)`)

england_income_deprivation <- 
  IMD::imd2019_england_lsoa11_indicators |> 
  select(area_code = lsoa11_code, people_income_deprived = `Income Domain numerator`)

scotland_income_deprivation <- 
  IMD::imd2020_dz11_scotland_indicators |> 
  select(area_code = dz11_code, people_income_deprived = Income_count)

gb_income_deprivation <- 
  bind_rows(england_income_deprivation, wales_income_deprivation, scotland_income_deprivation)

# Small areas to LAs lookup
lookup_areas_ltla <- 
  bind_rows(
    geographr::lookup_lsoa11_ltla21 |> 
      select(area_code = lsoa11_code, ltla_code = ltla21_code),
    
    geographr::lookup_dz11_iz11_ltla20 |> 
      select(area_code = dz11_code, ltla_code = ltla20_code)
  )

# Aggregate scores to LAs
gb_gp_access_ltla <- 
  gb_gp_access |> 
  left_join(lookup_areas_ltla) |> 
  left_join(gb_income_deprivation) |> 
  compositr::calculate_extent(gp_access_score, higher_level_geography = ltla_code, population = people_income_deprived)

# ---- Analyse reachability ----
n_least_reachable <- 
  gb_gp_access |> 
  filter(gp_access_decile == 1) |> 
  count()

n_least_reachable

scales::percent(n_least_reachable$n / nrow(gb_gp_access), accuracy = 0.01)

# Number of income-deprived people living in areas furthest from GP surgeries
gb_gp_access |> 
  left_join(
    IMD::imd2019_england_lsoa11_indicators |> 
      select(area_code = lsoa11_code, people_income_deprived = `Income Domain numerator`)
  ) |> 
  
  group_by(gp_access_quintile) |> 
  summarise(total_income_deprived = sum(people_income_deprived, na.rm = TRUE))

# How rural/urban are they?
ruc_gb <- bind_rows(
  geographr::ruc11_lsoa11 |> select(area_code = lsoa11_code, ruc),
  geographr::ruc16_dz11 |> select(area_code = dz11_code, ruc)
)

ruc_labels <- 
  tribble(
    ~gp_access_quintile, ~n, ~label,
    5, 2000, "Urban",
    5, 6000, "Rural"
  )

gb_gp_access |> 
  left_join(ruc_gb) |> 
  count(ruc, gp_access_quintile) |> 
  mutate(prop = n / sum(n)) |> 
  
  mutate(gp_access_quintile = fct_rev(factor(gp_access_quintile))) |> 
  
  ggplot(aes(x = gp_access_quintile, y = n)) +
  geom_col(aes(fill = ruc)) +
  geom_text(
    data = ruc_labels,
    aes(label = label),
    colour = "white",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_x_discrete(labels = rev(c("Furthest from GPs", 2, 3, 4, "Nearest to GPs"))) +
  scale_y_continuous(labels = scales::comma, position = "right") +
  scale_fill_manual(values = c(brc_colours$green, brc_colours$grey)) +
  theme_brc() +
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(size = 14)
  ) +
  labs(
    title = str_wrap(str_glue("Neighbourhoods furthest from GP practices are more likely to be <span style='color:{brc_colours$green}; font-weight:bold'>rural</span> compared to those nearest to GPs. But around half of the neighbourhoods furthest from GPs are <span style='color:{brc_colours$grey}; font-weight:bold'>urban areas</span>"), 90),
    # subtitle = str_wrap("Change in the percentage of people who could reach a GP by public transport / walking within 30 minutes, 2014-19", 57),
    x = NULL,
    y = "Number of neighbourhoods",
    fill = NULL,
    caption = "British Red Cross analysis of MHCLG, Welsh Government, and Scottish Government data\nProximity to GPs is based on road distance to a GP surgery (England), and travel time to a GP surgery by public transport (Wales and Scotland)"
  )

ggsave("analysis/marmot/gp access by rural and urban.png", width = 200, height = 120, units = "mm")

# ---- How many are in the Core 20? ----
# Make GB IMD
imd_gb <- 
  bind_rows(
    IMD::imd_england_lsoa |> select(area_code = lsoa_code, IMD_decile),
    IMD::imd_wales_lsoa |> select(area_code = lsoa_code, IMD_decile),
    IMD::imd_scotland_dz|> select(area_code = dz_code, IMD_decile)
  ) |> 
  mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Other area"))

gp_imd <- 
  gb_gp_access |> 
  left_join(imd_gb)

gp_imd |> 
  count(gp_access_quintile, Core20) |> 
  
  group_by(gp_access_quintile) |> 
  mutate(prop = n / sum(n))

# ---- Which are the areas in the Core20 with the greatest geographical barriers to GPs? ----
gp_imd_ltla <- 
  gp_imd |> 
  filter(IMD_decile <= 2 & gp_access_quintile == 1) |> 
  left_join(lookup_areas_ltla)

# Make a list of the LSOAs in each LA
# names_codes_lsoa11 <-
#   geographr::boundaries_lsoa11 |>
#   sf::st_drop_geometry() |> 
#   rename(area_name = lsoa11_name, area_code = lsoa11_code)

# Recognisable names for datasets
msoa11_names <- read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv") |> select(msoa11_code = msoa11cd, msoa11_recognisable_name = msoa11hclnm)

lsoa11_msoa11_names <-
  geographr::lookup_lsoa11_msoa11 |>
  left_join(msoa11_names) |>
  select(area_name = msoa11_recognisable_name, area_code = lsoa11_code)

names_codes_dz11 <-
  geographr::boundaries_dz11 |>
  sf::st_drop_geometry() |> 
  rename(area_name = dz11_name, area_code = dz11_code) |> 
  mutate(area_name = str_remove(area_name, " - [0-9]{2}"))

names_codes_areas <- bind_rows(lsoa11_msoa11_names, names_codes_dz11) |> 
  drop_na()

gp_imd_ltla_labels <- 
  gp_imd_ltla |> 
  left_join(names_codes_areas) |> 
  distinct(ltla_code, area_name) |> 
  group_by(ltla_code) |> 
  summarise(
    label = str_flatten(area_name, collapse = "<br/>")
  )

# Count number of Core20 LSOAs in each LA with greatest barriers, add labels, and save
ltla_names <- 
  geographr::boundaries_ltla21 |> 
  st_drop_geometry() |> 
  rename(ltla_code = ltla21_code, ltla_name = ltla21_name)

gp_imd_ltla |> 
  count(ltla_code, sort = TRUE, name = "Number of Core20 areas with geographical barriers to GPs") |> 
  left_join(ltla_names) |> 
  left_join(gp_imd_ltla_labels) |> 
  write_csv("analysis/marmot/outputs/core20 areas with barriers to GPs.csv")

# ---- Boundaries in Great Britain ----
boundaries_lsoa <- bind_rows(
  geographr::boundaries_lsoa11,
  geographr::boundaries_dz11 |> rename(lsoa11_code = dz11_code, lsoa11_name = dz11_name)
)

boundaries_health <- bind_rows(
  geographr::boundaries_icb22 |> rename(health_board_code = icb22_code, health_board_name = icb22_name),
  geographr::boundaries_lhb22 |> rename(health_board_code = lhb22_code, health_board_name = lhb22_name),
  geographr::boundaries_ltla19 |> filter(str_detect(ltla19_code, "^S")) |> rename(health_board_code = ltla19_code, health_board_name = ltla19_name)
)

boundaries_ltla <- 
  geographr::boundaries_ltla21 |> 
  filter(!str_detect(ltla21_code, "^N")) |> 
  rename(ltla_code = ltla21_code)

# ---- Make LA-level map ----
boundaries_ltla |> 
  left_join(gb_gp_access_ltla) |> 
  
  ggplot() +
  geom_sf(
    aes(fill = extent)
  ) +
  theme_brc_map() +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = "Reachability of GPs",
    caption = "British Red Cross analysis of MHCLG, Welsh Government and Scottish Government data"
  )

# ---- Make map ----
# Run this next line to stop an error in st_join()
sf_use_s2(FALSE)

least_reachable_gps <- 
  boundaries_lsoa |> 
  left_join(gb_gp_access, by = join_by(lsoa11_code == area_code)) |> 
  filter(gp_access_decile == 10)

# Save data for interactive map in Flourish
# Population-weighted centroids (LSOA 2011)
centroids_lsoa11 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_PWC_in_England_and_Wales_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# Convert geometry into lat and long
centroids_lsoa11 <- 
  centroids_lsoa11 |> 
  tidyr::extract(geometry, into = c('Longitude', 'Latitude'), '\\((.*),(.*)\\)', conv = TRUE) |> 
  select(area_code = lsoa11cd, area_name = lsoa11nm, Longitude, Latitude)

centroids_dz11 <- read_sf("analysis/marmot/data/SG_DataZone_Cent_2011.shp")

# Convert geometry into lat and long
centroids_dz11 <- 
  centroids_dz11 |> 
  st_transform(crs = 4326) |> 
  tidyr::extract(geometry, into = c('Longitude', 'Latitude'), '\\((.*),(.*)\\)', conv = TRUE) |> 
  select(area_code = DataZone, area_name = Name, Longitude, Latitude)

centroids_lsoa11 |> 
  bind_rows(centroids_dz11) |> 
  left_join(gb_gp_access) |> 
  filter(gp_access_quintile == 1) |> 
  select(area_code, Name = area_name, Longitude, Latitude) |> 
  write_csv("analysis/marmot/outputs/areas furthest from GPs.csv")

# least_reachable_gps |> 
#   ggplot() +
#   geom_sf(
#     fill = brc_colours$red_dunant,
#     colour = brc_colours$red_dunant
#   ) +
#   geom_sf(
#     data = boundaries_health,
#     colour = brc_colours$black_shadow,
#     fill = NA
#   ) +
#   theme_brc_map() +
#   theme(
#     plot.title.position = "plot"
#   ) +
#   labs(
#     title = "Reachability of GPs",
#     caption = "British Red Cross analysis of MHCLG, Welsh Government and Scottish Government data"
#   )
