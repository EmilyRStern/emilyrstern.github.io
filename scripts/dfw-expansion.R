library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(ggplot2)
library(scales)
library(stringr)
library(ggrepel)
options(tigris_use_cache = TRUE)

# ---------------------------
# 1) Get tabular counts (no geometry)
# ---------------------------
tx10 <- get_decennial(
  geography = "county",
  state = "TX",
  year = 2010,
  variables = "P001001",   # 2010 SF1 total pop
  sumfile  = "sf1",
  geometry = FALSE
) %>%
  transmute(GEOID, NAME, pop_2010 = value)

tx20 <- get_decennial(
  geography = "county",
  state = "TX",
  year = 2020,
  variables = "P1_001N",   # 2020 PL total pop
  dataset  = "pl",
  geometry = FALSE
) %>%
  transmute(GEOID, NAME, pop_2020 = value)

county_pop <- tx10 %>%
  left_join(tx20, by = c("GEOID", "NAME")) %>%
  mutate(
    pop_change_20_10 = pop_2020 - pop_2010,
    pct_change_20_10 = (pop_change_20_10 / pop_2010) * 100
  )

# ---------------------------
# 2) Get TX county geometry once and join
# ---------------------------
tx_geo <- counties(state = "TX", year = 2020, cb = TRUE, class = "sf") %>%
  select(GEOID, geometry)  # keep it light

county_pop_sf <- tx_geo %>%
  left_join(county_pop, by = "GEOID")

# ---------------------------
# 3) DFW subset
# ---------------------------
dfw_counties <- c(
  "Collin County, Texas", "Dallas County, Texas", "Denton County, Texas",
  "Ellis County, Texas", "Hood County, Texas", "Hunt County, Texas",
  "Johnson County, Texas", "Kaufman County, Texas", "Parker County, Texas",
  "Rockwall County, Texas", "Somervell County, Texas",
  "Tarrant County, Texas", "Wise County, Texas"
)

dfw_pop <- county_pop_sf %>%
  filter(NAME %in% dfw_counties)

# ---------------------------
# 4) National average % change (2010→2020)
# ---------------------------
us_2010 <- get_decennial(
  geography = "us",
  year = 2010,
  variables = "P001001",
  sumfile = "sf1"
) %>% pull(value)

us_2020 <- get_decennial(
  geography = "us",
  year = 2020,
  variables = "P1_001N",
  dataset = "pl"
) %>% pull(value)

nat_pct_change_20_10 <- (us_2020 - us_2010) / us_2010 * 100

# ---------------------------
# 5) MAP: % change (fill) + absolute increase (labels)
# ---------------------------

  dfw_counties <- c(
    "Collin County, Texas", "Dallas County, Texas", "Denton County, Texas",
    "Ellis County, Texas", "Hood County, Texas", "Hunt County, Texas",
    "Johnson County, Texas", "Kaufman County, Texas", "Parker County, Texas",
    "Rockwall County, Texas", "Somervell County, Texas",
    "Tarrant County, Texas", "Wise County, Texas"
  )

# --- Major DFW cities with approximate coordinates ---
major_cities <- data.frame(
  city = c("Dallas", "Fort Worth"),
  lon  = c(-96.7970, -97.3308),
  lat  = c(32.7767, 32.7555)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

major_cities_xy <- major_cities %>%
  cbind(st_coordinates(.))

# Base (all TX) + Highlight (DFW only)
tx_base <- county_pop_sf %>% select(GEOID, NAME, geometry)  # no data mapped to fill
dfw_pop  <- county_pop_sf %>% filter(NAME %in% dfw_counties)


# CPAL teal palette
pal_teal <- cpal_colors("teal_seq_5")

dfw_map <- ggplot() +
  geom_sf(data = tx_base, fill = "#F0F0F0", color = "white", linewidth = 0.2) +
  geom_sf(data = dfw_pop, aes(fill = pct_change_20_10), color = "white", linewidth = 0.4) +
  geom_sf(
    data = major_cities,
    shape = 21,
    fill = cpal_colors("gold"),
    color = "black",
    size = 3,
    stroke = 0.7
  ) +
  geom_text_repel(
    data = major_cities_xy,
    aes(x = X, y = Y, label = city),
    fontface = "bold",
    size = 3,
    nudge_y = 1,
    color = "black"
  ) +
  scale_fill_gradientn(
    colors = pal_teal,
    name = "% change (2010–2020)",
    labels = scales::label_number(accuracy = 0.1)
  ) +
  coord_sf() +
  labs(
    title = "Population Change (2010–2020): Dallas–Fort Worth Metro Counties",
    caption = "Source: U.S. Census (2010 SF1, 2020 PL Redistricting)"
  ) +
  theme_void(base_size = 12) +
  theme_cpal_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# ---------------------------
# 6) BAR: DFW % change vs U.S. average
# ---------------------------
bar_chart <- dfw_pop %>%
  st_drop_geometry() %>%
  mutate(County = str_remove(NAME, ", Texas")) %>%
  ggplot(aes(x = reorder(County, pct_change_20_10), y = pct_change_20_10)) +
  geom_col(fill = cpal_colors("midnight")) +
  geom_hline(yintercept = nat_pct_change_20_10, linetype = "dashed", color = cpal_colors("gold"), linewidth = 1) +
  annotate(
    "label",
    x = Inf, y = nat_pct_change_20_10,
    label = paste0("U.S. Avg: ", round(nat_pct_change_20_10, 1), "%"),
    hjust = 1.05, vjust = -0.4, label.size = 0, color = "gray10"
  ) +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Population % Change (2010–2020): DFW vs U.S. Average",
    x = NULL, y = NULL,
    caption = "Source: U.S. Census (2010 SF1, 2020 PL)"
  ) +
  theme_cpal_classic()

ggsave("../assets/img/dfw-popchange-map.png", dfw_map, width = 10, height = 6, dpi = 300)
ggsave("../assets/img/dfw-popchange-bar.png", bar_chart, width = 8, height = 6, dpi = 300)
