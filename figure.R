library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(stringi)

dat <- read.csv("scbd-ort-nr7-data-indicator-data-10-Sep-2026-15-39.csv") %>%
  mutate(Government = stri_trans_nfc(Government)) # fix NFD/NFC unicode mismatches (e.g. "Türkiye")

dat %>% group_by(Government) %>% count()
# 130 countries represented in the data

# Filter out national indicators
headline_indicators <- dat %>%
  filter(Indicator.Type != "national")

library(lubridate)

# Remove all indicators published after the deadline (which were not included in global report)
dat <- dat %>%
  mutate(
    published_date = dmy_hm(Published.on),
    published_date = as.Date(published_date)
  ) %>%
  filter(published_date <= as.Date("2026-03-07"))
nrow(dat)
# 113 countries in the filtered publication dates data

length(unique(dat$Government))

# summarise headline indicators by government and indicator code to get max and min values
indicators_sum <- dat %>%
  drop_na(Value) %>%
  group_by(Government, Indicator.Code) %>%
  summarise(max = max(Value, na.rm = TRUE), min = min(Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Government) %>%
  count(name = "indicators_count") %>%
  ungroup()

print(indicators_sum)
write.csv(indicators_sum, "indicators_summary.csv", row.names = FALSE)

# Histogram of indicators reported per country
hist_plot <- ggplot(indicators_sum, aes(x = indicators_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of indicators reported per country",
    x = "Indicators reported",
    y = "Number of countries"
  )

print(hist_plot)
ggsave("indicators_histogram.png", plot = hist_plot, width = 8, height = 5, dpi = 300)

# remove non-cbd entities from the world map, so they render as grey (not missing) on the map
non_cbd_entities <- c(
  "United States of America",
  "Vatican",
  "Taiwan",
  "Kosovo",
  "Somaliland",
  "N. Cyprus",
  "N. Mariana Is.",
  "U.S. Virgin Is.",
  "Guam",
  "American Samoa",
  "Puerto Rico",
  "S. Geo. and the Is.",
  "Br. Indian Ocean Ter.",
  "Saint Helena",
  "Pitcairn Is.",
  "Anguilla",
  "Falkland Is.",
  "Cayman Is.",
  "Bermuda",
  "British Virgin Is.",
  "Turks and Caicos Is.",
  "Montserrat",
  "Jersey",
  "Guernsey",
  "Isle of Man",
  "Aruba",
  "Curaçao",
  "W. Sahara",
  "St. Pierre and Miquelon",
  "Wallis and Futuna Is.",
  "St-Martin",
  "St-Barthélemy",
  "Fr. Polynesia",
  "New Caledonia",
  "Fr. S. Antarctic Lands",
  "Åland",
  "Greenland",
  "Faeroe Is.",
  "Macao",
  "Hong Kong",
  "Indian Ocean Ter.",
  "Heard I. and McDonald Is.",
  "Norfolk Island",
  "Ashmore and Cartier Is.",
  "Sint Maarten",
  "Antarctica",
  "Siachen Glacier"
)

# Canonical country names used by the world map (rnaturalearth's short-form
# names), mapped from the CBD data's and WDI's different spellings/long forms.
# Without this, joins silently fail and countries render as "missing" (grey)
# even though they reported data - e.g. DRC, Eswatini, Tanzania, Türkiye.
country_name_map <- c(
  "United States" = "United States of America",
  "Czech Republic" = "Czechia",
  "Republic of Korea" = "South Korea",
  "Democratic Republic of the Congo" = "Dem. Rep. Congo",
  "Eswatini" = "eSwatini",
  "Dominican Republic" = "Dominican Rep.",
  "Equatorial Guinea" = "Eq. Guinea",
  "Gambia (the)" = "Gambia",
  "Micronesia (Federated States of)" = "Micronesia",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Vincent and the Grenadines" = "St. Vin. and Gren.",
  "South Sudan" = "S. Sudan",
  "Türkiye" = "Turkey",
  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
  "United Republic of Tanzania" = "Tanzania",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Viet Nam" = "Vietnam",
  "Bahamas (The)" = "Bahamas",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Brunei Darussalam" = "Brunei",
  "Marshall Islands" = "Marshall Is.",
  "Cook Islands" = "Cook Is.",
  "Lao People's Democratic Republic" = "Laos",
  "State of Palestine" = "Palestine",
  # WDI-side spellings that differ from the CBD-side ones above
  "Korea, Rep." = "South Korea",
  "Egypt, Arab Rep." = "Egypt",
  "Iran, Islamic Rep." = "Iran",
  "Venezuela, RB" = "Venezuela",
  "Congo, Dem. Rep." = "Dem. Rep. Congo",
  "Congo, Rep." = "Congo",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Bahamas, The" = "Bahamas",
  "Gambia, The" = "Gambia",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Lao PDR" = "Laos",
  "St. Vincent and the Grenadines" = "St. Vin. and Gren.",
  "Turkiye" = "Turkey",
  "West Bank and Gaza" = "Palestine",
  "Slovak Republic" = "Slovakia",
  "Yemen, Rep." = "Yemen",
  "Antigua and Barbuda" = "Antigua and Barb.",
  "Bosnia and Herzegovina" = "Bosnia and Herz.",
  "Central African Republic" = "Central African Rep.",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "St. Lucia" = "Saint Lucia",
  "Sao Tome and Principe" = "São Tomé and Principe",
  "Solomon Islands" = "Solomon Is.",
  "Somalia, Fed. Rep." = "Somalia",
  "Syrian Arab Republic" = "Syria"
)

# Map names to map names
country_counts <- indicators_sum %>%
  mutate(Government = recode(Government, !!!country_name_map)) %>%
  print()


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  mutate(name = stri_trans_nfc(name), cbd_party = if_else(name %in% non_cbd_entities, "No", "Yes"))
print(world)

map_data <- world %>%
  left_join(country_counts %>% select(Government, indicators_count), by = c("name" = "Government")) %>%
  mutate(
    indicators_count = if_else(
      cbd_party == "No",
      NA_integer_,
      coalesce(indicators_count, 0L)
    )
  )

map_data %>% filter(cbd_party == "No") %>% select(name, indicators_count) 

log_breaks <- c(0, 1, 2, 5, 10, 20, 50, 100, 200)
log_breaks <- log_breaks[log_breaks <= max(map_data$indicators_count, na.rm = TRUE)]

map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = indicators_count), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey55",
    name = "Indicators\n(log scale)",
    trans = "log1p",
    breaks = log_breaks
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  ) +
  labs(
    title = "Indicators reported by country",
    caption = "Non-CBD entities shown in dark grey"
  )

library(patchwork)

# EU reports as a single CBD Party, so its count applies to all member states
eu_members <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
)

eu_indicator_count <- country_counts %>%
  filter(Government == "European Union") %>%
  pull(indicators_count)

eu_map_data <- world %>%
  filter(name %in% eu_members) %>%
  mutate(indicators_count = eu_indicator_count)

eu_inset <- ggplot(eu_map_data) +
  geom_sf(aes(fill = indicators_count), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-11, 32), ylim = c(34, 71), expand = FALSE) +
  scale_fill_viridis_c(
    option = "viridis",
    trans = "log1p",
    breaks = log_breaks,
    guide = "none"
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
  ) +
  labs(title = paste0("European Union (n = ", eu_indicator_count, ")"))

map_plot_with_inset <- map_plot +
  inset_element(eu_inset, left = 0, bottom = 0, right = 0.32, top = 0.5)

ggsave("indicators_map.png", plot = map_plot_with_inset, width = 12, height = 7, dpi = 300)

# Pull GDP (current US$) from the World Bank and compare to indicators reported
library(WDI)

gdp_raw <- WDI(indicator = "NY.GDP.MKTP.CD", start = 2015, end = 2024, extra = TRUE) %>%
  filter(region != "Aggregates") %>%
  rename(gdp = NY.GDP.MKTP.CD) %>%
  drop_na(gdp) %>%
  group_by(country) %>%
  slice_max(year, n = 1) %>%
  ungroup() %>%
  select(country, gdp)

gdp_data <- gdp_raw %>%
  mutate(country = recode(country, !!!country_name_map))

gdp_vs_indicators <- country_counts %>%
  inner_join(gdp_data, by = c("Government" = "country"))

write.csv(gdp_vs_indicators, "gdp_vs_indicators.csv", row.names = FALSE)

# All CBD-party countries get indicators_count = 0 (not dropped) if they never
# reported anything; only entities in non_cbd_entities stay grey on the map
cbd_country_names <- world %>%
  st_drop_geometry() %>%
  filter(cbd_party == "Yes") %>%
  distinct(name) %>%
  pull(name)

gdp_full <- tibble(Government = cbd_country_names) %>%
  left_join(country_counts, by = "Government") %>%
  mutate(indicators_count = coalesce(indicators_count, 0L)) %>%
  left_join(gdp_data, by = c("Government" = "country"))

gdp_scatter <- ggplot(gdp_vs_indicators, aes(x = gdp, y = log(indicators_count))) +
  geom_point(color = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  theme_minimal(base_size = 12) +
  geom_smooth(method="lm", se = TRUE) +
  labs(
    title = "Indicators reported vs. GDP",
    x = "GDP (current US$, log scale)",
    y = "Indicators reported"
  )

ggsave("gdp_vs_indicators.png", plot = gdp_scatter, width = 8, height = 6, dpi = 300)

# Why do many countries show up grey? Trace the data-availability funnel.
cat("\n--- Coverage funnel ---\n")
cat("CBD-party polygons on world map:", sum(world$cbd_party == "Yes"), "\n")
cat("Reported by 2026-03-07 deadline:", length(unique(dat$Government)), "\n")
cat("Have >=1 headline/non-national indicator value:", nrow(country_counts), "\n")
cat("Matched to WDI GDP data:", nrow(gdp_vs_indicators), "\n")
cat("(European Union excluded from country-level GDP join; shown separately via the inset)\n")

# Bivariate map styled after timogrossenbacher.ch/bivariate-maps-with-ggplot2-and-sf/
# 3x3 classes built from tertiles of GDP and indicators reported
default_font_color <- "#4e4d47"
default_background_color <- "#ffffff"

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "#dbdbd9", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = default_background_color, color = NA),
      panel.background = element_rect(fill = default_background_color, color = NA),
      legend.background = element_rect(fill = default_background_color, color = NA),
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0, color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5, color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = default_font_color),
      plot.caption = element_text(size = 7, hjust = .5, color = "#939184"),
      ...
    )
}

# tertile breaks for GDP (using the 0-filled CBD-party set)
quantiles_gdp <- gdp_full %>%
  drop_na(gdp) %>%
  pull(gdp) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# indicators_count is zero-inflated (many countries reported nothing), so quantile
# breaks aren't unique; classify zero as its own tier, then split reporters by median
ind_median_nonzero <- gdp_full %>%
  drop_na(gdp) %>%
  filter(indicators_count > 0) %>%
  pull(indicators_count) %>%
  median()

classify_ind_tertile <- function(x) {
  case_when(
    x <= 0 ~ 1L,
    x < ind_median_nonzero ~ 2L,
    TRUE ~ 3L
  )
}

# blue (#1E8CE3) x red (#C91024) blended 3x3 scale, per Stevens/Grossenbacher
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high indicators, high GDP
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low indicators, high GDP
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium indicators, medium GDP
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high indicators, low GDP
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0"  # low indicators, low GDP
) %>%
  pivot_longer(everything(), names_to = "group", values_to = "fill")

gdp_full <- gdp_full %>%
  mutate(
    gdp_tertile = if_else(!is.na(gdp), cut(gdp, breaks = quantiles_gdp, include.lowest = TRUE, labels = FALSE), NA_integer_),
    ind_tertile = if_else(!is.na(gdp), classify_ind_tertile(indicators_count), NA_integer_),
    group = if_else(!is.na(gdp), paste(ind_tertile, "-", gdp_tertile), NA_character_)
  ) %>%
  left_join(bivariate_color_scale, by = "group")

bi_map_data <- world %>%
  left_join(
    gdp_full %>% select(Government, fill),
    by = c("name" = "Government")
  ) %>%
  # scale_fill_identity()'s na.value is not applied by geom_sf, so bake the grey
  # in directly instead - otherwise NA fill renders fully transparent (invisible)
  mutate(fill = if_else(cbd_party == "No" | is.na(fill), "grey80", fill))


bi_map_plot <- ggplot(bi_map_data) +
  geom_sf(aes(fill = fill), color = "white", linewidth = 0.1) +
  scale_fill_identity() +
  labs(
    x = NULL, y = NULL,
    title = "GDP and biodiversity indicator reporting",
    subtitle = "Number of indicators reported vs. GDP, by country",
    caption = "Dark grey = non-CBD entity or missing GDP data; 0 indicators reported is still colored"
  ) +
  theme_map()

# legend, in the style of the blog post
bivariate_color_scale <- bivariate_color_scale %>%
  separate(group, into = c("ind", "gdp"), sep = " - ") %>%
  mutate(ind = as.integer(ind), gdp = as.integer(gdp))

bi_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(x = gdp, y = ind, fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher GDP \u2192", y = "More indicators \u2192") +
  theme_map() +
  theme(axis.title = element_text(size = 10)) +
  coord_fixed()

# EU inset colored by the EU's own bivariate class (GDP summed across member states)
eu_gdp <- gdp_data %>%
  filter(country %in% eu_members) %>%
  summarise(total = sum(gdp, na.rm = TRUE)) %>%
  pull(total)

eu_gdp_tertile <- cut(
  pmin(pmax(eu_gdp, min(quantiles_gdp)), max(quantiles_gdp)), # EU's summed GDP exceeds any single country, clamp into range
  breaks = quantiles_gdp, include.lowest = TRUE, labels = FALSE
)
eu_ind_tertile <- classify_ind_tertile(eu_indicator_count)
eu_fill <- bivariate_color_scale %>%
  filter(ind == eu_ind_tertile, gdp == eu_gdp_tertile) %>%
  pull(fill)

eu_bi_map_data <- world %>%
  filter(name %in% eu_members) %>%
  mutate(fill = eu_fill)

eu_bi_inset <- ggplot(eu_bi_map_data) +
  geom_sf(aes(fill = fill), color = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-11, 32), ylim = c(34, 71), expand = FALSE) +
  scale_fill_identity() +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
  ) +
  labs(title = "European Union")

bi_map_final <- bi_map_plot +
  inset_element(eu_bi_inset, left = 0.10, bottom = 0.02, right = 0.37, top = 0.44) +
  inset_element(bi_legend, left = -.10, bottom = 0.35, right = 0.28, top = 0.72)

ggsave("gdp_indicators_bivariate_map.png", plot = bi_map_final, width = 12, height = 7, dpi = 300)


