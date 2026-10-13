library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(stringi)

dat <- read.csv("scbd-ort-nr7-data-indicator-data-10-Sep-2026-15-39.csv") %>%
  mutate(Government = stri_trans_nfc(Government)) # fix NFD/NFC unicode mismatches (e.g. "Türkiye")

dat %>%
  group_by(Government) %>%
  count()
# 141 countries represented in the data

library(lubridate)

# Remove all indicators published after the deadline (which were not included in global report),
# filter out national indicators, and drop rows with no reported Value,
# so every downstream calculation uses headline indicators only
dat <- dat %>%
  mutate(
    published_date = dmy_hm(Published.on),
    published_date = as.Date(published_date)
  ) %>%
  filter(
    published_date <= as.Date("2026-03-07"),
    Indicator.Type != "national",
    !is.na(Value)
  )
nrow(dat)

dat %>%
  group_by(Government) %>%
  count()
# now 109 countries who reported on indicators by the deadline

length(unique(dat$Government))

# Official UN classifications used throughout this script: Least Developed
# Countries (LDC) and Small Island Developing States (SIDS). Several countries
# are officially both (e.g. Haiti, Kiribati, Comoros), so their representation
# rates are calculated independently below and are not mutually exclusive.
ldc_countries <- c(
  "Afghanistan", "Angola", "Bangladesh", "Benin", "Burkina Faso", "Burundi",
  "Cambodia", "Central African Republic", "Chad", "Comoros",
  "Democratic Republic of the Congo", "Djibouti", "Eritrea", "Ethiopia",
  "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Kiribati",
  "Lao People's Democratic Republic", "Lesotho", "Liberia", "Madagascar",
  "Malawi", "Mali", "Mauritania", "Mozambique", "Myanmar", "Nepal", "Niger",
  "Rwanda", "Senegal", "Sierra Leone", "Solomon Islands", "Somalia",
  "South Sudan", "Sudan", "Timor-Leste", "Togo", "Tuvalu", "Uganda",
  "United Republic of Tanzania", "Yemen", "Zambia"
)

sids_countries <- c(
  "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Cabo Verde",
  "Comoros", "Cook Islands", "Cuba", "Dominica", "Dominican Republic", "Fiji",
  "Grenada", "Guinea-Bissau", "Guyana", "Haiti", "Jamaica", "Kiribati",
  "Maldives", "Marshall Islands", "Micronesia (Federated States of)",
  "Mauritius", "Nauru", "Niue", "Palau", "Papua New Guinea", "Samoa",
  "São Tomé and Príncipe", "Singapore", "St. Kitts and Nevis", "St. Lucia",
  "St. Vincent and the Grenadines", "Seychelles", "Solomon Islands",
  "Suriname", "Timor-Leste", "Tonga", "Trinidad and Tobago", "Tuvalu",
  "Vanuatu"
)

# Count how many LDCs are represented in the data
ldc_count <- dat %>%
  filter(Government %in% ldc_countries) %>%
  distinct(Government) %>%
  nrow()

print(paste("Number of LDCs represented in the data:", ldc_count))
print(paste("Percentage of LDCs represented in the data:", round(ldc_count / length(ldc_countries) * 100, 2), "%"))

# Count how many SIDS are represented in the data
sids_count <- dat %>%
  filter(Government %in% sids_countries) %>%
  distinct(Government) %>%
  nrow()

print(paste("Number of SIDS represented in the data:", sids_count))
print(paste("Percentage of SIDS represented in the data:", round(sids_count / length(sids_countries) * 100, 2), "%")) # only 17.5% of SIDS represented in the data

# count distinct headline indicator codes per government (multiple years of reporting count once)
indicators_sum <- dat %>%
  group_by(Government) %>%
  summarise(indicators_count = n_distinct(Indicator.Code), .groups = "drop")

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

map_data %>%
  filter(cbd_party == "No") %>%
  select(name, indicators_count)

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

print(map_plot)

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
print(eu_inset)
map_plot_with_inset <- map_plot +
  inset_element(eu_inset, left = 0, bottom = 0, right = 0.32, top = 0.5)

print(map_plot_with_inset)

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

# Run linear model of GDP vs indicators_count, log-transforming the latter to reduce skew

# Why do many countries show up grey? Trace the data-availability funnel.
cat("\n--- Coverage funnel ---\n")
cat("CBD-party polygons on world map:", sum(world$cbd_party == "Yes"), "\n")
cat("Reported by 2026-03-07 deadline:", length(unique(dat$Government)), "\n")
cat("Have >=1 headline/non-national indicator value:", nrow(country_counts), "\n")
cat("Matched to WDI GDP data:", nrow(gdp_vs_indicators), "\n")
cat("(European Union excluded from country-level GDP join; shown separately via the inset)\n")

# Assign each CBD Party an official UN development status (Developed, Developing,
# Least Developed Country, or Small Island Developing State) and compare the
# number of indicators reported across these categories.
# Priority when a country is both an LDC and a SIDS: SIDS takes precedence.

# São Tomé and Príncipe never appears in the raw reporting data, but it is a CBD
# Party and a SIDS, so map its accented spelling to the world map's name too
country_name_map["São Tomé and Príncipe"] <- "São Tomé and Principe"

# Explicit "Developed" country list (all other CBD Parties are "Developing")
developed_countries <- c(
  "Australia", "Austria", "Canada", "Denmark", "Finland", "France", "Germany",
  "Greece", "Iceland", "Ireland", "Italy", "Japan", "Luxembourg", "Monaco",
  "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland"
)

# ldc_countries/sids_countries use the CBD data's naming (e.g. "Lao People's
# Democratic Republic"), which differs from the world map's naming (e.g.
# "Laos"), so recode them before matching against gdp_full/world's "name" field
ldc_mapped <- recode(ldc_countries, !!!country_name_map)
sids_mapped <- recode(sids_countries, !!!country_name_map)

# % of LDCs / SIDS reporting zero indicators, using the 0-filled CBD-party
# population (gdp_full). Calculated independently (not mutually exclusive),
# since several countries are officially both an LDC and a SIDS
ldc_percent <- gdp_full %>%
  filter(Government %in% ldc_mapped) %>%
  summarise(percent_0 = round((sum(indicators_count == 0) / n()) * 100, 1))

sids_percent <- gdp_full %>%
  filter(Government %in% sids_mapped) %>%
  summarise(percent_0 = round((sum(indicators_count == 0) / n()) * 100, 1))

developing_percent <- gdp_full %>%
  filter(!Government %in% developed_countries) %>%
  summarise(percent_0 = round((sum(indicators_count == 0) / n()) * 100, 1))

cat("\nPercent of LDCs reporting 0 indicators:", ldc_percent$percent_0, "%\n")
cat("Percent of SIDS reporting 0 indicators:", sids_percent$percent_0, "%\n")
cat("Percent of Developing countries reporting 0 indicators:", developing_percent$percent_0, "%\n")

development_status <- world %>%
  st_drop_geometry() %>%
  filter(cbd_party == "Yes") %>%
  distinct(name) %>%
  mutate(
    status = case_when(
      name %in% sids_mapped ~ "SIDS",
      name %in% ldc_mapped ~ "LDC",
      name %in% developed_countries ~ "Developed",
      TRUE ~ "Developing"
    )
  ) %>%
  select(name, status)

# EU member states report jointly as a single CBD Party, so drop their individual
# rows (which would otherwise look like 27 separate non-reporters) and add one
# row for the European Union itself, using its actual reported indicator count
development_full <- gdp_full %>%
  select(Government, indicators_count) %>%
  left_join(development_status, by = c("Government" = "name")) %>%
  filter(!Government %in% eu_members) %>%
  bind_rows(tibble(Government = "European Union", indicators_count = eu_indicator_count, status = "Developed")) %>%
  mutate(status = factor(status, levels = c("Developed", "Developing", "LDC", "SIDS")))

# count percentage of each development status that reported on 0 indicators
development_full %>%
  group_by(status) %>%
  summarise(
    total = n(),
    reported_0 = sum(indicators_count == 0), 
    percent_0 = round((reported_0 / total) * 100, 1)
  )

write.csv(development_full, "development_status_indicators.csv", row.names = FALSE)
# Look at number of indicators reported for develped countries
development_full %>%
  filter(status == "Developed") %>%
  print(n=47)

cat("\n--- Development status counts among CBD Parties ---\n")
print(table(development_full$status, useNA = "ifany"))

# Plot on a log1p scale (to keep the zero-inflated distribution readable) but
# label the axis with the back-transformed, actual indicator counts
dev_breaks <- c(0, 1, 2, 5, 10, 20, 30)
dev_breaks <- dev_breaks[dev_breaks <= max(development_full$indicators_count, na.rm = TRUE)]

development_boxplot <- ggplot(development_full, aes(x = status, y = indicators_count, fill = status)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = "#4e4d47") +
  scale_y_continuous(trans = "log1p", breaks = dev_breaks) +
  scale_fill_manual(values = c(
    "Developed" = "#4477AA",
    "Developing" = "#CCBB44",
    "LDC" = "#EE6677",
    "SIDS" = "#66CCEE"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "Indicators reported by development status",
    x = NULL,
    y = "Indicators reported"
  )

print(development_boxplot)
ggsave("indicators_by_development_status.png", plot = development_boxplot, width = 7, height = 6, dpi = 300)

# This doesn't really show what I want because many of the countries that are developed are eastern european countries that aren't that wealthy



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
      panel.grid.major = element_blank(), ,
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = default_background_color, color = NA),
      panel.background = element_rect(fill = default_background_color, color = NA),
      legend.background = element_rect(fill = default_background_color, color = NA),
      plot.margin = margin(0, 0, 0, 0),
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
  "1 - 1" = "#CABED0" # low indicators, low GDP
) %>%
  pivot_longer(everything(), names_to = "group", values_to = "fill")

gdp_full <- gdp_full %>%
  mutate(
    gdp_tertile = if_else(!is.na(gdp), cut(gdp, breaks = quantiles_gdp, include.lowest = TRUE, labels = FALSE), NA_integer_),
    ind_tertile = if_else(!is.na(gdp), classify_ind_tertile(indicators_count), NA_integer_),
    group = if_else(!is.na(gdp), paste(ind_tertile, "-", gdp_tertile), NA_character_)
  ) %>%
  left_join(bivariate_color_scale, by = "group")

write.csv(gdp_full, "gdp_full_bivariate.csv", row.names = FALSE)
# Run linear model of GDP vs indicators_count, log-transforming the latter to reduce skew

lm_model <- lm(
  log(indicators_count + 1) ~ log(gdp),
  data = gdp_full
)

summary(lm_model)

pred <- predict(
  lm_model,
  newdata = gdp_full,
  interval = "confidence"
)

gdp_full$predicted <- exp(pred[, "fit"]) - 1
gdp_full$lower <- exp(pred[, "lwr"]) - 1
gdp_full$upper <- exp(pred[, "upr"]) - 1

print(gdp_full$predicted)

# Make scatterplot of GDP vs indicators_count, colored by bivariate class

# Make theme for scatter plot
theme_scatter <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),

      # Axes
      axis.line = element_blank(),
      axis.text.x = element_text(size = 14, color = default_font_color),
      axis.text.y = element_text(size = 14, color = default_font_color),
      axis.title.x = element_text(size = 20, color = default_font_color),
      axis.title.y = element_text(size = 20, color = default_font_color),
      axis.ticks = element_blank(),

      # Grid
      panel.grid.major = element_line(color = "#dbdbd9", linewidth = 0.2),
      panel.grid.minor = element_blank(),

      # Background
      plot.background = element_rect(
        fill = default_background_color, color = NA
      ),
      panel.background = element_rect(
        fill = default_background_color, color = NA
      ),
      legend.background = element_rect(
        fill = default_background_color, color = NA
      ),

      # Other
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),

      # Legend
      legend.title = element_text(size = 11),
      legend.text = element_text(
        size = 9, hjust = 0, color = default_font_color
      ),

      # Titles
      plot.title = element_text(
        size = 15, hjust = 0.5, color = default_font_color
      ),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5, color = default_font_color
      ),
      plot.caption = element_text(
        size = 7, hjust = .5, color = "#939184"
      ),
      ...
    )
}

gdp_scatter <- ggplot(
  gdp_full,
  aes(x = gdp, y = indicators_count)
) +
  # Confidence interval
  geom_ribbon(
    aes(
      ymin = pmax(lower, 0),
      ymax = upper
    ),
    fill = "#777777",
    alpha = 0.07
  ) +
  # Regression line
  geom_line(
    data = gdp_full %>%
      filter(predicted >= 0) %>%
      arrange(gdp),
    aes(y = predicted),
    color = "#555555",
    linewidth = 0.65
  ) +
  # Countries colored according to bivariate class
  geom_point(
  aes(
    color = fill,
    size = indicators_count == 0
  ),
  alpha = 0.8
) +
  scale_size_manual(
    values = c(`TRUE` = 1.5, `FALSE` = 2.2),
    guide = "none"
  ) +
  scale_color_identity() +
  scale_x_log10(
    labels = scales::label_dollar(
      scale_cut = scales::cut_short_scale()
    )
  ) +

  # log1p allows countries with 0 indicators to remain visible
  scale_y_continuous(
    transform = "log1p",
    breaks = c(0, 1, 5, 10, 25, 50, 100, 150),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  labs(
    x = "GDP (current US$, log scale)",
    y = "Indicators reported (log scale)"
  ) +
  theme_scatter() +
  theme(
    # Very light grid so it doesn't compete with colors
    panel.grid.major = element_line(
      color = "#dbdbd9",
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 8, 5, 5)
  )
print(gdp_scatter)

ggsave("gdp_vs_indicators.pdf", plot = gdp_scatter, width = 6, height = 6, dpi = 300)

bi_map_data <- world %>%
  left_join(
    gdp_full %>% select(Government, fill),
    by = c("name" = "Government")
  ) %>%
  # scale_fill_identity()'s na.value is not applied by geom_sf, so bake the grey
  # in directly instead - otherwise NA fill renders fully transparent (invisible)
  mutate(fill = if_else(cbd_party == "No" | is.na(fill), "grey80", fill))


bi_map_plot <- ggplot(bi_map_data) +
  geom_sf(
    aes(fill = fill),
    color = default_background_color,
    linewidth = 0.12
  ) +
  scale_fill_identity() +
  coord_sf(
    crs = st_crs("ESRI:54030"),
    default_crs = st_crs(4326),
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_map() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )
print(bi_map_plot)
# legend, in the style of the blog post
bivariate_color_scale <- bivariate_color_scale %>%
  separate(group, into = c("ind", "gdp"), sep = " - ") %>%
  mutate(ind = as.integer(ind), gdp = as.integer(gdp))


bi_legend <- ggplot(
  bivariate_color_scale,
  aes(x = gdp, y = ind, fill = fill)
) +
  geom_tile(
    color = default_background_color,
    linewidth = 0.4
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  labs(
    x = "Higher GDP →",
    y = "More indicators →"
  ) +
  coord_fixed() +
  theme_void() +
  theme(
    axis.title.x = element_text(
      size = 11,
      color = default_font_color,
      margin = margin(t = 3)
    ),
    axis.title.y = element_text(
      size = 11,
      color = default_font_color,
      angle = 90,
      margin = margin(r = 3)
    ),
    plot.background = element_rect(
      fill = default_background_color,
      color = NA
    )
  )

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
  geom_sf(
    aes(fill = fill),
    color = default_background_color,
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = c(-11, 32),
    ylim = c(34, 71),
    expand = FALSE
  ) +
  scale_fill_identity() +
  theme_void(base_size = 9) +
  theme(
    plot.title = element_text(
      size = 8,
      face = "bold",
      hjust = 0.5,
      color = default_font_color,
      margin = margin(b = 2)
    ),
    panel.border = element_rect(
      color = "#939184",
      fill = NA,
      linewidth = 0.3
    ),
    plot.background = element_rect(
      fill = default_background_color,
      color = NA
    )
  ) +
  labs(title = "European Union")

bi_map_final <- bi_map_plot +
  inset_element(
    bi_legend,
    left = 0.12,
    bottom = 0.07,
    right = 0.285,
    top = 0.35
  ) +
  inset_element(
    eu_bi_inset,
    left = 0.355,
    bottom = 0.595,
    right = 0.465,
    top = 0.835
  ) +
  plot_annotation(
    caption = paste0(
      "Colors show countries' joint tertile of GDP and indicator reporting. ",
      "Grey indicates non-CBD entities or missing GDP data."
    ),
    theme = theme(
      plot.caption = element_text(
        size = 9,
        hjust = 0.5,
        color = "#939184",
        margin = margin(t = 5)
      ),
      plot.background = element_rect(
        fill = default_background_color,
        color = NA
      )
    )
  )


print(bi_map_final)



ggsave("gdp_indicators_bivariate_map.pdf", plot = bi_map_final, width = 12, height = 7, dpi = 300)

