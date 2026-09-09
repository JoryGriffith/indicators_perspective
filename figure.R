library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggplot2)

dat <- read.csv("indicator_data.csv")

# Filter data to only include headline indicators
headline_indicators <- dat %>% filter(Indicator.Type == "headline")
str(headline_indicators)

# load publishing dates
dates <- read.csv("publishing_dates.csv")

library(lubridate)

# Remove all NRs after the deadline (which were not included in global report)
dates <- dates %>%
  mutate(
    published_date = dmy_hm(Published.on),
    published_date = as.Date(published_date)
  ) %>%
  filter(published_date <= as.Date("2026-02-28"))

# Keep only countries represented in the filtered publication dates data
country_filter <- unique(dates$Government)
headline_indicators <- headline_indicators %>%
  filter(Government %in% country_filter)

length(unique(headline_indicators$Government))

# summarise headline indicators by government and indicator code to get max and min values
headline_indicators_sum <- headline_indicators %>%
  drop_na(Value) %>%
  group_by(Government, Indicator.Code) %>%
  summarise(max = max(Value, na.rm = TRUE), min = min(Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Government) %>%
  count(name = "headline_indicators_count") %>%
  ungroup()

print(headline_indicators_sum)
write.csv(headline_indicators_sum, "headline_indicators_summary.csv", row.names = FALSE)

# Map: number of headline indicators reported by country
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

country_counts <- headline_indicators_sum %>%
  mutate(
    Government = recode(
      Government,
      "United States" = "United States of America",
      "Czech Republic" = "Czechia",
      "Republic of Korea" = "South Korea"
    )
  )

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(cbd_party = if_else(name %in% non_cbd_entities, "No", "Yes"))

country_names <- unique(world$name)
write.csv(country_names, "country_names.csv", row.names = FALSE)

map_data <- world %>%
  left_join(country_counts %>% select(Government, headline_indicators_count), by = c("name" = "Government")) %>%
  mutate(
    headline_indicators_count = if_else(
      cbd_party == "No",
      NA_integer_,
      coalesce(headline_indicators_count, 0L)
    )
  )

map_plot <- ggplot(map_data) +
  geom_sf(aes(fill = headline_indicators_count), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    name = "Headline indicators",
    limits = c(0, max(map_data$headline_indicators_count, na.rm = TRUE)),
    breaks = pretty(c(0, max(map_data$headline_indicators_count, na.rm = TRUE)))
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
    title = "Headline indicators reported by country",
    caption = "Non-CBD entities shown in grey"
  )

print(map_plot)

ggsave("headline_indicators_map.png", plot = map_plot, width = 12, height = 7, dpi = 300)


