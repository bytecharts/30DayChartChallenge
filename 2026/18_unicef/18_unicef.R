# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readxl)
  library(countrycode)
  library(ggnewscale)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Load Data ----
data_path <- "../data/XLS_Justification-of-wife-beating-among-adolescents-database_Mar-2025-1.xlsx"

justification_raw <- read_excel(
  data_path,
  sheet = 1,
  skip = 7,
  col_names = c(
    "country",
    "male",
    "male_note",
    "male_source",
    "female",
    "female_note",
    "female_source"
  )
)

country_scores <- justification_raw %>%
  filter(!is.na(country), country != "Countries and areas") %>%
  mutate(
    male = suppressWarnings(as.numeric(male)),
    female = suppressWarnings(as.numeric(female)),
    iso3c = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Bolivia (Plurinational State of)" = "BOL",
        "Brunei Darussalam" = "BRN",
        "Cabo Verde" = "CPV",
        "Côte d'Ivoire" = "CIV",
        "Democratic People's Republic of Korea" = "PRK",
        "Iran (Islamic Republic of)" = "IRN",
        "Kosovo under UNSC res. 1244" = "XKX",
        "Lao People's Democratic Republic" = "LAO",
        "Micronesia (Federated States of)" = "FSM",
        "Netherlands (Kingdom of the)" = "NLD",
        "Republic of Korea" = "KOR",
        "Republic of Moldova" = "MDA",
        "Russian Federation" = "RUS",
        "State of Palestine" = "PSE",
        "Syrian Arab Republic" = "SYR",
        "Türkiye" = "TUR",
        "United Republic of Tanzania" = "TZA",
        "United States" = "USA",
        "Venezuela (Bolivarian Republic of)" = "VEN",
        "Viet Nam" = "VNM"
      ),
      warn = FALSE
    )
  ) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(
    male = first(male),
    female = first(female),
    .groups = "drop"
  )

world_map <- map_data("world") %>%
  filter(region != "Antarctica") %>%
  mutate(
    iso3c = countrycode(
      region,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Bolivia" = "BOL",
        "Brunei" = "BRN",
        "Cape Verde" = "CPV",
        "Czech Republic" = "CZE",
        "Democratic Republic of the Congo" = "COD",
        "Ivory Coast" = "CIV",
        "Kosovo" = "XKX",
        "Laos" = "LAO",
        "Micronesia" = "FSM",
        "Moldova" = "MDA",
        "North Korea" = "PRK",
        "Palestine" = "PSE",
        "Russia" = "RUS",
        "South Korea" = "KOR",
        "Swaziland" = "SWZ",
        "Syria" = "SYR",
        "Tanzania" = "TZA",
        "UK" = "GBR",
        "USA" = "USA",
        "Vatican" = "VAT",
        "Venezuela" = "VEN",
        "Vietnam" = "VNM"
      ),
      warn = FALSE
    )
  )

female_n <- country_scores %>% filter(!is.na(female)) %>% nrow()
male_n <- country_scores %>% filter(!is.na(male)) %>% nrow()
map_country_total <- world_map %>% filter(!is.na(iso3c)) %>% distinct(iso3c) %>% nrow()

female_label <- paste0("Girls (", female_n, "/", map_country_total, " countries)")
male_label <- paste0("Boys (", male_n, "/", map_country_total, " countries)")

female_map <- world_map %>%
  left_join(
    country_scores %>% transmute(iso3c, score = female),
    by = "iso3c"
  ) %>%
  mutate(indicator = factor(female_label, levels = c(female_label, male_label)))

male_map <- world_map %>%
  left_join(
    country_scores %>% transmute(iso3c, score = male),
    by = "iso3c"
  ) %>%
  mutate(indicator = factor(male_label, levels = c(female_label, male_label)))

map_long <- bind_rows(female_map, male_map)

map_with_data <- map_long %>% filter(!is.na(score))
map_without_data <- map_long %>% filter(is.na(score))

# ---- Plot Data ----
plot <- ggplot() +
  geom_polygon(
    data = map_without_data,
    aes(x = long, y = lat, group = group, fill = "No data"),
    color = scales::alpha(night_owlish_light$fg, 0.2),
    linewidth = 0.1
  ) +
  scale_fill_manual(
    name = "",
    values = c("No data" = scales::alpha(night_owlish_light$gray, 0.32)),
    guide = guide_legend(
      order = 2,
      title.position = "top",
      title.hjust = 0,
      keyheight = grid::unit(5, "mm"),
      keywidth = grid::unit(8, "mm")
    )
  ) +
  ggnewscale::new_scale_fill() +
  geom_polygon(
    data = map_with_data,
    aes(x = long, y = lat, group = group, fill = score),
    color = scales::alpha(night_owlish_light$fg, 0.22),
    linewidth = 0.1
  ) +
  scale_fill_gradientn(
    name = "Justification score (%)",
    colors = c(
      "#1F5E8B",
      night_owlish_cat[1],
      "#6FB9D7",
      night_owlish_cat[3],
      night_owlish_cat[2]
    ),
    values = scales::rescale(c(0, 20, 40, 70, 100)),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::label_number(suffix = "%"),
    oob = scales::squish,
    guide = guide_colorbar(
      order = 1,
      title.position = "top",
      title.hjust = 0,
      label.position = "right",
      label.theme = element_text(
        size = 8,
        color = night_owlish_light$fg_soft,
        margin = margin(l = 8)
      ),
      barheight = grid::unit(46, "mm"),
      barwidth = grid::unit(5, "mm"),
      frame.colour = scales::alpha(night_owlish_light$fg, 0.2),
      ticks.colour = night_owlish_light$fg_soft
    )
  ) +
  coord_quickmap() +
  facet_wrap(~indicator, ncol = 1) +
  labs(
    title = "Justification of Wife-Beating Among Adolescents",
    subtitle = "Percentage of girls and boys 15–19 years old who consider a husband to be justified in hitting or beating his wife<br> for at least one of the specified reasons, i.e., if his wife burns the food, argues with him, goes out without telling him,<br> neglects the children or refuses sexual relations.",
    caption = caption_global("UNICEF global databases, 2025, based on DHS, MICS and other national surveys.", "Day 18", "UNICEF")

  ) +
  theme_base() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = grid::unit(10, "pt"),
    strip.text = element_text(size = 13, family = "SpaceGrotesk", face = "bold", color = night_owlish_light$fg),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.box.spacing = grid::unit(12, "pt"),
    legend.spacing.y = grid::unit(8, "pt"),
    legend.margin = margin(4, 4, 4, 4),
    legend.title = element_text(size = 9, color = night_owlish_light$fg, margin = margin(b = 6)),
    legend.text = element_text(size = 8, color = night_owlish_light$fg_soft, margin = margin(l = 2, t = 1, b = 1)),
    plot.subtitle = element_markdown(
      size = 11,
      hjust = 0,
      color = night_owlish_light$fg_soft,
      margin = margin(b = 14),
      lineheight = 1.3
    )
  )

# ---- Save Plot ----
output_path <- "18_unicef.png"
ggsave(output_path, plot, width = 14, height = 11, type = "cairo", dpi = 300)
