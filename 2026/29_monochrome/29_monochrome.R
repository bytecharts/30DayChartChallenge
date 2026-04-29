# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(ggtext)
  library(sf)
  library(usmap)
  library(cowplot)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Data ----
obs <- tibble::tribble(
  ~year , ~value ,
   1998 ,    100 ,
   2000 ,     95 ,
   2002 ,     90 ,
   2004 ,     84 ,
   2006 ,     77 ,
   2008 ,     71 ,
   2010 ,     65 ,
   2012 ,     59 ,
   2014 ,     54 ,
   2016 ,     50 ,
   2018 ,     46 ,
   2020 ,     43 # verified: -57% from 1998 baseline
)
proj_opt <- tibble::tribble(
  ~year , ~value ,
   2020 ,     43 ,
   2025 ,     41 ,
   2030 ,     38 ,
   2035 ,     35 ,
   2040 ,     32 ,
   2045 ,     29 ,
   2050 ,     27 ,
   2055 ,     25
  # endpoint ~25 consistent with -44% range disappearance (optimistic scenario)
  # intermediate values interpolated
)

proj_mod <- tibble::tribble(
  ~year , ~value ,
   2020 ,     43 ,
   2025 ,     39 ,
   2030 ,     33 ,
   2035 ,     27 ,
   2040 ,     21 ,
   2045 ,     17 ,
   2050 ,     14 ,
   2055 ,     12
  # endpoint ~12 consistent with -51 to -70% additional from 2020
  # intermediate values interpolated
)

proj_sev <- tibble::tribble(
  ~year , ~value ,
   2020 ,     43 ,
   2025 ,     34 ,
   2030 ,     23 ,
   2035 ,     14 ,
   2040 ,      8 ,
   2045 ,      4 ,
   2050 ,      2 ,
   2055 ,      1
  # endpoint ~1-2 consistent with -93 to -97% additional from 2020
  # intermediate values interpolated
)

# Ecoregion uncertainty band — bounds from paper's reported ecoregion range
# hi = Greater Yellowstone ecoregion trajectory (-15% by 2020 → value = 85)
# lo = Madrean Archipelago trajectory (-83% by 2020 → value = 17)
# Intermediate values interpolated; 1998 start = 100 for all ecoregions
obs_band_hi <- tibble::tribble(
  ~year , ~value ,
   1998 ,    100 ,
   2000 ,     99 ,
   2004 ,     97 ,
   2008 ,     95 ,
   2012 ,     92 ,
   2016 ,     89 ,
   2018 ,     87 ,
   2020 ,     85 # Greater Yellowstone: -15%
)

obs_band_lo <- tibble::tribble(
  ~year , ~value ,
   1998 ,    100 ,
   2000 ,     91 ,
   2004 ,     71 ,
   2008 ,     52 ,
   2012 ,     37 ,
   2016 ,     24 ,
   2018 ,     20 ,
   2020 ,     17 # Madrean Archipelago: -83%
)

scenarios <- bind_rows(
  mutate(proj_opt, scenario = "Optimistic"),
  mutate(proj_mod, scenario = "Moderate"),
  mutate(proj_sev, scenario = "Severe")
)

scenario_colors <- c(
  Optimistic = alpha(theme_fg, 0.50),
  Moderate = alpha(theme_fg, 0.75),
  Severe = alpha(theme_fg, 0.95)
)

decade_labels <- scenarios %>%
  filter(year %in% c(2055)) %>%
  mutate(
    label = paste0("-", 100 - value, "%") # decline from 1998 baseline
  )

# Scenario end labels at 2055
scenario_end_labels <- tibble::tribble(
  ~year , ~value , ~scenario    , ~label                 ,
   2055 ,     25 , "Optimistic" , "Optimistic\n     25%" ,
   2055 ,     12 , "Moderate"   , "Moderate\n     12%"   ,
   2055 ,      1 , "Severe"     , "Severe\n     1%"
)
us_states <- usmap::us_map(regions = "states")


# ---- Map — corrected ecoregion-to-state assignments ----
# Based on EPA Level III ecoregion boundaries (Omernik 1987, revised)
# which align with the ecoregion framework used in Janousek et al. 2023.
# States listed are primary intersecting states only.
region_groups <- list(
  # --- Confirmed correct ---
  "Greater Yellowstone Ecosystem" = c("ID", "MT", "WY"),
  "Northern Rocky Mountains" = c("ID", "MT", "WY"),
  "Madrean Archipelago" = c("AZ", "NM"),

  # --- Fixed / expanded ---
  "Coastal Forest" = c("CA", "OR", "WA"), # NorCal coastal included
  "Cascade Range" = c("CA", "OR", "WA"), # N. CA Cascades (Shasta)
  "Southern Rocky Mountains" = c("CO", "NM", "UT"), # UT margin included
  "Colorado Plateau and Wyoming Basin" = c("AZ", "CO", "NM", "UT", "WY"), # NM restored
  "Blue Mountains" = c("ID", "OR", "WA"),
  "Columbia Plateau" = c("ID", "OR", "WA"),
  "Great Basin" = c("ID", "NV", "OR", "UT"),
  "Sierra Nevada" = c("CA"),
  "Mediterranean California" = c("CA"),
  "Semi-arid Prairies" = c("CO", "MT", "ND", "NM", "SD", "WY"),
  "Isolated Mountains in Prairie" = c("MT", "ND", "SD", "WY"),
  "Uinta and Wasatch Mountains" = c("CO", "UT", "WY"),
  "Mogollon Rim and Mountains" = c("AZ", "NM")
)


region_palette <- c(
  "Greater Yellowstone Ecosystem" = alpha("#d4d4d4", 0.75),
  "Northern Rocky Mountains" = alpha("#c8c8c8", 0.70),
  "Madrean Archipelago" = alpha("#bcbcbc", 0.65),
  "Coastal Forest" = alpha("#b0b0b0", 0.62),
  "Cascade Range" = alpha("#a4a4a4", 0.60),
  "Southern Rocky Mountains" = alpha("#989898", 0.58),
  "Colorado Plateau and Wyoming Basin" = alpha("#8c8c8c", 0.55),
  "Blue Mountains" = alpha("#808080", 0.52),
  "Columbia Plateau" = alpha("#747474", 0.50),
  "Great Basin" = alpha("#686868", 0.48),
  "Sierra Nevada" = alpha("#5c5c5c", 0.45),
  "Mediterranean California" = alpha("#505050", 0.43),
  "Semi-arid Prairies" = alpha("#444444", 0.42),
  "Isolated Mountains in Prairie" = alpha("#383838", 0.40),
  "Uinta and Wasatch Mountains" = alpha("#2c2c2c", 0.38),
  "Mogollon Rim and Mountains" = alpha("#202020", 0.36)
)

region_map <- bind_rows(lapply(names(region_groups), function(region_name) {
  us_states %>%
    filter(abbr %in% region_groups[[region_name]]) %>%
    mutate(region = region_name)
}))

mini_map <- ggplot() +
  geom_sf(
    data = us_states,
    fill = alpha(theme_bg, 0.85),
    color = alpha(theme_fg, 0.22),
    linewidth = 0.32
  ) +
  geom_sf(
    data = region_map,
    aes(fill = region),
    color = alpha(theme_fg, 0.35),
    linewidth = 0.32
  ) +
  scale_fill_manual(values = region_palette, guide = "none") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = theme_bg,
      color = alpha(theme_fg, 0.28),
      linewidth = 0.55
    ),
    panel.background = element_rect(fill = theme_bg, color = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

# ---- Plot ----
main_plot <- ggplot() +
  geom_ribbon(
    data = tibble::tibble(
      year = obs_band_hi$year,
      ymin = obs_band_lo$value,
      ymax = obs_band_hi$value
    ),
    aes(x = year, ymin = ymin, ymax = ymax),
    fill = alpha(night_owlish_light$gray, 0.18)
  ) +
  geom_line(
    data = obs,
    aes(year, value),
    color = alpha(theme_fg, 0.85),
    linewidth = 1.1
  ) +
  geom_line(
    data = filter(scenarios, scenario == "Optimistic"),
    aes(year, value),
    color = scenario_colors[["Optimistic"]],
    linewidth = 0.95,
    linetype = "dotted"
  ) +
  geom_line(
    data = filter(scenarios, scenario == "Moderate"),
    aes(year, value),
    color = scenario_colors[["Moderate"]],
    linewidth = 0.95,
    linetype = "dashed"
  ) +
  geom_line(
    data = filter(scenarios, scenario == "Severe"),
    aes(year, value),
    color = scenario_colors[["Severe"]],
    linewidth = 0.95,
    linetype = "longdash"
  ) +
  geom_vline(
    xintercept = 2020,
    linetype = 3,
    linewidth = 0.4,
    color = theme_muted
  ) +
  annotate(
    "text",
    x = 2020.6,
    y = 22,
    label = "Projected ->",
    hjust = 0,
    family = "FiraSans",
    fontface = "bold",
    size = 5.5,
    color = alpha(theme_fg, 0.8),
  ) +
  # after geom_vline, before annotate("text", ...) for "projection starts"
  annotate(
    "point",
    x = 2020,
    y = 43,
    size = 3.2,
    color = theme_fg
  ) +
  annotate(
    "text",
    x = 2022.8,
    y = 45,
    label = "43% (2020)",
    hjust = 0.5,
    family = "FiraSans",
    fontface = "bold",
    size = 5.5,
    color = alpha(theme_fg, 0.85)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2055, 5),
    limits = c(1998, 2055.5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  geom_text(
    data = scenario_end_labels,
    aes(year, value, label = label, color = scenario),
    hjust = 0,
    nudge_x = -3.4,
    nudge_y = 5,
    size = 5.5,
    family = "FiraSans",
    fontface = "bold",
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Optimistic" = alpha(theme_fg, 0.50),
      "Moderate" = alpha(theme_fg, 0.75),
      "Severe" = alpha(theme_fg, 0.95)
    )
  ) +
  labs(
    title = "Western Bumble Bee Decline, 1998–2020",

    subtitle = paste0(
      "<b style='color:#2A2F3A'>Bombus occidentalis</b> occupancy across ",
      "<b style='color:#2A2F3A'>16 western US ecoregions</b>, 1998\u20132020. ",
      "Observed mean decline of <b style='color:#2A2F3A'>\u221257%</b>; ",
      "shaded band spans <b style='color:#2A2F3A'>\u221215% ",
      "(Greater Yellowstone)</b> to <b style='color:#2A2F3A'>\u221283% ",
      "(Madrean Archipelago)</b>. ",
      "Three projected scenarios to <b style='color:#2A2F3A'>2055</b> ",
      "from Janousek et al. (2023) *PNAS*."
    ),
    x = NULL,
    y = "occupancy index (1998=100%)",
    caption = caption_global(
      "Janousek W.M. et al. (2023) PNAS 120(7). DOI: 10.1073/pnas.2211223120. Observed occupancy 1998\u20132020: USGS data release doi.org/10.5066/P9UHMCV1.",
      "Day 29",
      "Monochrome"
    )
  ) +
  theme_base() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = alpha(theme_fg, 0.08),
      linewidth = 0.3
    ),
    legend.position = "none",
    plot.title = element_markdown(
      family = theme_title_family,
      face = "bold",
      size = 32,
      color = theme_fg,
      hjust = 0,
      margin = margin(b = 8)
    ),

    plot.subtitle = element_textbox(
      family = "FiraSans",
      size = 18,
      color = night_owlish_light$fg_soft,
      hjust = 0,
      lineheight = 1.2,
      width = unit(0.85, "npc"), # controls wrapping width
      margin = margin(b = 14),
      padding = margin(8, 0, 0, 0)
    ),
    axis.title.y = element_text(
      size = 14,
      angle = 90,
      color = alpha(theme_fg, 0.8),
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(size = 10, color = alpha(theme_fg, 0.8)),
    axis.text.y = element_text(size = 10, color = alpha(theme_fg, 0.8)),
    plot.caption = element_markdown(
      family = theme_caption_family,
      size = 10,
      color = night_owlish_light$fg_soft,
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.margin = margin(18, 18, 18, 18),
    axis.text = element_text(size = 9, color = theme_muted)
  )

plot <- ggdraw() +
  draw_plot(main_plot, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(mini_map, x = 0.59, y = 0.54, width = 0.38, height = 0.33)

# ---- Save Plot ----
ggsave(
  "29_monochrome.png",
  plot,
  width = 14,
  height = 12,
  dpi = 320,
  type = "cairo",
  bg = "#fff"
)
