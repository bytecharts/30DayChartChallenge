# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(scales)
  library(patchwork)
})

# ---- Source Theme ----
source("../utils/theme.R")

# ---- Data ----
df <- data.frame(
  Year = rep(2018:2024, each = 6),
  Party = rep(c("BJP", "INC", "TMC", "BRS", "DMK", "Others"), times = 7),
  Amount = c(
    550,120,100,90,50,146,
    3000,600,600,500,200,900,
    1800,350,300,250,150,550,
    950,200,180,150,120,200,
    1500,300,280,250,150,320,
    1700,350,320,300,180,350,
    550,120,100,90,60,80
  )
)

# Change to billions

df <- df %>%
  mutate(Amount = Amount * 0.01)

year_totals <- df %>%
  group_by(Year) %>%
  summarise(total = sum(Amount), .groups = "drop")

max_funding_total <- max(year_totals$total)

party_order <- df %>%
  group_by(Party) %>%
  summarise(total = sum(Amount), .groups = "drop") %>%
  arrange(total) %>%
  pull(Party)

df <- df %>%
  mutate(Party = factor(Party, levels = party_order))

top_parties <- df %>%
  group_by(Party) %>%
  summarise(total = sum(Amount), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 3) %>%
  pull(Party)

bar_width <- 0.65

general_results_df <- data.frame(
  Year = c(2019, 2024),
  Election = "General",
  Won = c(303, 240),
  Total = c(543, 543),
  Unit = "seats",
  WonStates = NA_character_
)

state_results_df <- data.frame(
  Year = c(2018, 2020, 2021, 2022, 2023),
  Election = "State",
  Won = c(0, 1, 1, 5, 3),
  Total = c(6, 1, 4, 6, 5),
  Unit = "states",
  WonStates = c(
    NA_character_,
    "Bihar",
    "Assam",
    "UP, Uttarakhand,\nGoa, Manipur,\nGujarat",
    "MP, Rajasthan,\nChhattisgarh"
  )
)

elections_df <- bind_rows(general_results_df, state_results_df) %>%
  mutate(
    Rest = Total - Won,
    Pct = Won / Total * 100,
    RestPct = 100 - Pct,
    Label = case_when(
      Election == "State" ~ WonStates,
      TRUE ~ paste(Won, Unit)
    ),
    LabelY = Pct / 2,
    RestLabel = paste(
      Rest,
      if_else(Rest == 1, sub("s$", "", Unit), Unit)
    ),
    RestLabelY = Pct + (RestPct / 2)
  )

results_stack_df <- bind_rows(
  elections_df %>%
    transmute(Year, Election, Segment = "Won", Value = Pct),
  elections_df %>%
    transmute(Year, Election, Segment = "Rest", Value = 100 - Pct)
) %>%
  mutate(Segment = factor(Segment, levels = c("Won", "Rest")))

# ---- Plot ----
funding_plot <- ggplot(df, aes(x = Year, y = Amount, fill = Party)) +

  # Stacked bars
  geom_col(
    width = bar_width,
    color = night_owlish_light$bg,
    linewidth = 0.4
  ) +

  geom_text(
    data = df %>% filter(Party %in% top_parties),
    aes(label = scales::number(Amount, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 3,
    fontface = "bold",
    color = night_owlish_light$fg
  ) +

  # Scales
  scale_x_continuous(
    breaks = 2018:2024,
    limits = c(2017.5, 2024.5),
    expand = expansion(mult = c(0, 0))
  ) +
    scale_y_continuous(
      limits = c(0, max_funding_total * 1.05),
      breaks = seq(0, ceiling(max_funding_total / 5) * 5, by = 5),
      expand = expansion(mult = c(0, 0.02)),
      labels = scales::label_number(suffix = "B", accuracy = 0.1)
    )+
  scale_fill_manual(values = c(
    "BJP" = night_owlish_cat[3],
    "INC" = "#CDD3DC",
    "TMC" = "#BEC5CF",
    "BRS" = "#B0B8C3",
    "DMK" = "#A1AAB7",
    "Others" = "#939DAA"
  )) +

  # Labels
  labs(
title= "India’s Ruling Party <span style='color:#FFB454'>(BJP)</span> Led Electoral Bond Funding",
 subtitle = "Electoral bonds are anonymous bank-issued donations to political parties;<br><span style='color:#FFB454'>BJP</span> received the largest share of funding and secured key electoral wins<br> until the Scheme Was Ended in 2024",
    caption = caption_global(
      "Source: SBI & Election Commission disclosures (reconstructed)",
      "Day 17",
      "Remake"
    ),
    x = NULL,
    y = "Funding (₹ billions)",
    fill = NULL
  ) +

  coord_flip() +

  # Theme
  theme_base() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 10), angle =90),
    plot.subtitle = element_markdown(color = night_owlish_light$fg_soft)
  )

results_plot <- ggplot(elections_df, aes(x = Year, y = Pct)) +
  geom_col(
    data = results_stack_df,
    aes(x = Year, y = Value, fill = Segment, linetype = Election),
    width = bar_width,
    position = position_stack(reverse = TRUE),
    color = night_owlish_light$fg,
    linewidth = 0.6
  ) +
  geom_text(
    data = elections_df %>% filter(Election == "General"),
    aes(x = Year, y = LabelY, label = Label),
    size = 3,
    fontface = "bold",
    color = night_owlish_light$fg
  ) +
  geom_text(
    data = elections_df %>% filter(Election == "State", Won > 0),
    aes(x = Year, y = LabelY, label = Label),
    size = 2.3,
    lineheight = 0.95,
    fontface = "bold",
    color = night_owlish_light$fg
  ) +
  geom_text(
    data = elections_df %>% filter(Rest > 0),
    aes(x = Year, y = RestLabelY, label = RestLabel),
    size = 2.5,
    fontface = "bold",
    color = night_owlish_light$fg_soft
  ) +
  scale_fill_manual(values = c(
    "Won" = night_owlish_cat[3],
    "Rest" = "#B3BBC7"
  )) +
  scale_linetype_manual(
    values = c("General" = "solid", "State" = "dotted"),
    labels = c(
      "General" = "Lok Sabha (national)",
      "State" = "State assembly"
    ),
    name = "Election type"
  ) +
  scale_x_continuous(
    breaks = 2018:2024,
    limits = c(2017.5, 2024.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Election results",
    x = NULL,
    y = "Won out of total (%)"
  ) +
  coord_flip() +
  theme_base() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_markdown(size = 18, hjust = 0.5),
    plot.subtitle = element_markdown(color = night_owlish_light$fg_soft),
    axis.title.y = element_text(margin = margin(r = 10), angle = 90)
  ) +
  guides(
    fill = "none",
    linetype = guide_legend(override.aes = list(
      fill = NA,
      color = night_owlish_light$fg
    ))
  )

plot <- funding_plot + results_plot + plot_layout(widths = c(1.6, 1))

# ---- Save ----
ggsave("16_causation_electoral_bonds.png",
  plot,
  width = 16,
  height = 9,
  dpi = 300,
  type = "cairo"
)
