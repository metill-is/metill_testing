library(tidyverse)
library(metill)
library(ggtext)
library(scales)
library(patchwork)
library(geomtextpath)
library(ggiraph)
library(glue)
library(ggh4x)

theme_set(theme_metill())

litur_island <- "#08306b"

litur_danmork <- "#e41a1c"

litur_finnland <- "#3690c0"

litur_noregur <- "#7f0000"

litur_svithjod <- "#fd8d3c"

litur_luxemborg <- "black"

litur_total <- "#005824"

litur_annad <- "#737373"

d <- read_csv("dashboards/properties/data/data_combined.csv") |>
  rename(land = country) |>
  mutate(
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      land == "Meðaltal" ~ litur_total,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth),
    land_ordered = glue("<i style='color:{colour}'>{land}</i>")
  )

plot_dat <- d |>
  pivot_longer(c(dwellings, population_adult)) |>
  mutate(
    name = fct_recode(
      name,
      "Fasteignir" = "dwellings",
      "Fullorðnir" = "population_adult"
    )
  ) |>
  mutate(
    idx = value / value[year == min(year)],
    .by = c(land, name)
  ) |>
  filter(
    any(year == 2007),
    .by = land
  )

p1 <- plot_dat |>
  ggplot(aes(year, idx)) +
  geom_line_interactive(
    data = plot_dat |>
      filter(colour == litur_annad),
    aes(group = land, colour = litur_annad, data_id = land),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line_interactive(
    data = plot_dat |>
      filter(colour != litur_annad),
    aes(group = land, colour = colour, data_id = land),
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(2007, 2023, by = 4),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_extended(10),
    labels = function(x) hlutf(x - 1),
    guide = guide_axis_truncated()
  ) +
  scale_color_identity() +
  scale_linewidth_manual(
    values = c(0.2, 1.4)
  ) +
  scale_alpha_manual(
    values = c(0.3, 1)
  ) +
  facet_wrap("name") +
  theme(
    legend.position = "none",
    panel.spacing.x = unit(10, "pt")
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fjölgun fasteigna og fullorðinna frá 2007"
  )

p2 <- d |>
  ggplot(aes(year, population_adult / dwellings)) +
  geom_line_interactive(
    data = ~ filter(.x, colour == litur_annad),
    aes(group = land, colour = litur_annad, data_id = land),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line_interactive(
    data = ~ filter(.x, colour != litur_annad),
    aes(group = land, colour = colour, data_id = land),
    linewidth = 1
  ) +
  scale_color_identity() +
  scale_linewidth_manual(
    values = c(0.2, 1.4)
  ) +
  scale_alpha_manual(
    values = c(0.3, 1)
  ) +
  geom_textsmooth(
    data = ~ filter(.x, land == "Pólland"),
    linewidth = 1.5,
    text_only = TRUE,
    vjust = -0.4,
    span = 0.5,
    alpha = 0.3,
    size = 3,
    aes(
      label = "Í mörgum löndum fækkaði fullorðnum á hverja fasteign",
      col = colour
    )
  ) +
  geom_textsmooth(
    data = ~ filter(.x, land == "Ísland"),
    linewidth = 1.5,
    text_only = TRUE,
    vjust = -0.4,
    span = 0.4,
    aes(
      label = "Á Íslandi fjölgaði fullorðnum á hverja fasteign úr 1.96 í 2.02",
      col = colour
    )
  ) +
  scale_x_continuous(
    breaks = seq(2007, 2023, by = 2),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_extended(10),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fullorðnir á hverja fasteign"
  )




p3 <- d |>
  filter(
    year == max(year),
    .by = land
  ) |>
  mutate(
    adults_per_dwelling = population_adult / dwellings,
    land_ordered = fct_reorder(land_ordered, adults_per_dwelling)
  ) |>
  ggplot(aes(adults_per_dwelling, land_ordered)) +
  geom_segment_interactive(
    aes(xend = 1, yend = land_ordered, data_id = land, col = colour)
  ) +
  geom_point_interactive(
    aes(data_id = land, col = colour)
  ) +
  geom_text_interactive(
    aes(
      x = 0.995,
      label = glue("{land} ({year}) "),
      data_id = land,
      col = colour
    ),
    hjust = 1,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(1, 2, by = 0.1),
    expand = expansion(c(0, 0.1)),
    guide = guide_axis_truncated()
  ) +
  scale_color_identity() +
  coord_cartesian(
    xlim = c(0.92, NA),
    clip = "off"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fullorðnir á hverja fasteign í síðustu mælingu"
  )

design <- "
AAAA
BBCC
"

p <- wrap_plots(list(p3, p1, p2)) +
  plot_layout(
    design = design,
    heights = c(1, 1.2)
  ) +
  plot_annotation(
    title = str_c(
      "Þrátt fyrir mikla fasteignauppbyggingu á Íslandi ",
      "hefur ekki tekist að halda í við fólksfjölgun"
    ),
    subtitle = str_c(
      "Á Íslandi hefur fasteignum fjölgað hlutfallslega mest, ",
      "en það sama má segja um fullorðna íbúa"
    ),
    caption = str_c(
      "Fullorðinn skilgreindur sem eldri en 18 ára | ",
      "Íslensk gögn frá fasteignaskrá og Hagstofu Íslands | ",
      "Önnur gögn frá Gleeson gagnasafninu og Eurostat"
    )
  )

p

ggsave(
  "dashboards/properties/img/fp_with_background.png",
  p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.55
)

ggsave(
  "dashboards/properties/img/fp.png",
  p &
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      strip.background = element_rect(fill = "transparent")
    ),
  width = 8,
  height = 0.621 * 8,
  scale = 1.55
)
