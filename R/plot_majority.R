library(tidyverse)
library(ggiraph)
library(metill)
library(patchwork)
library(here)
library(arrow)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill())

colors <- tribble(
  ~flokkur, ~litur,
  "Sjálfstæðisflokkurinn", "#377eb8",
  "Framsóknarflokkurinn", "#41ab5d",
  "Samfylkingin", "#e41a1c",
  "Vinstri Græn", "#006d2c",
  "Viðreisn", "#f16913",
  "Píratar", "#6a51a3",
  "Miðflokkurinn", "#08306b",
  "Flokkur Fólksins", "#FBB829",
  "Sósíalistaflokkurinn", "#a50f15",
  "Annað", "grey50"
)


samsteypa_flokkar <- c(
  #"xS" = "Samfylkingin",
  "xD" = "Sjálfstæðisflokkurinn",
  "xM" = "Miðflokkurinn",
  "xC" = "Viðreisn"
  #"xF" = "Flokkur Fólksins"
)

d <- read_parquet(here("data", "y_rep_draws.parquet")) |>
  mutate(
    flokkur = if_else(
      flokkur %in% samsteypa_flokkar,
      "Samsteypa",
      "Annað"
    )
  ) |>
  summarise(
    value = sum(value),
    .by = c(.iteration, .chain, .draw, dags, flokkur)
  ) |>
  mutate(
    value = value / sum(value),
    .by = c(.draw, dags)
  ) |> 
  summarise(
    mean = mean(value),
    q5 = quantile(value, 0.05),
    q95 = quantile(value, 0.95),
    .by = c(dags, flokkur)
  ) |>
  mutate(
    litur = if_else(flokkur == "Samsteypa", "#e41a1c", "grey50")
  )


p1 <- d |>
  filter(dags == max(dags)) |>
  distinct(dags, flokkur, mean, q5, q95, litur) |>
  mutate(
    flokkur_ordered = fct_reorder(flokkur, mean)
  ) |>
  ggplot(aes(mean, flokkur_ordered, color = litur, data_id = flokkur)) +
  geom_text_interactive(
    aes(label = flokkur, x = 0),
    hjust = 1,
    nudge_x = -0.005,
    size = 5
  ) +
  geom_segment_interactive(
    aes(xend = 0, yend = flokkur_ordered),
    alpha = 0.3,
    linewidth = 0.6
  ) +
  geom_point_interactive(
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 0.6, by = 0.1),
    guide = ggh4x::guide_axis_truncated(
      trunc_upper = 0.6
    ),
    labels = label_percent()
  ) +
  scale_colour_identity() +
  coord_cartesian(clip = "off", xlim = c(-0.1, 0.6)) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Staðan í nýjustu könnunum"
  )

p2 <- d |>
  ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
  geom_vline(
    xintercept = clock::date_build(2024, 11, 30),
    alpha = 0.4
  ) +
  annotate(
    geom = "label",
    label = "Kosningar 30. nóvember",
    x = clock::date_build(2024, 11, 30),
    y = 0.5,
    hjust = 0.5,
    vjust = 1,
    angle = 90,
    fill = "#faf9f9"
  ) +
  geom_smooth_interactive(
    method = "loess",
    span = 0.12,
    se = 0,
    n = 500
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(
      trunc_upper = clock::date_build(2024, 11, 30)
    ),
    limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
    labels = label_date_short(),
    breaks = clock::date_build(
      2024,
      c(
        8, 8, 8, 8,
        9, 9, 9, 9,
        10, 10, 10, 10,
        11, 11, 11, 11, 11
      ),
      c(
        1, 8, 15, 22,
        1, 8, 15, 22,
        1, 8, 15, 22,
        1, 8, 15, 22, 30
      )
    )
  ) +
  scale_y_continuous(
    breaks = breaks_extended(8),
    guide = ggh4x::guide_axis_truncated(),
    labels = label_percent()
  ) +
  scale_colour_identity() +
  coord_cartesian(
    xlim = clock::date_build(2024, c(8, 11), c(1, 30)),
    ylim = c(0.3, 0.7)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Kapphlaupið"
  )

p3 <- d |>
  ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
  geom_vline(
    xintercept = clock::date_build(2024, 11, 30),
    alpha = 0.4
  ) +
  annotate(
    geom = "label",
    label = "Kosningar 30. nóvember",
    x = clock::date_build(2024, 11, 30),
    y = 0.5,
    hjust = 0.5,
    vjust = 1,
    angle = 90,
    fill = "#faf9f9"
  ) +
  geom_smooth_interactive(
    method = "loess",
    span = 0.12,
    se = 0,
    n = 500
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(
      trunc_upper = clock::date_build(2024, 11, 30)
    ),
    limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
    labels = label_date_short(),
    breaks = seq.Date(
      from = clock::date_build(2016, 1),
      to = clock::date_build(2024, 11, 30),
      by = "3 month"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0, 1),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 1
    ),
    labels = label_percent()
  ) +
  scale_colour_identity() +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Fylgisþróun",
    caption = "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni og Rafael Daniel Vias"
  )

design <- "
AABB
CCCC
"

p <- wrap_plots(
  p1, p2, p3,
  design = design,
  heights = c(0.7, 1)
) +
  plot_annotation(
    title = glue::glue("Samanlagt fylgi {str_c(names(samsteypa_flokkar), collapse = ' + ')}"),
    subtitle = str_c(
      "Niðustöður mismunandi kannana vegnar saman með tölfræðilíkani"
    )
  )

p

ggsave(
  here("Figures", glue::glue("majority_{str_c(names(samsteypa_flokkar), collapse = '_')}.png")),
  width = 8,
  height = 0.9 * 8,
  scale = 1.2
)
