library(tidyverse)
library(here)
library(metill)
library(ggtext)
library(ggh4x)
library(ggiraph)
library(patchwork)

theme_set(theme_metill(type = "blog"))
d <- read_csv(here("dashboards/kindergardens/data/kindergardens.csv"))

samtals <- d |>
  group_by(ar, aldur) |>
  summarise_at(
    vars(n_leik, n_heild),
    sum
  ) |>
  ungroup() |>
  mutate(sveitarfelag = "Samtals")

plot_dat <- d |>
  bind_rows(samtals) |>
  filter(
    aldur == "1 árs",
    n_heild > 20
  ) |>
  mutate(
    hlutf = n_leik / n_heild
  ) |>
  filter(
    any(ar == 2023),
    .by = sveitarfelag
  )

p1 <- plot_dat |>
  filter(
    ar %in% (max(ar) - c(0, 1)),
  ) |>
  select(sveitarfelag, ar, hlutf) |>
  mutate(
    ar = if_else(ar == max(ar), "post", "pre")
  ) |>
  pivot_wider(names_from = ar, values_from = hlutf) |>
  drop_na() |>
  mutate(
    sveitarfelag_ordered = fct_reorder(
      sveitarfelag, post * (!str_detect(sveitarfelag, "Samtals"))
    ),
    increase = factor(1 * (post > pre))
  ) |>
  ggplot(aes(y = sveitarfelag_ordered, data_id = sveitarfelag)) +
  geom_text_interactive(
    aes(x = 0, label = sveitarfelag, col = increase),
    hjust = 1
  ) +
  geom_point_interactive(
    aes(x = pre, col = increase),
    size = 3
  ) +
  geom_segment_interactive(
    aes(
      x = pre,
      xend = post,
      yend = sveitarfelag_ordered,
      col = increase
    ),
    arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  geom_segment_interactive(
    aes(
      x = post,
      xend = 0,
      yend = sveitarfelag_ordered
    ),
    lty = 5,
    alpha = 0.07,
    linewidth = 0.2
  ) +
  scale_x_continuous(
    labels = label_percent(),
    breaks = breaks_width(0.25),
    limits = c(0, 1),
    expand = expansion(),
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  coord_cartesian(clip = "off", xlim = c(-0.1, 1)) +
  theme(
    legend.position = "none",
    plot.margin = margin(c(5, 25, 5, 15)),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL
  )

p2 <- plot_dat |>
  ggplot(aes(ar, hlutf, data_id = sveitarfelag)) +
  geom_point_interactive(
    aes(group = sveitarfelag),
    alpha = 0.5
  ) +
  geom_smooth_interactive(
    aes(group = sveitarfelag),
    se = 0,
    col = "black",
    span = 0.4
  ) +
  scale_x_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = label_percent(),
    breaks = breaks_width(0.1),
    guide = ggh4x::guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL
  )


p <- p1 + p2 +
  plot_layout(ncol = 1)


girafe(
  ggobj = p,
  width_svg = 11,
  height_svg = 0.9 * 11,
  bg = "transparent",
  options = list(
    opts_tooltip(
      opacity = 0.8,
      use_fill = TRUE,
      use_stroke = FALSE,
      css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
    ),
    opts_hover(css = ""),
    opts_hover_inv(css = "opacity:0.05"),
    opts_toolbar(saveaspng = TRUE),
    opts_zoom(max = 1)
  )
)
