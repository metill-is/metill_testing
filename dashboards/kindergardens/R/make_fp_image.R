library(tidyverse)
library(here)
library(metill)
library(ggtext)
library(ggh4x)

theme_set(theme_metill(type = "blog"))
d <- read_csv(here("dashboards/kindergardens/data/kindergardens.csv"))

samtals <- d |>
  group_by(ar, aldur) |>
  summarise_at(
    vars(n_leik, n_heild),
    sum
  ) |>
  ungroup() |>
  mutate(
    sveitarfelag = "<b style='font-size:18px;'>Samtals</b>"
  )


d |>
  bind_rows(
    samtals
  ) |>
  filter(
    aldur == "1 árs",
    ar %in% (max(ar) - c(0, 1)),
    n_heild > 20
  ) |>
  mutate(
    hlutf = n_leik / n_heild
  ) |>
  select(sveitarfelag, ar, hlutf) |>
  mutate(
    ar = if_else(ar == max(ar), "post", "pre")
  ) |>
  pivot_wider(names_from = ar, values_from = hlutf) |>
  drop_na() |>
  mutate(
    sveitarfelag = fct_reorder(sveitarfelag, post * (!str_detect(sveitarfelag, "Samtals"))),
    increase = factor(1 * (post > pre))
  ) |>
  ggplot(aes(y = sveitarfelag)) +
  geom_point(
    aes(x = pre, col = increase),
    size = 3
  ) +
  geom_segment(
    aes(
      x = pre,
      xend = post,
      yend = sveitarfelag,
      col = increase
    ),
    arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  geom_segment(
    aes(
      x = post,
      xend = 0,
      yend = sveitarfelag
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
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(),
    plot.margin = margin(c(5, 25, 5, 15))
  ) +
  labs(
    title = "Hlutfall eins árs barna með leikskólapláss lækkaði í flestum sveitarfélögum frá 2022 til 2023",
    subtitle = "Reiknað sem fjöldi leikskólaplássa í leikskólum hvers sveitarfélags eftir aldri deilt með fjölda barna á viðeigandi aldri sem búa í því sveitarfélagi",
    x = "Breyting frá 2022 til 2023",
    y = NULL,
    caption = str_c(
      "Byggt á gögnum Hagstofu um Leikskóla og mannfjöldatölum Hagstofu | ",
      "Sýnt fyrir sveitarfélög með fleiri en 20 skráð börn á eins árs aldri samkvæmt Hagstofu"
    )
  )

ggsave(
  filename = here::here("dashboards", "kindergardens", "img", "fp.png"),
  width = 8, height = 0.55 * 8, scale = 1.75
)
