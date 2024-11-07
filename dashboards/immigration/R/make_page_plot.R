library(tidyverse)
library(metill)
library(hagstofa)
library(gganimate)
library(ggh4x)
theme_set(theme_metill(type = "blog"))

d <- read_csv(
  here::here("dashboards", "immigration", "data", "origin.csv")
)


rikisfong <- d |>
  summarise(
    n = sum(n),
    .by = rikisfang
  ) |>
  top_n(n = 40, wt = n) |>
  filter(
    rikisfang != "Ísland"
  )

d |>
  filter(
    ar == max(ar),
    rikisfang != "Ísland"
  ) |>
  arrange(desc(n)) |>
  mutate(
    rikisfang = if_else(
      row_number() <= 40,
      rikisfang,
      "Annað"
    )
  ) |>
  count(rikisfang, ar, wt = n, name = "n") |>
  mutate(
    rikisfang2 = glue::glue("{rikisfang} ({number(n, big.mark = '.',decimal.mark = ',')})") |>
      fct_reorder(n * (rikisfang != "Annað"))
  ) |>
  arrange(desc(rikisfang2)) |>
  mutate(
    total = sum(n),
    total = if_else(
      row_number() == 1,
      total,
      NA
    ),
    n = cumsum(n),
    xend = lag(n, default = 0)
  ) |>
  # slice_head(n = 70) |>
  ggplot(aes(n, rikisfang2)) +
  geomtextpath::geom_labelvline(
    aes(
      xintercept = total,
      label = glue::glue("Samtals {number(total, big.mark = '.',decimal.mark = ',')} innflytjendur árið {ar}")
    ),
    lty = 1
  ) +
  geom_segment(
    aes(xend = xend, yend = rikisfang2)
  ) +
  geom_point(
    shape = "|",
    size = 3
  ) +
  geom_point(
    shape = "|",
    size = 3,
    aes(x = xend)
  ) +
  scale_x_continuous(
    breaks = tufte_breaks(c(0, 63528)),
    labels = label_number(),
    limits = c(0, NA),
    expand = expansion(c(0, 0.1)),
    guide = guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Samanlagður fjöldi innflytjenda",
    y = "Land (Fjöldi)",
    title = "Innflytjendur á Íslandi (2024)",
    subtitle = "Frá hvaða löndum komu innflytjendur til Íslands?",
    caption = "Mynd teiknuð úr gögnum Hagstofu:\nhttps://px.hagstofa.is/pxis/pxweb/is/Ibuar/Ibuar__mannfjoldi__3_bakgrunnur__Rikisfang/MAN04103.px"
  )


ggsave(
  filename = here::here("dashboards", "immigration", "img", "innfl.png"),
  width = 8, height = 0.621 * 8, scale = 1.3
)
