library(here)
library(tidyverse)
library(geomtextpath)
library(ggh4x)

d_wf <- read_csv(here("articles", "workforce", "data", "vinnuafl.csv"))

d_pop <- read_csv(here("dashboards", "immigration", "data", "origin.csv"))

d <- d_wf |>
  filter(
    kyn == "Alls",
    rekstrarform == "Alls starfandi"
  ) |>
  select(dags, bakgrunnur, starfandi) |>
  inner_join(
    d_pop |>
      mutate(
        bakgrunnur = if_else(
          rikisfang == "Ísland",
          "Íslenskur bakgrunnur",
          "Innflytjendur"
        )
      ) |>
      mutate(
        dags = clock::date_build(ar)
      ) |>
      count(dags, bakgrunnur, wt = n, name = "pop")
  )

p <- d |>
  pivot_longer(c(-dags, -bakgrunnur)) |>
  mutate(
    index = value / value[dags == min(dags)],
    .by = c(name, bakgrunnur)
  ) |>
  filter(
    bakgrunnur == "Innflytjendur"
  ) |>
  mutate(
    name = fct_recode(
      name,
      "Mannfjöldi" = "pop",
      "Fjöldi starfandi" = "starfandi"
    )
  ) |>
  ggplot(aes(dags, index)) +
  geom_hline(
    yintercept = 1,
    lty = 2,
    alpha = 0.3,
    linewidth = 0.3
  ) +
  geom_textline(
    aes(col = name, label = name, hjust = name),
    linewidth = 1,
    size = 6
  ) +
  scale_x_date(
    guide = guide_axis_truncated(),
    breaks = breaks_width("2 year", offset = "1 year"),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(),
    labels = \(x) percent(x - 1),
    breaks = breaks_width(0.25)
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_hjust_manual(
    values = c(0.93, 0.9)
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Starfandi innflytjendum hefur fjölgað jafnhratt og erlendum fullorðnum íbúum",
    subtitle = "Hlutfallsleg fjölgun fullorðinna og starfandi innflytjenda í janúar hvers árs",
    caption = "Gögn Hagstofu um starfandi samkvæmt skrám og íbúafjölda eftir ríkisfangi"
  )

p
ggsave(
  here(
    "dashboards",
    "immigration",
    "img",
    "plot2c.png"
  ),
  p,
  width = 8,
  height = 0.5 * 8,
  scale = 1.4
)
