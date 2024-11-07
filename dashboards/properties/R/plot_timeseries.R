library(tidyverse)
library(metill)
library(ggh4x)
library(scales)
library(mgcv)
theme_set(theme_metill())


d <- read_csv(here::here("dashboards/properties/data/fasteignir_pop.csv"))
pop <- read_csv(here::here("dashboards/properties/data/pop_adult_iceland.csv"))


d <- d |>
  mutate(
    country = "Iceland"
  ) |>
  inner_join(
    pop,
    by = c("ar" = "year")
  )

plot_dat <- d |>
  mutate(
    fasteignir = if_else(
      ar == 2024,
      fasteignir / (10 / 12),
      fasteignir
    )
  ) |>
  mutate(
    new_per_pop = fasteignir / pop * 1000
  )

m <- gam(fasteignir ~ s(ar, bs = "ad"), offset = log(pop), family = nb(), data = plot_dat)

smoothed_dat <- tibble(
  ar = seq(min(plot_dat$ar), max(plot_dat$ar), length.out = 500)
)

smoothed_dat$pred <- predict(m, type = "response", newdata = smoothed_dat, offset = 0) * 1000



plot_dat |>
  mutate(
    pred = predict(m, offset = 0, type = "response") * 1000
  ) |>
  ggplot(aes(x = ar, y = new_per_pop)) +
  geom_line(
    alpha = 0.2
  ) +
  geom_line(
    data = smoothed_dat,
    aes(y = pred),
    col = "black",
    linewidth = 1.5
  ) +
  scale_x_continuous(
    breaks = c(seq(1900, 2010, by = 10), 2024),
    guide = guide_axis_truncated(
      trunc_lower = 1900,
      trunc_upper = 2024
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 14, by = 2),
    guide = guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 14
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Fjöldi nýbyggðra fasteigna á 1.000 fullorðna íbúa Íslands frá 1900 til 2024",
    subtitle = "Grunngögn sýnd með fölum lit | Leitni sýnd með svörtum lit"
  )

ggsave(
  here::here("dashboards/properties/img/img.png"),
  width = 8,
  height = 0.621 * 8,
  scale = 1.3
)
