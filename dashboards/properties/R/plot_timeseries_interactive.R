#' @export
plot_timeseries_interactive <- function() {
  library(tidyverse)
  library(metill)
  library(ggh4x)
  library(scales)
  library(mgcv)
  library(ggiraph)
  theme_set(theme_metill(type = "blog"))


  d <- read_csv(here::here("dashboards/properties/data/fasteignir_pop.csv"))
  pop <- read_csv(here::here("dashboards/properties/data/pop_adult_iceland.csv"))


  d <- d |>
    mutate(
      country = "Iceland"
    ) |>
    inner_join(
      pop,
      by = c("ar" = "year", "country")
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
      new_per_pop = fasteignir / population_adult * 1000
    )

  m <- gam(fasteignir ~ s(ar, bs = "ad"), offset = log(population_adult), family = nb(), data = plot_dat)

  smoothed_dat <- tibble(
    ar = seq(min(plot_dat$ar), max(plot_dat$ar), length.out = 500)
  )

  smoothed_dat$pred <- predict(m, type = "response", newdata = smoothed_dat, offset = 0) * 1000

  plot_dat <- plot_dat |>
    mutate(
      pred = predict(m, offset = 0, type = "response") * 1000,
      tooltip = str_c(
        "Ár: ", ar, "\n",
        "Fullorðnir íbúar: ", number(population_adult, accuracy = 1, big.mark = ".", decimal.mark = ","), "\n",
        "Nýjar fasteignir: ", number(fasteignir, accuracy = 1, big.mark = ".", decimal.mark = ","), "\n",
        "Á 1.000 íbúa: ", number(new_per_pop, accuracy = 0.1, big.mark = ".", decimal.mark = ","), "\n",
        "Leitni: ", number(pred, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
      )
    )

  p <- plot_dat |>
    ggplot(aes(x = ar, y = new_per_pop)) +
    geom_line_interactive(
      alpha = 0.2
    ) +
    geom_point_interactive(
      aes(
        tooltip = tooltip,
        group = "none",
        data_id = ar
      ),
      alpha = 0.7,
      size = 2,
      hover_nearest = TRUE
    ) +
    geom_line_interactive(
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
      breaks = seq(0, 20, by = 2),
      guide = guide_axis_truncated(
        trunc_lower = 0,
        trunc_upper = 20
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Fjöldi nýbyggðra fasteigna á 1.000 fullorðna íbúa Íslands frá 1900 til 2024",
      subtitle = "Grunngögn sýnd með fölum lit | Leitni sýnd með svörtum lit"
    )


  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.9 * 11,
    bg = "transparent",
    options = list(
      opts_tooltip(
        offx = -100,
        offy = 100,
        use_cursor_pos = TRUE,
        opacity = 0.9,
        use_fill = TRUE,
        use_stroke = FALSE,
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
      ),
      opts_hover(css = "fill:red;stroke:red;stroke-width:3px"),
      opts_hover_inv(css = "opacity:0.1"),
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  )
}
