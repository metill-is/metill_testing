make_plot3b <- function() {
  d <- read_csv(
    here::here("dashboards", "immigration", "data", "origin_combined.csv")
  )


  sf_data <- ne_countries(scale = "small", returnclass = "sf") |>
    select(name, continent, geometry)

  plot_data <- sf_data |>
    filter(name != "Iceland") |>
    left_join(
      d |>
        filter(
          rikisfang != "Ísland",
          ar == max(ar)
        ) |>
        mutate(
          country = if_else(
            country == "United States",
            "United States of America",
            country
          )
        ),
      by = join_by(name == country)
    ) |>
    arrange(desc(n)) |>
    mutate(
      n = coalesce(n, 0),
      n_group = cut(
        n,
        breaks = c(
          -Inf, 0, 10, 100, 500, 1000, 10000, Inf
        ),
        labels = c(
          "Engin",
          "[1 - 10]",
          "[10 - 100]",
          "[100 - 500]",
          "[500 - 1.000]",
          "[1.000 - 10.000]",
          "[10.000+]"
        )
      )
    )

  p <- ggplot(plot_data, aes(fill = n_group, data_id = n_group)) +
    geom_sf_interactive(
      aes(
        tooltip = glue::glue("{name}: {n}")
      ),
      color = "black",
      linewidth = 0.05
    ) +
    scale_fill_brewer(
      palette = "Greys"
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "transparent")
    ) +
    labs(
      fill = "Fjöldi",
      title = "Frá hvaða löndum komu innflytjendur til Íslands?",
    )


  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.6 * 11,
    bg = "transparent",
    options = list(
      opts_tooltip(
        opacity = 0.5,
        use_fill = FALSE,
        use_stroke = FALSE,
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white;background-color:black;"
      ),
      opts_hover(css = ""),
      opts_hover_inv(css = "opacity:0.05"),
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  )
}
