#' @export
make_deciles_plot <- function(d, ord_var, nm) {
  box::use(
    arrow[read_parquet],
    dplyr[filter, mutate],
    ggiraph[
      geom_col_interactive,
      geom_line_interactive,
      geom_point_interactive,
      girafe,
      opts_hover,
      opts_hover_inv
    ],
    ggplot2[
      aes,
      coord_cartesian,
      ggplot,
      labs,
      scale_color_brewer,
      scale_fill_brewer,
      scale_x_continuous,
      scale_y_continuous,
      theme,
      theme_set
    ],
    glue[glue],
    here[here],
    metill[theme_metill],
    patchwork[plot_annotation, wrap_plots],
    scales[breaks_pretty, label_number, label_percent, number, percent],
    stringr[str_c]
  )

  box::use(
    dashboards / wagesandexpenses / R / prep_plot_data[prep_plot_data]
  )

  theme_set(theme_metill(type = "blog"))

  plot_dat <- prep_plot_data(d, ord_var, nm)

  fill_name <- ord_var

  fill_labels <- c(
    "1" = "Lægstu 10%",
    "2" = "10-20%",
    "3" = "20-30%",
    "4" = "30-40%",
    "5" = "40-50%",
    "6" = "50-60%",
    "7" = "60-70%",
    "8" = "70-80%",
    "9" = "80-90%",
    "10" = "Hæstu 10%"
  )

  p1 <- plot_dat |>
    ggplot(aes(year, value_perc)) +
    geom_col_interactive(
      aes(
        fill = decile,
        tooltip = tooltip,
        data_id = data_id
      ),
      width = 0.95
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_percent(accuracy = 1)
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      labels = fill_labels,
      direction = -1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Skipting",
      fill = fill_name
    )

  p2 <- plot_dat |>
    ggplot(aes(year, value)) +
    geom_col_interactive(
      aes(
        fill = decile,
        tooltip = tooltip,
        data_id = data_id
      ),
      width = 0.95
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_number(
        scale = 1e-9,
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "ma.kr"
      )
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      labels = fill_labels,
      direction = -1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Magn",
      fill = fill_name
    )

  p3 <- plot_dat |>
    ggplot(aes(year, value_per_n)) +
    geom_line_interactive(
      aes(
        group = decile,
        tooltip = tooltip,
        data_id = data_id,
        col = decile
      ),
      linewidth = 1
    ) +
    geom_point_interactive(
      aes(
        tooltip = tooltip,
        data_id = data_id,
        col = decile
      ),
      size = 2
    ) +
    scale_x_continuous(
      expand = c(0.01, 0.01),
      breaks = breaks_pretty()
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = label_number(
        scale = 1e-6,
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "m.kr"
      )
    ) +
    scale_color_brewer(
      palette = "RdBu",
      direction = -1
    ) +
    coord_cartesian(
      clip = "off"
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Á mann"
    )

  design <- "
  111111222222
  333333333333
  "

  p <- wrap_plots(
    p1, p2, p3,
    design = design,
    guides = "collect",
    heights = c(1, 0.9)
  ) +
    plot_annotation(
      title = nm,
      caption = "Sýnt á verðlagi 2024"
    )


  out <- girafe(
    ggobj = p,
    options = list(
      opts_hover_inv(css = "opacity:0.1;"),
      opts_hover(css = "stroke-width:2;")
    )
  )

  out
}
