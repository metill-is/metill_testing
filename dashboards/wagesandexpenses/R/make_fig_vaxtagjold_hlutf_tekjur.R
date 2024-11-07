#' @export
make_fig_vaxtagjold_hlutf_tekjur <- function(d) {
  box::use(
    arrow[read_parquet],
    dplyr[filter, mutate, select],
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
    stringr[str_c],
    tidyr[pivot_wider]
  )

  box::use(
    dashboards / wagesandexpenses / R / prep_plot_data[prep_plot_data]
  )

  theme_set(theme_metill(type = "blog"))

  plot_dat <- d |>
    filter(
      order_var == "Ráðstöfunartekjur",
      name %in% c(
        "Vaxtagjöld v/íbúðalána",
        "Ráðstöfunartekjur (Tekjur - Skattar)"
      )
    ) |>
    select(-n) |>
    pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    mutate(
      value = `Vaxtagjöld v/íbúðalána` / `Ráðstöfunartekjur (Tekjur - Skattar)`
    ) |>
    select(
      year,
      decile,
      value
    ) |>
    mutate(
      decile = factor(decile, levels = 10:1),
      tooltip = glue(
        str_c(
          "<b>{year}</b><br>",
          "Tíundarhluti: {decile}<br>",
          "Hlutfall: {percent(value, accuracy = 1, decimal.mark = ',', big.mark = '.')}<br>"
        )
      ),
      data_id = paste0(year, "_", decile)
    )

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

  p <- plot_dat |>
    ggplot(aes(year, value)) +
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
      labels = label_percent(
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "%"
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
      title = "Vaxtagjöld v/íbúðalána sem hlutfall af ráðstöfunartekjum"
    )

  return(p)
}
