#' @export
make_fig_fasteignir_tekjur <- function(d) {
  box::use(
    arrow[read_parquet],
    dplyr[filter, mutate],
    ggiraph[
      geom_col_interactive,
      geom_line_interactive,
      geom_point_interactive,
      girafe,
      opts_hover,
      opts_hover_inv,
      opts_toolbar,
      opts_tooltip,
      opts_zoom
    ],
    ggplot2[
      aes,
      coord_cartesian,
      element_blank,
      element_line,
      element_rect,
      element_text,
      ggplot,
      labs,
      margin,
      scale_color_brewer,
      scale_fill_brewer,
      scale_x_continuous,
      scale_y_continuous,
      theme,
      theme_classic,
      theme_set,
      `%+replace%`
    ],
    ggtext[element_markdown, element_textbox_simple],
    glue[glue],
    here[here],
    metill[theme_metill],
    patchwork[plot_annotation, wrap_plots],
    scales[breaks_pretty, label_number, label_percent, number, percent],
    stringr[str_c]
  )

  box::use(
    dashboards / wagesandexpenses / R / prep_plot_data[prep_plot_data],
    dashboards / wagesandexpenses / R / make_fig_vaxtagjold_hlutf_tekjur[make_fig_vaxtagjold_hlutf_tekjur]
  )

  theme_set(theme_metill(type = "blog"))

  title_font_size_sub <- 20

  limits_topright <- c(0, 2700000000000)
  limits_bottom <- c(0, 50000000)

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

  ord_var <- "Ráðstöfunartekjur"
  nm <- "Íbúðalán"

  plot_dat1 <- prep_plot_data(d, ord_var, nm)

  fill_name <- ord_var



  p1 <- plot_dat1 |>
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
      title = glue(
        "<p>Íbúðalán</p><p style='font-size:{title_font_size_sub}px;'>Skipting</p>"
      ),
      fill = fill_name
    ) +
    theme(
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    )

  p2 <- plot_dat1 |>
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
      ),
      limits = limits_topright
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      labels = fill_labels,
      direction = -1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = glue(
        "<p> </p><p style='font-size:{title_font_size_sub}px;'>Magn</p>"
      ),
      fill = fill_name
    ) +
    theme(
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    )

  p3 <- plot_dat1 |>
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
      ),
      limits = limits_bottom
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
      title = glue(
        "<p style='font-size:{title_font_size_sub}px;'>Á mann</p>"
      )
    ) +
    theme(
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    )

  ord_var <- "Ráðstöfunartekjur"
  nm <- "Ráðstöfunartekjur (Tekjur - Skattar)"

  plot_dat2 <- prep_plot_data(d, ord_var, nm)

  fill_name <- ord_var



  p4 <- plot_dat2 |>
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
      title = glue(
        "<p>Ráðstöfunartekjur</p><p style='font-size:{title_font_size_sub}px;'>Skipting</p>"
      ),
      fill = fill_name
    ) +
    theme(
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    )

  p5 <- plot_dat2 |>
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
      ),
      limits = limits_topright
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      labels = fill_labels,
      direction = -1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = glue(
        "<p> </p><p style='font-size:{title_font_size_sub}px;'>Magn</p>"
      ),
      fill = fill_name
    ) +
    theme(
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    )

  p6 <- plot_dat2 |>
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
      ),
      limits = limits_bottom
    ) +
    scale_color_brewer(
      palette = "RdBu",
      direction = -1
    ) +
    coord_cartesian(
      clip = "off"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_textbox_simple(
        margin = margin(b = 10)
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = glue(
        "<p style='font-size:{title_font_size_sub}px;'>Á mann</p>"
      )
    )

  p7 <- make_fig_vaxtagjold_hlutf_tekjur(d)


  design <- "
  111111222222444444555555
  333333333333666666666666
  777777777777777777777777
  "

  p <- wrap_plots(
    p1, p2, p3, p4, p5, p6, p7,
    design = design,
    guides = "collect",
    heights = c(1, 0.9)
  ) +
    plot_annotation(
      caption = "Sýnt á verðlagi 2024"
    )


  girafe(
    ggobj = p,
    width_svg = 12,
    height_svg = 0.621 * 12,
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
}
