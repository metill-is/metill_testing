#' @export
skuldir_hlutf_tekjur <- function(d) {
  box::use(
    arrow[read_parquet],
    dplyr[
      arrange,
      case_when,
      filter,
      lag,
      lead,
      if_else,
      left_join,
      mutate,
      pull,
      select,
      summarise
    ],
    ggh4x[guide_axis_truncated],
    ggiraph[
      geom_col_interactive,
      geom_label_interactive,
      geom_line_interactive,
      geom_point_interactive,
      geom_text_interactive,
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
      element_rect,
      ggplot,
      ggsave,
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
    scales[breaks_extended, breaks_pretty, label_number, label_percent, number, percent],
    stringr[str_c],
    tidyr[pivot_wider],
    visitalaneysluverds[vnv_convert]
  )

  theme_set(theme_metill(type = "blog"))

  d <- here("dashboards", "wagesandexpenses", "data", "deciles.parquet") |>
    read_parquet()

  d$value <- vnv_convert(d$value, d$year)

  plot_dat <- d |>
    filter(
      order_var == "Ráðstöfunartekjur",
      name %in% c(
        "Skuldir alls",
        "Ráðstöfunartekjur (Tekjur - Skattar)"
      )
    ) |>
    pivot_wider(
      names_from = name,
      values_from = value
    ) |>
    mutate(
      value = `Skuldir alls` / `Ráðstöfunartekjur (Tekjur - Skattar)`
    ) |>
    select(
      year,
      decile,
      value,
      tekjur = `Ráðstöfunartekjur (Tekjur - Skattar)`,
      n
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

  intervals <- plot_dat |>
    filter(year == 2023) |>
    summarise(
      tekjur_a_mann = tekjur / n * 1e6 / 12,
      .by = decile
    ) |>
    mutate(
      lower = lag(tekjur_a_mann),
      upper = lead(tekjur_a_mann),
      lower = tekjur_a_mann - (tekjur_a_mann - lower) / 2,
      upper = tekjur_a_mann + (upper - tekjur_a_mann) / 2,
      interval = case_when(
        decile == 1 ~ glue("0 kr. - {number(upper, accuracy = 5e3, big.mark = '.', decimal.mark = ',')} kr."),
        decile == 10 ~ glue("{number(lower, accuracy = 5e3, big.mark = '.', decimal.mark = ',')}+ kr."),
        TRUE ~ glue("{number(lower, accuracy = 5e3, big.mark = '.', decimal.mark = ',')} - {number(upper, accuracy = 5e3, big.mark = '.', decimal.mark = ',')} kr.")
      ),
      interval = if_else(
        decile == 1,
        str_c("Ráðstöfunartekjur:\n", interval),
        interval
      )
    )

  plot_dat <- plot_dat |>
    left_join(intervals, by = "decile") |>
    mutate(
      label = if_else(
        year == 2023,
        interval,
        NA_character_
      ),
      y = value + 0.025 * (decile == 8) - 0.025 * (decile == 7) + 0.08 * (decile == 1)
    ) |>
    arrange(desc(year))

  p <- plot_dat |>
    ggplot(
      aes(
        year,
        value,
        data_id = decile,
        col = decile
      )
    ) +
    geom_line_interactive(
      aes(
        group = decile
      ),
      linewidth = 1
    ) +
    geom_point_interactive(
      aes(tooltip = tooltip),
      size = 2
    ) +
    geom_label_interactive(
      aes(
        label = label,
        tooltip = tooltip,
        y = y
      ),
      size = 5,
      nudge_x = 0.3,
      hjust = 0,
      fill = NA,
      label.size = 0
    ) +
    scale_x_continuous(
      expand = c(0.01, 0.01),
      breaks = seq(1997, 2023, by = 2),
      guide = guide_axis_truncated(
        trunc_lower = 1997,
        trunc_upper = 2023
      )
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      labels = label_percent(
        accuracy = 1,
        decimal.mark = ",",
        big.mark = ".",
        suffix = "%"
      ),
      breaks = breaks_extended(n = 10),
      guide = guide_axis_truncated(
        trunc_lower = 0.5,
        trunc_upper = 3
      )
    ) +
    scale_color_brewer(
      palette = "RdBu",
      direction = -1,
      labels = intervals$interval
    ) +
    scale_fill_brewer(
      palette = "RdBu",
      direction = -1,
      labels = intervals$interval
    ) +
    coord_cartesian(
      clip = "on",
      ylim = c(0, 3.4),
      xlim = c(1997, 2030)
    ) +
    labs(
      x = NULL,
      y = "Skuldir sem hlutfall af árlegum ráðstöfunartekjum",
      title = "Skuldir Íslendinga hafa lækkað sem hlutfall af ráðstöfunartekjum síðustu árin",
      subtitle = "Sýnt eftir mánaðarlegum ráðstöfunartekjum",
      col = "Ráðstöfunartekjur"
    ) +
    theme(
      legend.position = "none"
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
