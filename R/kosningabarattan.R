#' @export
make_kosningabarattan_plot <- function(d, coverage_data, colors, polling_data, point_shapes) {
  p <- d |>
    ggplot(aes(dags, median, colour = litur, data_id = flokkur)) +
    annotate(
      geom = "segment",
      x = clock::date_build(2021, 8, 1),
      xend = clock::date_build(2024, 11, 30),
      y = seq(0, 0.3, by = 0.05),
      yend = seq(0, 0.3, by = 0.05),
      alpha = 0.4,
      linewidth = 0.05
    ) +
    annotate(
      geom = "segment",
      x = clock::date_build(2024, 11, 30),
      xend = clock::date_build(2024, 11, 30),
      y = 0,
      yend = 0.3,
      alpha = 0.4,
      linewidth = 0.5
    ) +
    annotate(
      geom = "label",
      label = "Kosningar 30. nóvember",
      x = clock::date_build(2024, 11, 30),
      y = 0.18,
      hjust = 0.5,
      vjust = 1,
      angle = 90,
      fill = "#faf9f9"
    ) +
    geom_text_interactive(
      data = colors |>
        filter(flokkur != "Annað") |>
        mutate(
          label = str_c("x", c("D", "B", "S", "V", "C", "P", "M", "F", "J", "L"))
        ) |>
        arrange(label) |>
        mutate(
          x = seq.Date(
            from = clock::date_build(2024, 8, 8),
            to = clock::date_build(2024, 11, 23),
            length.out = 10
          ),
          flokkur = str_to_sentence(flokkur)
        ),
      aes(
        x = x,
        label = label,
        col = litur,
        y = 0.3,
        data_id = flokkur
      ),
      inherit.aes = FALSE,
      size = 11,
      vjust = 0,
      fontface = "bold"
    ) +
    geom_ribbon_interactive(
      data = coverage_data |>
        filter(
          # dags <= max(polling_data$dags)
          coverage >= 0.3
        ),
      aes(
        x = dags,
        ymin = lower,
        ymax = upper,
        alpha = -sqrt(coverage),
        fill = litur,
        data_id = flokkur,
        group = str_c(flokkur, coverage)
      ),
      inherit.aes = FALSE
    ) +
    geom_line_interactive(
      data = ~ filter(.x, dags <= max(polling_data$dags)),
      linewidth = 0.6,
      alpha = 0.5
    ) +
    geom_point_interactive(
      aes(y = p_poll, shape = fyrirtaeki, fill = litur),
      alpha = 0.3
    ) +
    scale_x_date(
      guide = ggh4x::guide_axis_truncated(
        trunc_upper = clock::date_build(2024, 11, 30)
      ),
      limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
      labels = label_date_short(),
      breaks = clock::date_build(
        2024,
        c(
          8, 8, 8, 8,
          9, 9, 9, 9,
          10, 10, 10, 10,
          11, 11, 11, 11, 11
        ),
        c(
          1, 8, 15, 22,
          1, 8, 15, 22,
          1, 8, 15, 22,
          1, 8, 15, 22, 30
        )
      ),
      expand = expansion(add = c(0, 6))
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.3, by = 0.05),
      limits = c(0, NA),
      guide = ggh4x::guide_axis_truncated(),
      labels = label_percent()
    ) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_alpha_continuous(
      range = c(0.03, 0.04)
    ) +
    scale_shape_manual(
      values = point_shapes,
      name = "Framkvæmd:",
      na.translate = FALSE
    ) +
    guides(
      fill = "none",
      alpha = "none",
      shape = guide_legend(override.aes = list(alpha = 0.8))
    ) +
    coord_cartesian(
      xlim = clock::date_build(2024, c(8, 11), c(1, 30)),
      ylim = c(0, 0.31)
    ) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.box.margin = margin(6, 6, 6, 6)
    ) +
    labs(
      x = NULL,
      y = NULL
    )


  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.621 * 11,
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
