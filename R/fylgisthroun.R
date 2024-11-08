#' @export
make_fylgisthroun_plot <- function(d, coverage_data, colors, polling_data, point_shapes) {
  p <- d |>
    filter(dags >= clock::date_build(2021, 1, 1)) |>
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
    #    geom_ribbon_interactive(
    #      data = coverage_data,
    #      aes(
    #        x = dags,
    #        ymin = lower,
    #        ymax = upper,
    #        alpha = -coverage,
    #        fill = litur,
    #        data_id = flokkur,
    #        group = str_c(flokkur, coverage)
    #      ),
    #      inherit.aes = FALSE
    #    ) +
    geom_line_interactive(
      linewidth = 1
    ) +
    geom_point_interactive(
      aes(
        y = p_poll,
        shape = fyrirtaeki,
        color = litur,
        fill = litur
      ),
      alpha = 0.2
    ) +
    geom_point_interactive(
      data = ~ filter(.x, fyrirtaeki == "Kosning"),
      aes(y = p_poll, x = dags, shape = " Kosningar"),
      alpha = 1,
      size = 4
    ) +
    scale_alpha_continuous(
      range = c(0, 0.1)
    ) +
    scale_shape_manual(
      values = point_shapes,
      name = "Könnunarfyrirtæki:"
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    guides(
      fill = "none",
      alpha = "none",
      shape = guide_legend(override.aes = list(alpha = 0.8))
    ) +
    scale_x_date(
      guide = ggh4x::guide_axis_truncated(
        trunc_upper = clock::date_build(2024, 11, 30)
      ),
      limits = c(NA_Date_, clock::date_build(2024, 11, 30)),
      labels = label_date_short(),
      breaks = seq.Date(
        from = clock::date_build(2021, 1),
        to = clock::date_build(2024, 11, 30),
        by = "2 month"
      ),
      expand = expansion(add = c(0, 25))
    ) +
    scale_y_continuous(
      breaks = seq(0, 0.3, by = 0.05),
      guide = ggh4x::guide_axis_truncated(),
      labels = label_percent(),
      expand = expansion()
    ) +
    coord_cartesian(
      xlim = clock::date_build(c(2021, 2024), c(9, 11), c(1, 30)),
      ylim = c(0, 0.32)
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.box.margin = margin(6, 6, 6, 6)
    )


  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.7 * 11,
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
