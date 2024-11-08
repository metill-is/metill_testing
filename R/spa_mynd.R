make_spa_mynd <- function(coverage_data, colors) {
  coverage_data |>
    filter(
      dags == max(dags)
    ) |>
    ggplot(aes(
      y = flokkur_ordered,
      color = litur,
      group = paste(flokkur, coverage)
    )) +
    annotate(
      geom = "segment",
      x = seq(0.1, 0.25, 0.05),
      xend = seq(0.1, 0.25, 0.05),
      y = 0.5,
      yend = 10.255,
      alpha = 0.2,
      linewidth = 0.2
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        alpha = -coverage
      ),
      linewidth = 5
    ) +
    geom_segment(
      aes(
        x = mean,
        xend = mean,
        y = as.integer(flokkur_ordered) - 0.4,
        yend = as.integer(flokkur_ordered) + 0.4
      ),
      linewidth = 1
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = lower,
        y = as.integer(flokkur_ordered) - 0.255,
        yend = as.integer(flokkur_ordered) + 0.255,
        alpha = -coverage
      ),
      linewidth = 0.2
    ) +
    geom_segment(
      aes(
        x = upper,
        xend = upper,
        y = as.integer(flokkur_ordered) - 0.2,
        yend = as.integer(flokkur_ordered) + 0.2,
        alpha = -coverage
      ),
      linewidth = 0.2
    ) +
    geom_vline(
      xintercept = 0.05,
      linetype = "dashed",
      alpha = 0.4
    ) +
    scale_color_identity() +
    scale_x_continuous(
      breaks = c(0.05, seq(0, 0.25, 0.05)),
      labels = label_percent(),
      limits = c(0, 0.25),
      guide = ggh4x::guide_axis_truncated(
        trunc_lower = 0,
        trunc_upper = 0.25
      ),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_discrete(
      guide = ggh4x::guide_axis_truncated(),
      expand = expansion()
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3)
    ) +
    coord_cartesian(
      xlim = c(0, 0.25),
      ylim = c(0.5, 10.5),
      clip = "off"
    ) +
    theme(
      legend.position = "none",
      axis.text.y = element_markdown(size = 12),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    labs(
      x = NULL,
      y = NULL
    )
}
