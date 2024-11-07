make_plot3a <- function() {
  d <- read_csv(
    here::here("dashboards", "immigration", "data", "origin_combined.csv")
  )

  rikisfong <- d |>
    summarise(
      n = sum(n),
      .by = rikisfang
    ) |>
    top_n(n = 35, wt = n) |>
    filter(
      rikisfang != "Ísland"
    )

  plot_data <- d |>
    filter(
      ar == max(ar),
      rikisfang != "Ísland"
    ) |>
    arrange(desc(n)) |>
    mutate(
      rikisfang = if_else(
        row_number() <= 65,
        rikisfang,
        "Annað"
      )
    ) |>
    count(rikisfang, ar, wt = n, name = "n") |>
    mutate(
      rikisfang2 = glue("{rikisfang} ({number(n, big.mark = '.',decimal.mark = ',')})") |>
        fct_reorder(n * (rikisfang != "Annað"))
    ) |>
    arrange(desc(rikisfang2)) |>
    mutate(
      total = sum(n),
      total = if_else(
        row_number() == 1,
        total,
        NA
      ),
      n = cumsum(n),
      xend = lag(n, default = 0)
    )

  p1 <- ggplot(plot_data, aes(n, rikisfang2)) +
    geomtextpath::geom_labelvline(
      aes(
        xintercept = total,
        label = glue("Samtals {number(total, big.mark = '.',decimal.mark = ',')} innflytjendur árið {ar}")
      ),
      lty = 1
    ) +
    geom_segment_interactive(
      aes(xend = xend, yend = rikisfang2, tooltip = rikisfang2, data_id = rikisfang)
    ) +
    geom_point_interactive(
      aes(tooltip = rikisfang2, data_id = rikisfang),
      shape = "|",
      size = 3
    ) +
    geom_point_interactive(
      aes(x = xend, tooltip = rikisfang2, data_id = rikisfang),
      shape = "|",
      size = 3
    ) +
    geom_text_interactive(
      aes(x = 0, label = str_c(rikisfang2, " "), data_id = rikisfang),
      hjust = 1,
      size = 3.5
    ) +
    scale_x_continuous(
      breaks = tufte_breaks(c(0, max(plot_data$n))),
      labels = label_number(),
      limits = c(0, NA),
      expand = expansion(c(0, 0.1)),
      guide = guide_axis_truncated()
    ) +
    scale_y_discrete(
      guide = guide_axis_truncated()
    ) +
    labs(
      x = "Samanlagður fjöldi innflytjenda",
      y = NULL,
      title = "Innflytjendur á Íslandi (2024)",
      subtitle = "Frá hvaða löndum komu innflytjendur til Íslands?",
      caption = "Mynd teiknuð úr gögnum Hagstofu:\nhttps://px.hagstofa.is/pxis/pxweb/is/Ibuar/Ibuar__mannfjoldi__3_bakgrunnur__Rikisfang/MAN04103.px"
    ) +
    coord_cartesian(clip = "off", xlim = c(-1.3e4, NA)) +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
    )


  p <- p1

  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.9 * 11,
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
