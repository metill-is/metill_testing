make_plot4 <- function() {
  d <- read_csv(
    here::here("dashboards", "immigration", "data", "unemployment.csv")
  )

  p1 <- d_hlutf |>
    filter(name != "p") |>
    mutate(
      name = fct_recode(
        name,
        "Fædd erlendis" = "foreign_country",
        "Innfædd" = "reporting_country"
      )
    ) |>
    ggplot(aes(time, value / 100)) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      data = ~ filter(.x, land != "Ísland"),
      aes(group = land),
      linewidth = lw_other,
      alpha = 0.2
    ) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      data = ~ filter(.x, land == "Ísland"),
      linewidth = lw_iceland,
      col = "darkblue",
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated(),
      limits = clock::date_build(c(2010, 2024))
    ) +
    scale_y_continuous(
      labels = label_hlutf(),
      limits = c(0, 0.4),
      guide = guide_axis_truncated()
    ) +
    coord_cartesian(
      ylim = c(0, 0.2)
    ) +
    facet_wrap("name") +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Atvinnuleysi eftir fæðingarlandi",
    )



  p2 <- d_hlutf |>
    filter(name == "p") |>
    ggplot(aes(time, value_trend)) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      data = ~ filter(.x, land != "Ísland"),
      aes(group = land),
      linewidth = lw_other,
      alpha = 0.2
    ) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      n = 1000,
      data = ~ filter(.x, land == "Ísland"),
      linewidth = lw_iceland,
      col = "darkblue",
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated(),
      limits = clock::date_build(c(2010, 2024))
    ) +
    scale_y_continuous(
      labels = \(x) {
        out <- number(x, suffix = "x hærra", accuracy = 1, big.mark = ".", decimal.mark = ",")
        if_else(
          out == "0x hærra",
          "Jafnhátt",
          out
        )
      },
      limits = c(0, 4),
      guide = guide_axis_truncated()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Er atvinnuleysi hærra hjá fólki fæddu erlendis?",
    )


  p3 <- d_atv |>
    ggplot(aes(time, p_trend)) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      data = ~ filter(.x, land != "Ísland"),
      aes(group = land),
      linewidth = lw_other,
      alpha = 0.2
    ) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      n = 1000,
      data = ~ filter(.x, land == "Ísland"),
      linewidth = lw_iceland,
      col = "darkblue",
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated(),
      limits = clock::date_build(c(2010, 2024))
    ) +
    scale_y_continuous(
      labels = label_hlutf(),
      limits = c(0, 1),
      guide = guide_axis_truncated()
    ) +
    coord_cartesian(
      ylim = c(0, 0.62)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Hlutfall innflytjenda af öllum starfandi",
    )



  p4 <- d_fj |>
    ggplot(aes(time, p_trend)) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      data = ~ filter(.x, land != "Ísland"),
      aes(group = land),
      linewidth = lw_other,
      alpha = 0.2
    ) +
    stat_smooth(
      geom = "line",
      span = 0.2,
      n = 1000,
      data = ~ filter(.x, land == "Ísland"),
      linewidth = lw_iceland,
      col = "darkblue",
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated(),
      limits = clock::date_build(c(2010, 2024))
    ) +
    scale_y_continuous(
      breaks = breaks_width(0.2),
      labels = label_hlutf(),
      limits = c(0, 1),
      guide = guide_axis_truncated()
    ) +
    coord_cartesian(
      ylim = c(0, 0.8)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Hlutfall innflytjenda af skráðum atvinnulausum",
    )








  design <- "
AAAAAAAAAAAAAAAAAAAA
CCCCCCCCCCDDDDDDDDDD
"



  p <- wrap_plots(list(p1, p3, p4)) +
    plot_layout(design = design, nrow = 2) +
    plot_annotation(
      title = "Samanburður á atvinnuleysi eftir fæðingarlandi á Íslandi og samanburðarlöndum í Evrópu",
      subtitle = str_c(
        "Hækkandi hlutfall innflytjenda meðal skráðra atvinnulausra á Íslandi má nær eingöngu rekja til aukins innflutts vinnuafls eftir COVID-19"
      ),
      caption = "Myndir sýna ársmeðaltöl hverju sinni"
    )

  p
}
