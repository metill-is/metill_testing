make_plot4a <- function() {
  library(tidyverse)
  library(eurostat)
  library(metill)
  library(ggh4x)
  library(geomtextpath)
  library(glue)
  library(ggtext)
  library(patchwork)
  library(ggiraph)
  theme_set(theme_metill(type = "blog"))

  litur_island <- "#08306b"
  litur_danmork <- "#e41a1c"
  litur_finnland <- "#3690c0"
  litur_noregur <- "#7f0000"
  litur_svithjod <- "#fd8d3c"
  litur_annad <- "#737373"



  offenders <- get_eurostat(
    "crim_just_ctz"
  )

  offenders <- offenders |>
    label_eurostat()

  pop <- get_eurostat(
    "migr_pop1ctz",
    filters = list(
      age = "Y15-64",
      citizen = c("NAT", "FOR_STLS"),
      sex = "T"
    )
  )

  pop <- pop |>
    label_eurostat()

  pop <- pop |>
    janitor::remove_constant() |>
    inner_join(
      metill::country_names(),
      by = join_by(geo == country)
    ) |>
    rename(pop = values) |>
    mutate(
      citizen = if_else(
        str_detect(citizen, "Foreign country"),
        "Foreign country",
        citizen
      )
    )

  plot_dat <- offenders |>
    filter(
      unit != "Per hundred thousand inhabitants"
    ) |>
    select(
      -freq,
      -unit
    ) |>
    rename(time = TIME_PERIOD) |>
    inner_join(
      metill::country_names(),
      by = join_by(geo == country)
    ) |>
    inner_join(
      pop |>
        mutate(
          pop = zoo::na.approx(pop, na.rm = FALSE),
          .by = c(geo, land, citizen)
        ),
      by = join_by(land, geo, time, citizen)
    )

  p1 <- plot_dat |>
    mutate(
      values = values / pop
    ) |>
    select(-pop) |>
    pivot_wider(names_from = citizen, values_from = values) |>
    janitor::clean_names() |>
    mutate(
      value = foreign_country / reporting_country
    ) |>
    filter(
      time == max(time),
      .by = land
    ) |>
    filter(
      value > 1,
      leg_stat == "Suspected person"
    ) |>
    drop_na() |>
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    ) |>
    mutate(
      land = fct_reorder(land, value)
    ) |>
    ggplot(aes(value - 1, land, col = colour, data_id = geo)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(land, " ")),
      hjust = 1,
      size = 3.5
    ) +
    geom_segment_interactive(
      aes(xend = 0, yend = land),
      linewidth = 0.1, alpha = 1
    ) +
    geom_point_interactive(size = 3) +
    scale_y_discrete(
      guide = guide_axis_truncated()
    ) +
    scale_x_continuous(
      limits = c(NA, NA),
      expand = expansion(c(0, 0.1)),
      breaks = seq(0, 10, by = 2),
      labels = \(x) {
        out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
        if_else(
          out == "0x fleiri",
          "Jafnmargir",
          out
        )
      },
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    coord_cartesian(clip = "off", xlim = c(-1, NA)) +
    theme(
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Grunaðir um glæp (2022)"
    )

  p2 <- plot_dat |>
    mutate(
      values = values / pop
    ) |>
    select(-pop) |>
    pivot_wider(names_from = citizen, values_from = values) |>
    janitor::clean_names() |>
    mutate(
      value = foreign_country / reporting_country
    ) |>
    filter(
      time == max(time),
      .by = land
    ) |>
    filter(
      value > 1,
      leg_stat == "Prosecuted person"
    ) |>
    drop_na() |>
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    ) |>
    mutate(
      land = fct_reorder(land, value)
    ) |>
    ggplot(aes(value - 1, land, col = colour, data_id = geo)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(land, " ")),
      hjust = 1,
      size = 3.5
    ) +
    geom_segment_interactive(
      aes(xend = 0, yend = land),
      linewidth = 0.1, alpha = 1
    ) +
    geom_point_interactive(size = 3) +
    scale_y_discrete(
      guide = guide_axis_truncated()
    ) +
    scale_x_continuous(
      limits = c(0, NA),
      expand = expansion(c(0, 0.1)),
      breaks = seq(0, 10, by = 1),
      labels = \(x) {
        out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
        if_else(
          out == "0x fleiri",
          "Jafnmargir",
          out
        )
      },
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    coord_cartesian(clip = "off", xlim = c(-1, NA)) +
    theme(
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Saksóttir (2022)"
    )

  p3 <- plot_dat |>
    mutate(
      values = values / pop
    ) |>
    select(-pop) |>
    pivot_wider(names_from = citizen, values_from = values) |>
    janitor::clean_names() |>
    # filter(
    #   leg_stat == "Prosecuted person"
    # ) |>
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    ) |>
    mutate(
      p = foreign_country / reporting_country
    ) |>
    filter(
      leg_stat == "Suspected person"
    ) |>
    mutate(
      leg_stat = fct_recode(
        leg_stat,
        "Saksóttir" = "Prosecuted person",
        "Grunaðir" = "Suspected person"
      )
    ) |>
    ggplot(aes(time, p, data_id = geo)) +
    geom_line_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour),
      alpha = 0.3,
      col = litur_annad
    ) +
    geom_line_interactive(
      data = ~ filter(.x, colour != litur_annad),
      aes(group = land, colour = colour),
      linewidth = 1
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated()
    ) +
    scale_y_continuous(
      limits = c(0, 8),
      expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(6),
      labels = \(x) {
        out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
        if_else(
          out == "0x fleiri",
          "Jafnmargir",
          out
        )
      },
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    scale_hjust_manual(
      values = c(0.3, 0.4, 0.25, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 8)) +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Grunaðir (Þróun)"
    )

  p4 <- plot_dat |>
    mutate(
      values = values / pop
    ) |>
    select(-pop) |>
    pivot_wider(names_from = citizen, values_from = values) |>
    janitor::clean_names() |>
    # filter(
    #   leg_stat == "Prosecuted person"
    # ) |>
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    ) |>
    mutate(
      p = foreign_country / reporting_country
    ) |>
    filter(
      # year(time) < 2022,
      leg_stat == "Prosecuted person"
    ) |>
    mutate(
      leg_stat = fct_recode(
        leg_stat,
        "Saksóttir" = "Prosecuted person",
        "Grunaðir" = "Suspected person"
      )
    ) |>
    ggplot(aes(time, p, data_id = geo)) +
    geom_line_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour),
      alpha = 0.3,
      col = litur_annad
    ) +
    geom_line_interactive(
      data = ~ filter(.x, colour != litur_annad),
      aes(group = land, colour = colour),
      linewidth = 1
    ) +
    scale_x_date(
      breaks = breaks_width("2 year", offset = "1 year"),
      labels = label_date_short(),
      guide = guide_axis_truncated()
    ) +
    scale_y_continuous(
      limits = c(0, 8),
      expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(6),
      labels = \(x) {
        out <- number(x, suffix = "x fleiri", accuracy = 1, big.mark = ".", decimal.mark = ",")
        if_else(
          out == "0x fleiri",
          "Jafnmargir",
          out
        )
      },
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    scale_hjust_manual(
      values = c(0.3, 0.4, 0.25, 0.2)
    ) +
    coord_cartesian(ylim = c(0, 8)) +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Saksóttir (Þróun)"
    )


  p <- (p1 + p2) /
    (p3 + p4) +
    plot_annotation(
      title = "Samanburður á tíðni gruns um glæp og saksóknar meðal innflytjenda og innfæddra",
      subtitle = "Hve mikið fleiri eru grunaðir með erlent ríkisfang en grunaðir með innlent ríkisfang (á höfðatölu)?",
    )


  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.9 * 11,
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
