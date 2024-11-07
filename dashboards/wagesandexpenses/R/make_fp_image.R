box::use(
  arrow[read_parquet],
  dplyr[case_when, filter, lag, lead, if_else, left_join, mutate, pull, select, summarise],
  ggh4x[guide_axis_truncated],
  ggiraph[
    geom_col_interactive,
    geom_line_interactive,
    geom_point_interactive,
    geom_text_interactive,
    girafe,
    opts_hover,
    opts_hover_inv
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
  patchwork[plot_annotation, wrap_plots],
  scales[breaks_extended, breaks_pretty, label_number, label_percent, number, percent],
  stringr[str_c],
  tidyr[pivot_wider],
  visitalaneysluverds[vnv_convert]
)

box::use(
  dashboards / wagesandexpenses / R / plot_theme[theme_metill],
  dashboards / wagesandexpenses / R / prep_plot_data[prep_plot_data]
)

theme_set(theme_metill())

d <- here("dashboards", "wagesandexpenses", "data", "deciles.parquet") |>
  read_parquet()

d$value <- vnv_convert(d$value, d$year)

plot_dat <- d |>
  filter(
    order_var == "Ráðstöfunartekjur",
    name %in% c(
      "Vaxtagjöld v/íbúðalána",
      "Ráðstöfunartekjur (Tekjur - Skattar)"
    )
  ) |>
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
      decile == 9,
      str_c("Ráðstöfunartekjur:\n", interval),
      interval
    )
  )

p <- plot_dat |>
  left_join(intervals, by = "decile") |>
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
  geom_text_interactive(
    data = ~ filter(., year == 2023),
    aes(
      label = interval,
      tooltip = tooltip,
      data_id = data_id,
      col = decile,
      y = value + 0.003 * (decile == 9) - 0.0005 * (decile == 1) + 0.0005 * (decile == 10)
    ),
    size = 5,
    nudge_x = 0.3,
    hjust = 0
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
    limits = c(0, 0.12),
    labels = label_percent(
      accuracy = 1,
      decimal.mark = ",",
      big.mark = ".",
      suffix = "%"
    ),
    guide = guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 0.12
    )
  ) +
  scale_color_brewer(
    palette = "RdBu",
    direction = -1
  ) +
  coord_cartesian(
    clip = "on",
    ylim = c(0, 0.12),
    xlim = c(1997, 2030)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Vaxtagjöld v/íbúðalána sem hlutfall af ráðstöfunartekjum",
    subtitle = "Eftir lækkun frá 2013 til 2021 hafa vaxtagjöld aukist sem hlutfall af ráðstöfunartekjum síðustu tvö ár",
    col = "Tíundarhluti"
  ) +
  theme(
    legend.position = "none"
  )
p

ggsave(
  here("dashboards", "wagesandexpenses", "img", "image.png"),
  plot = p +
    theme(
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      legend.background = element_rect(fill = "transparent")
    ),
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)

ggsave(
  here("dashboards", "wagesandexpenses", "img", "image_with_bg.png"),
  plot = p,
  width = 8,
  height = 0.621 * 8,
  scale = 1.5
)
