#' @export
make_kosningabarattan_plot <- function() {
  library(tidyverse)
  library(ggiraph)
  library(metill)
  library(patchwork)
  library(here)
  library(arrow)
  Sys.setlocale("LC_ALL", "is_IS.UTF-8")

  theme_set(theme_metill(type = "blog"))

  caption <- str_c(
    "Matið styðst við kannanir Félagsvísindastofnunar, ",
    "Fréttablaðs ásamt Stöð 2 og Vísi, ",
    "Gallup, Maskínu, ",
    "MMR og Prósents frá upphafi árs 2016 til dagsins í dag ",
    "ásamt niðurstöðum kosninga 2016, 2017 og 2021\n",
    "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias"
  )

  colors <- tribble(
    ~flokkur, ~litur,
    "Sjálfstæðisflokkurinn", "#377eb8",
    "Framsóknarflokkurinn", "#41ab5d",
    "Samfylkingin", "#e41a1c",
    "Vinstri græn", "#006d2c",
    "Viðreisn", "#f16913",
    "Píratar", "#6a51a3",
    "Miðflokkurinn", "#08306b",
    "Flokkur fólksins", "#FBB829",
    "Sósíalistaflokkurinn", "#a50f15",
    "Annað", "grey50"
  )

  point_shapes <- c(
    "Gallup" = 21,
    "Maskína" = 22,
    "Prósent" = 23,
    "Félagsvísindastofnun" = 24,
    " Kosningar" = 4
  )

  # read data
  polling_data <- read_csv(here("data", "polling_data.csv")) |>
    mutate(
      p_poll = n / sum(n),
      flokkur = str_to_sentence(flokkur),
      .by = c(date, fyrirtaeki)
    ) |>
    rename(dags = date)



  d <- read_parquet(here("data", "y_rep_draws.parquet")) |>
    summarise(
      mean = mean(value),
      q5 = quantile(value, 0.05),
      q95 = quantile(value, 0.95),
      .by = c(dags, flokkur)
    ) |>
    mutate(
      flokkur = str_to_sentence(flokkur)
    ) |>
    inner_join(
      colors
    ) |>
    inner_join(
      polling_data
    )


  coverage_data <- read_parquet(
    here("data", "y_rep_draws.parquet")
  ) |>
    reframe(
      coverage = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
      lower = quantile(value, 0.5 - coverage / 2),
      upper = quantile(value, 0.5 + coverage / 2),
      .by = c(dags, flokkur)
    ) |>
    mutate(
      flokkur = str_to_sentence(flokkur)
    ) |>
    inner_join(
      colors
    ) |>
    filter(
      dags <= max(polling_data$dags)
    )

  p <- d |>
    ggplot(aes(dags, mean, colour = litur, data_id = flokkur)) +
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
    geom_line_interactive(
      data = ~ filter(.x, dags <= max(polling_data$dags)),
      linewidth = 1
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
      limits = c(0, 0.3),
      guide = ggh4x::guide_axis_truncated(),
      labels = label_percent()
    ) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_alpha_continuous(
      range = c(0, 0.1)
    ) +
    scale_shape_manual(
      values = point_shapes,
      name = "Könnunarfyrirtæki:",
      na.translate = FALSE
    ) +
    coord_cartesian(
      xlim = clock::date_build(2024, c(8, 11), c(1, 30))
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Kapphlaupið"
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
