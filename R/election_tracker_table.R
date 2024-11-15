make_election_tracker_table <- function() {
  library(tidyverse)
  library(gt)
  library(gtExtras)
  library(arrow)
  library(here)
  library(glue)

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

  day_suffix <- glue(
    str_c("{mday(today())}. {month(today(), label = TRUE, abbr = FALSE)}")
  )

  # read data
  polling_data <- read_csv(here("data", "polling_data.csv")) |>
    mutate(
      p_poll = n / sum(n),
      flokkur = str_to_sentence(flokkur),
      .by = c(date, fyrirtaeki)
    ) |>
    rename(dags = date)

  d <- read_parquet(here("data", "y_rep_draws_no_polling_bias.parquet")) |>
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
    )

  table <- d |>
    filter(dags == today()) |>
    select(flokkur, mean, q5, q95) |>
    arrange(desc(mean)) |>
    gt() |>
    cols_label(
      flokkur = "",
      mean = "Fylgismat",
      q5 = "Neðri",
      q95 = "Efri"
    ) |>
    tab_spanner(
      label = md("**95% Óvissubil**"),
      columns = c(q5, q95)
    ) |>
    cols_align(
      align = "left",
      columns = 1
    ) |>
    cols_align(
      align = "center",
      columns = -1
    ) |>
    fmt_percent(
      decimals = 0
    ) |>
    tab_header(
      title = glue("Fylgismat stjórnmálaflokka fyrir {day_suffix}"),
      subtitle = glue(
        str_c(
          "Síðasta könnun: {max(polling_data$dags)}"
        )
      )
    ) |>
    tab_source_note(
      md(
        str_c(
          "*Matið styðst við kannanir Félagsvísindastofnunar, ",
          "Fréttablaðs ásamt Stöð 2 og Vísi, ",
          "Gallup, Maskínu, ",
          "MMR og Prósents frá upphafi árs 2016 til dagsins í dag ",
          "ásamt niðurstöðum kosninga 2016, 2017 og 2021*"
        )
      )
    ) |>
    tab_footnote(
      "Unnið af Brynjólfi Gauta Guðrúnar Jónssyni, Agnari Frey Helgasyni, Hafsteini Einarssyni og Rafael Daniel Vias"
    ) |>
    tab_options(
      table.background.color = "#fdfcfc"
    )


  for (row in seq_len(nrow(colors))) {
    table <- table |>
      tab_style(
        style = cell_text(
          color = colors$litur[row],
          weight = 800
        ),
        locations = cells_body(
          rows = flokkur == colors$flokkur[row]
        )
      )
  }


  table
}
