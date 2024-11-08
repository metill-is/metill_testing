#' @export
make_spa_tafla <- function(coverage_data, colors) {
  table <- coverage_data |>
    filter(
      dags == max(dags),
      coverage == 0.9
    ) |>
    select(flokkur, mean, lower, upper) |>
    arrange(desc(mean)) |>
    gt(process_md = TRUE) |>
    fmt_percent(
      columns = mean:upper,
      decimals = 0
    ) |>
    cols_label(
      flokkur = "",
      mean = "Spá",
      lower = "Neðri",
      upper = "Efri"
    ) |>
    tab_spanner(
      label = md("90% Óvissubil"),
      columns = lower:upper
    ) |>
    tab_options(
      table.background.color = "transparent",
      column_labels.hidden = FALSE,
      table.border.top.style = "0px",
      table.border.bottom.style = "0px",
      table.font.size = "16px",
      # table_body.hlines.style = "0px",
      table_body.border.bottom.style = "0px",
      table_body.border.top.style = "0px"
    ) |>
    opt_table_font(
      font = google_font("Lato"),
      weight = "bold"
    )

  for (row in seq_len(nrow(colors))) {
    table <- table |>
      tab_style(
        style = cell_text(
          color = colors$litur[row],
          weight = "bold",
          font = google_font("Lato")
        ),
        locations = cells_body(
          rows = flokkur == str_to_sentence(colors$flokkur[row])
        )
      )
  }


  table
}
