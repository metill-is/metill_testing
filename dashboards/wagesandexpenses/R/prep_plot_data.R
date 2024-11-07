#' @export
prep_plot_data <- function(d, ord_var, nm) {
  box::use(
    dplyr[filter, mutate, select],
    tidyr[pivot_wider],
    glue[glue],
    readr[write_csv],
    here[here],
    stringr[str_c],
    scales[percent, number]
  )

  plot_dat <- d |>
    filter(
      order_var == ord_var,
      name %in% nm
    ) |>
    mutate(
      value_perc = value / sum(value),
      .by = c(year, name)
    ) |>
    mutate(
      decile = factor(decile, levels = 10:1),
      data_id = paste0(year, "_", decile),
      value = value * 1e6,
      value_per_n = value / n,
      tooltip = glue(
        str_c(
          "<b>{year}</b><br>",
          "Tíundarhluti: {decile}<br>",
          "Hlutfall: {percent(value_perc, accuracy = 1, decimal.mark = ',', big.mark = '.')}<br>",
          "Magn: {number(value, scale = 1e-9, accuracy = 10, decimal.mark = ',', big.mark = '.', suffix = ' ma.kr')}<br>",
          "Magn á mann: {number(value_per_n, scale = 1e-6, accuracy = 0.1, decimal.mark = ',', big.mark = '.', suffix = ' m.kr')}"
        )
      )
    )

  out_name <- glue("{nm}_ordered_by_{ord_var}.csv")

  plot_dat |>
    select(
      year,
      decile,
      "Total" = value,
      "Percent" = value_perc,
      "Value per person" = value_per_n
    ) |>
    write_csv(here("dashboards", "wagesandexpenses", "data", out_name))

  return(plot_dat)
}
