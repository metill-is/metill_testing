library(tidyverse)
library(hagstofa)
library(pxweb)
cache_dir <- here::here("dashboards", "immigration", "data")

url <- "https://px.hagstofa.is:443/pxen/api/v1/en/Ibuar/mannfjoldi/3_bakgrunnur/Rikisfang/MAN04103.px"

px_variables <- pxweb_get(
  url = url
)


px_variables$variables |> str()

px_data <- pxweb_get(
  url = url,
  query = list(
    "Aldur" = c("1"),
    "Kyn" = c("1"),
    "Ár" = px_variables$variables[[3]]$values,
    "Ríkisfang" = px_variables$variables[[1]]$values
  )
)

d <- px_data |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  janitor::remove_constant() |>
  rename(n = 3) |>
  filter(
    citizenship != "Total"
  )

d |>
  write_csv(
    here::here(cache_dir, "origin_en.csv")
  )
