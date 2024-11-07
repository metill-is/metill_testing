library(tidyverse)
library(eurostat)
library(arrow)

pop <- get_eurostat(
  "demo_pjan",
  filters = list(
    sex = "T"
  )
)

d <- pop |>
  label_eurostat()


d <- d |>
  janitor::remove_constant()


d <- d |>
  filter(
    !age %in% c("Total", "Unknown", "Open-ended age class")
  ) |>
  mutate(
    age = if_else(
      age == "Less than 1 year",
      "0",
      age
    ) |>
      parse_number()
  ) |>
  filter(
    age > 18
  ) |>
  summarise(
    population_adult = sum(values, na.rm = T),
    .by = c(geo, time)
  ) |>
  mutate(year = year(time)) |>
  select(country = geo, year, population_adult)

library(hagstofa)

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/1_yfirlit/Yfirlit_mannfjolda/MAN00101.px"


d_hg <- hg_data(url) |>
  filter(
    parse_number(Aldur) > 18,
    Kyn == "Alls"
  ) |>
  collect() |>
  janitor::clean_names() |>
  mutate_at(
    vars(aldur, ar),
    parse_number
  ) |>
  rename(n = 4) |>
  janitor::remove_constant() |>
  drop_na() |>
  summarise(
    population_adult = sum(n),
    .by = ar
  ) |>
  rename(year = ar) |>
  mutate(
    country = "Iceland"
  )

d |>
  filter(country != "Iceland") |>
  bind_rows(d_hg |> filter(ar >= 2007)) |>
  write_csv("dashboards/properties/data/pop.csv")

d_hg |>
  write_csv("dashboards/properties/data/pop_adult_iceland.csv")
