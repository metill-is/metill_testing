library(eurostat)
library(tidyverse)
library(metill)
library(ggh4x)
library(geomtextpath)
library(patchwork)
theme_set(theme_metill())

lw_iceland <- 1.5
lw_other <- 0.2

d_fj <- get_eurostat(
  "lfsq_ugacob",
  filters = list(
    age = "Y20-64",
    c_birth = c("FOR", "NAT", "TOTAL"),
    sex = "T"
  )
)

d_hlutf <- get_eurostat(
  "lfsq_urgacob",
  filters = list(
    age = "Y20-64",
    c_birth = c("FOR", "NAT", "TOTAL"),
    sex = "T"
  )
)

d_atv <- get_eurostat(
  "lfsq_egacob",
  filters = list(
    age = "Y20-64",
    c_birth = c("FOR", "NAT", "TOTAL"),
    sex = "T"
  )
)


d_fj <- d_fj |>
  label_eurostat() |>
  janitor::remove_constant() |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  pivot_wider(names_from = c_birth, values_from = values) |>
  janitor::clean_names() |>
  mutate(
    p = foreign_country / total
  ) |>
  drop_na(p) |>
  mutate(
    p_trend = slider::slide_dbl(p, mean, .before = 3),
    .by = geo
  )

d_atv <- d_atv |>
  label_eurostat() |>
  janitor::remove_constant() |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  pivot_wider(names_from = c_birth, values_from = values) |>
  janitor::clean_names() |>
  mutate(
    p = foreign_country / total
  ) |>
  drop_na(p) |>
  mutate(
    p_trend = slider::slide_dbl(p, mean, .before = 3),
    .by = geo
  )

d_hlutf <- d_hlutf |>
  label_eurostat() |>
  janitor::remove_constant() |>
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |>
  pivot_wider(names_from = c_birth, values_from = values) |>
  janitor::clean_names() |>
  mutate(
    p = foreign_country / reporting_country - 1
  ) |>
  select(land, time, foreign_country, reporting_country, p) |>
  pivot_longer(-c(1:2)) |>
  mutate(
    value = zoo::na.approx(value, na.rm = FALSE),
    value_trend = slider::slide_dbl(value, mean, .before = 3),
    .by = c(land, name)
  ) |>
  drop_na() |>
  mutate(
    value_trend = if_else(
      (land == "Ísland") & (time >= clock::date_build(2023, 4)) & (name == "foreign_country"),
      value,
      value_trend
    )
  )


d_hlutf |>
  write_csv(
    here::here("dashboards", "immigration", "data", "unemployment.csv")
  )
