library(tidyverse)
library(hagstofa)

cache_dir <- here::here("dashboards", "immigration", "data")

d_is <- read_csv(
  here::here(cache_dir, "origin.csv")
)

d_en <- read_csv(
  here::here(cache_dir, "origin_en.csv")
)

d <- d_is |>
  mutate(
    country = d_en$citizenship
  )

d |>
  write_csv(
    here::here(cache_dir, "origin_combined.csv")
  )
