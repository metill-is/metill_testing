library(tidyverse)
library(hagstofa)
library(here)

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/visitolur/1_vnv/2_undirvisitolur/VIS01301.px"

d <- hg_data(url) |>
  filter(
    Liður %in% c(
      "Áhrif á vísitölu, %",
      "Mánaðarbreyting, %",
      "Vægi, %"
    )
  ) |>
  collect()

d_combined <- d |>
  janitor::clean_names() |>
  mutate(
    date = ym(manudur),
    visitala_neysluverds = visitala_neysluverds / 100
  ) |>
  select(date, undirvisitala, lidur, value = visitala_neysluverds) |>
  mutate(
    flokkur_1 = ifelse(
      str_detect(undirvisitala, "^[0-9]{2} "),
      undirvisitala,
      NA
    ),
    flokkur_2 = ifelse(
      str_detect(undirvisitala, "^[0-9]{3} "),
      undirvisitala,
      NA
    ),
    flokkur_3 = ifelse(
      str_detect(undirvisitala, "^[0-9]{4} "),
      undirvisitala,
      NA
    )
  ) |>
  group_by(date, lidur) |>
  fill(flokkur_1, .direction = "down") |>
  group_by(date, lidur, flokkur_1) |>
  fill(flokkur_2, .direction = "down") |>
  group_by(date, lidur, flokkur_1, flokkur_2) |>
  fill(flokkur_3, .direction = "down") |>
  ungroup() |>
  mutate(
    flokkur_2 = case_when(
      all(is.na(flokkur_2)) ~ flokkur_1,
      is.na(flokkur_2) ~ NA_character_,
      TRUE ~ flokkur_2
    ),
    .by = flokkur_1
  ) |>
  mutate(
    flokkur_3 = case_when(
      all(is.na(flokkur_3)) ~ flokkur_2,
      is.na(flokkur_3) ~ NA_character_,
      TRUE ~ flokkur_3
    ),
    .by = flokkur_2
  ) |>
  mutate(
    flokkur_1 = ifelse(
      undirvisitala == "Vísitala neysluverðs",
      undirvisitala,
      flokkur_1
    ),
    flokkur_2 = ifelse(
      undirvisitala == "Vísitala neysluverðs",
      undirvisitala,
      flokkur_2
    ),
    flokkur_3 = ifelse(
      undirvisitala == "Vísitala neysluverðs",
      undirvisitala,
      flokkur_3
    )
  ) |>
  mutate_at(
    vars(flokkur_2, flokkur_3),
    coalesce, "Samtals"
  )

d3 <- d_combined |>
  filter(
    flokkur_3 != "Samtals",
    undirvisitala != "Vísitala neysluverðs"
  )

d3 |>
  write_csv(
    here("dashboards/inflation/data/undirflokkar3.csv")
  )

d2 <- d_combined |>
  filter(
    flokkur_2 != "Samtals",
    flokkur_3 == "Samtals",
    undirvisitala != "Vísitala neysluverðs"
  )

d2 |>
  write_csv(
    here("dashboards/inflation/data/undirflokkar2.csv")
  )

d1 <- d_combined |>
  filter(
    flokkur_2 == "Samtals",
    flokkur_3 == "Samtals",
    undirvisitala != "Vísitala neysluverðs"
  )

d1 |>
  write_csv(
    here("dashboards/inflation/data/undirflokkar1.csv")
  )

d_tot <- d_combined |>
  filter(
    undirvisitala == "Vísitala neysluverðs"
  )

d_tot |>
  write_csv(
    here("dashboards/inflation/data/visitala_neysluverds.csv")
  )
