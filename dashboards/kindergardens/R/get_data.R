library(tidyverse)
library(pxweb)
library(here)

d <- pxweb_get(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/skolamal/1_leikskolastig/0_lsNemendur/SKO01000.px",
  query = list(
    "Leikskóli" = c("*"),
    "Ár" = c("*"),
    "Kyn/aldur" = c("First year", "1 years", "2 years", "3 years", "4 years", "5 years")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(n = born_og_leikskolar_i_desember_1998_2023, aldur = kyn_aldur) |>
  filter(leikskoli != "Alls") |>
  separate(leikskoli,
    into = c("sveitarfelag", "leikskoli"),
    sep = " - "
  ) |>
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Reykjav") ~ "Reykjavíkurborg",
      sveitarfelag == "Kópavogur" ~ "Kópavogsbær",
      sveitarfelag == "Seltjarnarnes" ~ "Seltjarnarnesbær",
      sveitarfelag == "Hafnarfjörður" ~ "Hafnarfjarðarkaupstaður",
      sveitarfelag == "Grindavík" ~ "Grindavíkurbær",
      sveitarfelag == "Akranes" ~ "Akraneskaupstaður",
      sveitarfelag == "Stykkishólmur" ~ "Stykkishólmsbær",
      sveitarfelag == "Akureyri" ~ "Akureyrarbær",
      sveitarfelag == "Hveragerði" ~ "Hveragerðisbær",
      sveitarfelag == "Vestmannaeyjar" ~ "Vestmannaeyjabær",
      sveitarfelag %in% c("Garðabær", "Sveitarfélagið Álftanes") ~ "Garðabær",
      TRUE ~ sveitarfelag
    )
  ) |>
  mutate(
    ar = parse_number(ar),
    aldur = as_factor(aldur) |> fct_relevel("Á fyrsta ári")
  ) |>
  count(sveitarfelag, ar, aldur, wt = n)

mannfjoldi <- pxweb_get(
  url = "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px",
  query = list(
    "Sveitarfélag" = c("*"),
    "Aldur" = as.character(0:5),
    "Ár" = c("*"),
    "Kyn" = c("0")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(mannfjoldi = 5) |>
  mutate(ar = parse_number(ar)) |>
  filter(sveitarfelag != "Alls") |>
  select(sveitarfelag, aldur, ar, mannfjoldi) |>
  mutate(
    aldur = str_replace(aldur, "Á 1. ári", "Á fyrsta ári") |> str_trim() |> as_factor(),
    ar = ar
  )


d <- d |>
  inner_join(
    mannfjoldi,
    by = c("sveitarfelag", "aldur", "ar")
  ) |>
  rename(
    n_leik = n,
    n_heild = mannfjoldi
  ) |>
  filter(
    sveitarfelag != "Grindavíkurbær"
  )


d |>
  write_csv(here("dashboards/kindergardens/data/kindergardens.csv"))
