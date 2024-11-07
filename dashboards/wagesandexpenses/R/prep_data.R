library(arrow)
library(tidyverse)
library(pxweb)
library(metill)
library(here)

base_url <- paste("https://px.hagstofa.is:443/pxis/api/v1/is",
  "Samfelag/lifskjor/5_skuldastada_heimili/1_skuldir_eignir/",
  sep = "/"
)

urls <- list(
  property = paste0(base_url, "THJ09004.px"),
  debt = paste0(base_url, "THJ09003.px"),
  income = paste0(base_url, "THJ09002.px"),
  equity = paste0(base_url, "THJ09005.px"),
  revenue = paste0(base_url, "THJ09006.px")
)


d_property <- pxweb_get(
  url = urls$property,
  query = list(
    "Ár" = c("*"),
    "Tíundarhluti" = c("*"),
    "Eignir, skuldir, tekjur og gjöld" = c("*")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(name = 3, value = 4) |>
  mutate(ar = parse_number(ar)) |>
  mutate(tiundarbreyta = "Eignir") |>
  pivot_wider() |>
  rename(fjoldi = "Fjöldi í hóp") |>
  pivot_longer(
    c(-ar, -tiundarhluti, -tiundarbreyta, -fjoldi)
  )

d_debt <- pxweb_get(
  url = urls$debt,
  query = list(
    "Ár" = c("*"),
    "Tíundarhluti" = c("*"),
    "Eignir, skuldir, tekjur og gjöld" = c("*")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(name = 3, value = 4) |>
  mutate(ar = parse_number(ar)) |>
  mutate(tiundarbreyta = "Skuldir") |>
  pivot_wider() |>
  rename(fjoldi = "Fjöldi í hóp") |>
  pivot_longer(
    c(-ar, -tiundarhluti, -tiundarbreyta, -fjoldi)
  )


d_income <- pxweb_get(
  url = urls$income,
  query = list(
    "Ár" = c("*"),
    "Tíundarhluti" = c("*"),
    "Eignir, skuldir, tekjur og gjöld" = c("*")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(name = 3, value = 4) |>
  mutate(ar = parse_number(ar)) |>
  mutate(tiundarbreyta = "Heildartekjur") |>
  pivot_wider() |>
  mutate(
    `Skattar alls` = `Tekjur alls` - `Ráðstöfunartekjur (tekjur -skattar alls)`
  ) |>
  pivot_longer(c(-ar, -tiundarhluti, -tiundarbreyta)) |>
  pivot_wider() |>
  rename(fjoldi = "Fjöldi í hóp") |>
  pivot_longer(
    c(-ar, -tiundarhluti, -tiundarbreyta, -fjoldi)
  )

d_equity <- pxweb_get(
  url = urls$equity,
  query = list(
    "Ár" = c("*"),
    "Tíundarhluti" = c("*"),
    "Eignir, skuldir, tekjur og gjöld" = c("*")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(name = 3, value = 4) |>
  mutate(ar = parse_number(ar)) |>
  mutate(tiundarbreyta = "Eigið fé") |>
  pivot_wider() |>
  rename(fjoldi = "Fjöldi í hóp") |>
  pivot_longer(
    c(-ar, -tiundarhluti, -tiundarbreyta, -fjoldi)
  )

d_revenue <- pxweb_get(
  url = urls$revenue,
  query = list(
    "Ár" = c("*"),
    "Tíundarhluti" = c("*"),
    "Eignir, skuldir, tekjur og gjöld" = c("*")
  ),
  verbose = FALSE
) |>
  as.data.frame() |>
  as_tibble() |>
  janitor::clean_names() |>
  rename(name = 3, value = 4) |>
  mutate(ar = parse_number(ar)) |>
  mutate(tiundarbreyta = "Ráðstöfunartekjur") |>
  pivot_wider() |>
  rename(fjoldi = "Fjöldi í hóp") |>
  pivot_longer(
    c(-ar, -tiundarhluti, -tiundarbreyta, -fjoldi)
  )



d <- bind_rows(
  d_property,
  d_equity,
  d_debt,
  d_income,
  d_revenue
) |>
  filter(str_detect(tiundarhluti, "[0-9]")) |>
  mutate(tiundarhluti = parse_number(tiundarhluti)) |>
  select(
    year = ar,
    order_var = tiundarbreyta,
    decile = tiundarhluti,
    name,
    value,
    n = fjoldi
  ) |>
  filter(
    !str_detect(name, "vaxtagjöld$"),
    !str_detect(name, "-[ ]*[Vv]axtagjöld"),
    !str_detect(name, "[Bb]arnabæt")
  ) |>
  filter(name != "Skattar") |>
  mutate(
    name = case_when(
      str_detect(name, "Ráðstöfunartekjur") ~ "Ráðstöfunartekjur (Tekjur - Skattar)", # nolint
      str_detect(name, "Eigið fé alls") ~ "Eigið fé alls (Eignir - Skuldir)",
      str_detect(name, "Skattar alls|^Skattar$") ~ "Skattar alls",
      TRUE ~ name
    )
  )

d |>
  write_parquet(here("dashboards", "wagesandexpenses", "data", "deciles.parquet"))
