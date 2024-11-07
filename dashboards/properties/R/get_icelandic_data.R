library(tidyverse)
library(arrow)
library(metill)
library(rjson)

d_json <- fromJSON(file = "https://talnaefni.fasteignaskra.is/talnaefni/v1/staerdibudasveitarfelog")

d_fasteignir <- tibble(
  data = list(d_json)
) |>
  unnest_wider(data) |>
  unnest_longer(sveitarfélög) |>
  unnest_wider(sveitarfélög) |>
  unnest_longer(c(-name)) |>
  mutate_at(vars(-name), parse_number) |>
  mutate(
    fjolbyli_fjoldi = coalesce(fjolbyli_fjoldi, 0),
    serbyli_fjoldi = coalesce(serbyli_fjoldi, 0),
    fjoldi_nyjar = fjolbyli_fjoldi + serbyli_fjoldi
  ) |>
  mutate(
    fjoldi = cumsum(fjoldi_nyjar),
    .by = name
  ) |>
  select(
    ar = date,
    sveitarfelag = name,
    fjoldi_nyjar,
    fjoldi
  )

mannfjoldi <- mtl_mannfjoldi_svf() |>
  collect() |>
  mutate(
    vinnualdur = ifelse((aldur >= 20) & (aldur <= 64), 1, 0),
    heild = 1,
    fullordin = ifelse(aldur >= 20, 1, 0)
  ) |>
  group_by(sveitarfelag, ar) |>
  summarise(
    mannfjoldi_vinnualdur = sum(mannfjoldi * vinnualdur),
    mannfjoldi_fullordin = sum(mannfjoldi * fullordin),
    mannfjoldi = sum(mannfjoldi * heild)
  )


d <- d |>
  left_join(
    mannfjoldi,
    by = c("ar", "sveitarfelag")
  ) |>
  group_by(sveitarfelag) |>
  fill(mannfjoldi_vinnualdur, mannfjoldi_fullordin, mannfjoldi, .direction = "down") |>
  ungroup() |>
  drop_na()

d |>
  summarise(
    new_dwellings = sum(fjoldi),
    dwellings = sum(cum_fjoldi),
    population = sum(mannfjoldi),
    .by = ar
  ) |>
  rename(year = ar) |>
  mutate(
    country = "Iceland",
    new_by_pop = new_dwellings / population * 1000,
    rolling = slider::slide_index_dbl(new_by_pop, year, sum, .before = 9),
    country = "Iceland"
  ) |>
  write_parquet("greinar/fasteignafjoldi/data/data_iceland.parquet")



d_fasteignir |>
  summarise(
    fjoldi_nyjar = sum(fjoldi_nyjar, na.rm = TRUE),
    fjoldi_kop = fjoldi[sveitarfelag == "Kópavogsbær"],
    fjoldi_gbr = fjoldi[sveitarfelag == "Garðabær"],
    fjoldi_hfj = fjoldi[sveitarfelag == "Hafnarfjarðarkaupstaður"],
    fjoldi_sel = fjoldi[sveitarfelag == "Seltjarnarnesbær"],
    .by = ar
  ) |>
  mutate(
    nyjar_ar = slider::slide_dbl(fjoldi_nyjar, sum, .before = 5, .complete = TRUE),
    fjoldi_annad = fjoldi_gbr + fjoldi_kop
  ) |>
  drop_na() |>
  ggplot(aes(ar, nyjar_ar)) +
  geom_line(aes(col = "Nýjar fasteignir á landsvísu síðustu 7 ár")) +
  geom_line(aes(ar, fjoldi_annad, col = "Samtals fasteignir í Gbr og Kóp"))
