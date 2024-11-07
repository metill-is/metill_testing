library(rjson)
library(tidyverse)
library(hagstofa)
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
  ) |>
  filter(sveitarfelag != "Allt landið")


url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/1_yfirlit/Yfirlit_mannfjolda/MAN00101.px"


d <- hg_data(url)

d_pop <- d |>
  filter(
    Kyn == "Alls",
    Aldur == "Alls"
  ) |>
  collect()


d <- d_pop |>
  janitor::clean_names() |>
  rename(n = 4) |>
  mutate(
    ar = parse_number(ar)
  ) |>
  drop_na() |>
  summarise(
    pop = sum(n),
    .by = c(ar)
  )


d_combined <- d_fasteignir |>
  summarise(
    fasteignir = sum(fjoldi_nyjar, na.rm = TRUE),
    .by = ar
  ) |>
  left_join(
    d,
    by = c("ar")
  )

d_combined |>
  write_csv(here::here("dashboards/properties/data/fasteignir_pop.csv"))


d_combined |>
  mutate(
    total_fasteignir = cumsum(fasteignir),
  ) |>
  filter(ar >= 2007) |>
  ggplot(aes(ar, pop / total_fasteignir)) +
  geom_line()
