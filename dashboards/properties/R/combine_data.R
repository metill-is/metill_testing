d <- read_csv("dashboards/properties/data/fasteignir_pop.csv")
d_gleeson <- read_csv("dashboards/properties/data/gleeson.csv")


d_gleeson

d_iceland <- d |>
  rename(
    year = ar,
    new_dwellings = fasteignir,
    population = pop
  ) |>
  mutate(
    dwellings = cumsum(new_dwellings),
    country = "Iceland",
    new_by_pop = new_dwellings / population * 1000,
    rolling = slider::slide_index_dbl(new_by_pop, year, sum, .before = 9)
  )

d_iceland |>
  filter(year >= 2007) |>
  ggplot(aes(year, population / dwellings)) +
  geom_line()


bind_rows(d_iceland, d_gleeson) |>
  inner_join(
    read_csv("dashboards/properties/data/pop.csv")
  ) |>
  mutate(
    new_by_adult = new_dwellings / population_adult * 1000,
    rolling_adult = slider::slide_index_dbl(new_by_adult, year, sum, .before = 9),
    .by = country
  ) |>
  filter(
    between(year, 2007, 2023)
  ) |>
  inner_join(
    metill::country_names()
  ) |>
  select(-country) |>
  rename(country = land) |>
  write_csv("dashboards/properties/data/data_combined.csv")
