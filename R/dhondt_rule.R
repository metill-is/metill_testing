library(tidyverse)
library(ggiraph)
library(metill)
library(patchwork)
library(here)
library(arrow)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

dhondt <- function(votes) {
  n_seats <- 63
  party_seats <- numeric(length(votes))
  temp_votes <- votes

  while (sum(party_seats) < n_seats) {
    which_max <- which.max(temp_votes)
    party_seats[which_max] <- party_seats[which_max] + 1
    temp_votes[which_max] <- votes[which_max] / (party_seats[which_max] + 1)
  }

  party_seats
}

theme_set(theme_metill())

colors <- tribble(
  ~flokkur, ~litur,
  "Sjálfstæðisflokkurinn", "#377eb8",
  "Framsóknarflokkurinn", "#41ab5d",
  "Samfylkingin", "#e41a1c",
  "Vinstri Græn", "#006d2c",
  "Viðreisn", "#f16913",
  "Píratar", "#6a51a3",
  "Miðflokkurinn", "#08306b",
  "Flokkur Fólksins", "#FBB829",
  "Sósíalistaflokkurinn", "#a50f15",
  "Annað", "grey50"
)



samsteypa_flokkar <- c(
  "xS" = "Samfylkingin",
  "xM" = "Miðflokkurinn",
  # "xD" = "Sjálfstæðisflokkurinn",
  # "xC" = "Viðreisn"
  "xF" = "Flokkur Fólksins"
)

d <- read_parquet(here("data", "y_rep_draws.parquet")) |>
  mutate(
    flokkur = if_else(
      flokkur %in% samsteypa_flokkar,
      "Samsteypa",
      "Annað"
    )
  ) |>
  summarise(
    value = sum(value),
    .by = c(.iteration, .chain, .draw, dags, flokkur)
  ) |>
  mutate(
    value = dhondt(value),
    .by = c(.iteration, .chain, .draw, dags)
  )

d |>
  filter(flokkur == "Samsteypa") |>
  summarise(
    p_majority = mean(value >= 32),
    .by = dags
  ) |>
  ggplot(aes(dags, p_majority)) +
  geom_line()


d |>
  distinct(dags, flokkur, mean, litur) |>
  mutate(
    seats = calculate_dhondt_rule(mean),
    .by = dags
  ) |>
  ggplot(aes(dags, seats, col = litur)) +
  geom_line() +
  scale_color_identity()


recent_dat <- d |>
  filter(dags == max(dags)) |>
  distinct(dags, flokkur, mean, q5, q95, litur) |>
  arrange(desc(flokkur))

votes <- recent_dat |> pull(mean)
names(votes) <- recent_dat |> pull(flokkur)

n_seats <- 63
party_seats <- numeric(length(votes))
names(party_seats) <- names(votes)
temp_votes <- votes


while (sum(party_seats) < n_seats) {
  which_max <- which.max(temp_votes)
  party_seats[which_max] <- party_seats[which_max] + 1
  temp_votes[which_max] <- votes[which_max] / (party_seats[which_max] + 1)
}

party_seats
tibble(
  flokkur = names(party_seats),
  seats = party_seats
) |>
  inner_join(colors) |>
  mutate(
    flokkur = fct_reorder(flokkur, seats),
    litur = fct_reorder(litur, seats)
  ) |>
  arrange(desc(seats)) |>
  mutate(
    next_flokkur = lead(flokkur),
    next_flokkur = coalesce(next_flokkur, flokkur),
    lag_seats = lag(seats, default = 0),
    x_start = cumsum(lag_seats),
    x_end = cumsum(seats)
  ) |>
  ggplot(aes(y = flokkur, col = litur)) +
  geom_segment(
    aes(yend = flokkur, x = x_start, xend = x_end),
    linewidth = 4
  ) +
  geom_segment(
    aes(yend = next_flokkur, x = x_end, xend = x_end),
    linewidth = 0.3,
    col = "grey50",
    lty = 2,
    alpha = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(0, 63, by = 3),
    expand = c(0, 0),
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = 0,
      trunc_upper = 63
    )
  ) +
  scale_y_discrete(
    guide = ggh4x::guide_axis_truncated(
      trunc_lower = ~ .x - 0.1,
      trunc_upper = ~ .x + 0.1
    )
  ) +
  scale_color_identity() +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(
  here("Figures", "dhondt_rule.png"),
  width = 8,
  height = 0.5 * 8,
  scale = 1.2
)
