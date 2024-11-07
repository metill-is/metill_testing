library(tidyverse)
library(arrow)
library(metill)
library(rjson)
library(gt)
library(ggh4x)
library(ggtext)

theme_set(theme_metill())


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
  ) |>
  filter(
    !sveitarfelag %in% c("Allt landið")
  )

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
    nyjar_rolling = slider::slide_dbl(fjoldi_nyjar, sum, .before = 6, .complete = TRUE),
    min_ar = slider::slide_dbl(ar, min, .before = 6, .complete = TRUE),
    max_ar = slider::slide_dbl(ar, max, .before = 6, .complete = TRUE),
    heildarfjoldi_kopgbr = fjoldi_gbr + fjoldi_kop
  ) |>
  filter(ar >= 2000) |>
  select(ar, nyjar_rolling, heildarfjoldi_kopgbr) |>
  gt() |>
  tab_header(
    title = "Fjöldi nýja fasteigna á landsvísu síðustu 7 ár",
    subtitle = "og samtals fasteignir í Garðabær og Kópavogi"
  ) |>
  cols_label(
    ar = "Ár",
    nyjar_rolling = md("**Byggðar fasteignir**<br>*síðustu 7 ár*"),
    heildarfjoldi_kopgbr = md("**Samtals fasteignir**<br>*Gbr og Kóp*")
  ) |>
  fmt_number(
    columns = c(nyjar_rolling, heildarfjoldi_kopgbr),
    decimals = 0
  ) |>
  cols_align(
    align = "center",
    columns = -1
  )



p <- d_fasteignir |>
  summarise(
    fjoldi_nyjar = sum(fjoldi_nyjar, na.rm = TRUE),
    fjoldi_kop = fjoldi[sveitarfelag == "Kópavogsbær"],
    fjoldi_gbr = fjoldi[sveitarfelag == "Garðabær"],
    fjoldi_hfj = fjoldi[sveitarfelag == "Hafnarfjarðarkaupstaður"],
    fjoldi_sel = fjoldi[sveitarfelag == "Seltjarnarnesbær"],
    .by = ar
  ) |>
  mutate(
    nyjar_rolling = slider::slide_dbl(fjoldi_nyjar, sum, .before = 6, .complete = TRUE),
    min_ar = slider::slide_dbl(ar, min, .before = 6, .complete = TRUE),
    max_ar = slider::slide_dbl(ar, max, .before = 6, .complete = TRUE),
    heildarfjoldi_kopgbr = fjoldi_gbr + fjoldi_kop
  ) |>
  ggplot(aes(ar, nyjar_rolling)) +
  geom_line(
    aes(
      col = "Nýjar fasteignir á landsvísu<br>*síðustu 7 ár*"
    ),
    linewidth = 1
  ) +
  geom_line(
    aes(
      y = heildarfjoldi_kopgbr,
      col = "Samtals fasteignir<br>*Gbr og Kóp*"
    ),
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = c(seq(1900, 2010, 10), 2024),
    guide = guide_axis_truncated(trunc_lower = 1900, trunc_upper = 2024)
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(trunc_lower = 0, trunc_upper = 25000),
    limits = c(0, 25000)
  ) +
  scale_colour_brewer(
    palette = "Set1",
    guide = guide_legend(
      override.aes = list(linewidth = 2)
    )
  ) +
  theme(
    legend.position = c(0.5, 0.95),
    legend.text = element_textbox(),
    legend.direction = "horizontal"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = str_c(
      "Nýjar fasteignir á landsvísu og samtals fasteignir í Garðabæ og Kópavogi"
    )
  )

p

ggsave(
  "fjoldi_gbr_kop_compare.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.3
)
