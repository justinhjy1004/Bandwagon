library(tidyverse)
library(stargazer)

by_year <- read_csv("nber_by_year.csv")

by_year <- by_year |> 
  drop_na() |>
  filter(year != 2015) # NAs for most Patents

nbers <- unique(by_year$nber)

non_category <- by_year |> 
  mutate(is_category = nber == nbers[1]) |>
  group_by(is_category, year) |>
  summarise(count = sum(count)) |>
  ungroup() |>
  group_by(is_category) |>
  mutate(
    lag_count = lag(count),
    growth = (count - lag_count)/ lag_count,
    nber = nbers[1])

for (x in nbers[2:length(nbers)]) {
  non <- by_year |> 
    ungroup() |>
    mutate(is_category = nber == x) |>
    group_by(is_category, year) |>
    summarise(count = sum(count)) |>
    ungroup() |>
    group_by(is_category) |>
    mutate(
      lag_count = lag(count),
      growth = (count - lag_count)/ lag_count,
      nber = x)
  non_category <- rbind(non_category, non)
}

#=======================================================
# Autoregression
#=======================================================

time_growth <- non_category |>
  spread(key = is_category, value = growth) 

in_cat <- time_growth |>
  select(nber, `TRUE`,year) |>
  drop_na()

out_cat <- time_growth |>
  select(nber, `FALSE`, year) |>
  drop_na()

cat_diff <- in_cat |>
  left_join(out_cat, by = c("nber", "year")) |>
  group_by("nber") |>
  mutate(
    diff = `TRUE` - `FALSE`,
    diff.l1 = lag(diff),
    diff.l2 = lag(diff, n=2),
    diff.l3 = lag(diff, n=3))

ar1 <- lm(diff ~ diff.l1, cat_diff)
ar2 <- lm(diff ~ diff.l1 + diff.l2, cat_diff)
ar3 <- lm(diff ~ diff.l1 + diff.l2 + diff.l3, cat_diff)

stargazer(ar1, ar2, ar3)
