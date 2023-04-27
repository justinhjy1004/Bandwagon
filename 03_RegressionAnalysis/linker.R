library(tidyverse)
library(lubridate)

#==================================================================
# Get the Difference between Growth Rates
# in NBER categories across the years
# Considering IN and OUT Categories
#==================================================================

by_year <- read_csv("./nber_by_year.csv")

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

#==============================================================
# Get Patents and Match by NBER category
# And the Difference Between Growth Rates
#==============================================================

args <- commandArgs(trailingOnly = TRUE)

file_name <- args[1]

print(file_name)

raw_patents <- read.csv(file_name, header = FALSE)

colnames(raw_patents) <- c("assignee_id",   "patent_id", "patent_date", "nber",         
                "num_citation",  "nber_category")

patents <- raw_patents |>
  mutate(
    nber = nber_category,
    year = year(patent_date)
  ) |>
  select(-nber_category) |> 
  left_join(non_category, by = c("nber")) |>
  mutate(
    year_diff = year.x - year.y,
    growth = 1+growth
  ) |>
  filter(year_diff > 0, year_diff <= 4) |>
  group_by(
    assignee_id, patent_id, patent_date, nber, 
    num_citation, is_category
  ) |>
  summarise(
    growth = prod(growth, na.rm = TRUE)
  ) |>
  spread(is_category, growth) |>
  rename(
    in_category_growth = `TRUE`,
    out_category_growth = `FALSE`
  ) 

write.csv(patents, gsub("raw", "processed", file_name))