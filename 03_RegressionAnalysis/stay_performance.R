library(tidyverse)
library(lubridate)

#==================================================================
# Get the Difference between Growth Rates
# in NBER categories across the years
# Considering IN and OUT Categories
#==================================================================

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

#==============================================================
# Get Patents and Match by NBER category
# And the Difference Between Growth Rates
#==============================================================

raw_patents <- read_csv("raw_roots_top.csv")

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
  ) |>
  arrange(assignee_id,patent_date) |>
  group_by(assignee_id) |>
  mutate(
    prev_nber = lag(nber),
    diff_growth = in_category_growth - out_category_growth,
    change_nber = ifelse(prev_nber == nber, 0, 1),
    lag_citation = lag(num_citation)
    ) |>
  drop_na()
  
#==============================================================
# 'Manually' perform 2SLS for IV estimate
#==============================================================

first_stage <- lm(change_nber ~ diff_growth + log(lag_citation), patents)
summary(first_stage)

library(stargazer)
stargazer(first_stage)

patents$fit_changed <- first_stage$fitted.values

second_stage <- lm(log(num_citation) ~ log(lag_citation) + fit_changed, patents)
summary(second_stage)

# Switching Roots corresponds to a -3.3899 decrease in log citation,
# which is way larger (magnitude-wise) than the non IV estimate below.

#==============================================================
# Using ivreg for IV estimate
#==============================================================

library(ivreg)

iv_model <- ivreg(log(num_citation) ~ change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, data = patents)
summary(iv_model)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv <- lm(log(num_citation) ~ log(lag_citation) + change_nber, patents)
summary(no_iv)

no_iv <- lm(asinh(num_citation) ~ asinh(lag_citation) * change_nber, patents)
summary(no_iv)

#===============================================================
# Diff-in-Diff Model
#===============================================================

lm_model <- lm(log(num_citation) ~ log(lag_citation) * change_nber, patents)
summary(lm_model)

pois_model <- glm(log(num_citation) ~ log(lag_citation) * change_nber,
                data = patents, family="poisson")
summary(pois_model)

stargazer(no_iv, iv_model, lm_model, pois_model)

# d(log(citation))/d(change_nber) = 0.35060 - 0.1758log(lag_citation)
# While switching has static negative change as seen in the IV 
# estimate, the Diff and Diff implies that it depends how well the
# firm was doing in the previous root (as proxied by lag citation)
# Firms that are underperforming lose little (and in fact, could
# have benefited from switching) but good performing firms in their
# roots that switch gets penalized more.



#=======================================================
# ARIMA
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
    lag_diff = lag(diff)     )

plot(cat_diff$diff, cat_diff$lag_diff)
arima <- lm(diff~lag_diff, cat_diff)
summary(arima)

stargazer(arima)

#=============================================
# Section by Performance
#=============================================

performance <- raw_patents |>
  group_by(assignee_id) |>
  summarise(avg_citation = mean(num_citation))

hist(log(performance$avg_citation))
qr_3 <- summary(performance$avg_citation)[[2]]

under_third <- performance |>
  filter(avg_citation < qr_3)
under_third <- under_third$assignee_id

in_under_third <- patents$assignee_id %in% under_third
  
under_patents <- patents[in_under_third,]

iv_model <- ivreg(log(num_citation) ~ change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, data = under_patents)
summary(iv_model)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv <- lm(log(num_citation) ~ log(lag_citation) + change_nber, under_patents)
summary(no_iv)


over_patents <- patents[!in_under_third,]

iv_model <- ivreg(log(num_citation) ~ change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, data = over_patents)
summary(iv_model)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv <- lm(log(num_citation) ~ log(lag_citation) + change_nber, over_patents)
summary(no_iv)

#=============================================================
# Difference in switching behavior
#=============================================================

switchers <- patents |>
  ungroup() |>
  group_by(assignee_id) |>
  summarise(
    num_switches = sum(change_nber),
    num_citation = mean(num_citation)
  ) |>
  filter(
    num_switches >= 2
  )

# ONLY ONCE

patents_only_once <- patents[!(patents$assignee_id %in% switchers$assignee_id),]

no_iv <- lm(log(num_citation) ~ log(lag_citation) * change_nber, patents_only_once)
summary(no_iv)

library(ivreg)

iv_model <- ivreg(log(num_citation) ~ change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, data = patents_only_once)
summary(iv_model)

# MORE THAN ONCE

patents_mult <- patents[(patents$assignee_id %in% switchers$assignee_id),]

no_iv <- lm(log(num_citation) ~ log(lag_citation) + change_nber, patents_mult)
summary(no_iv)

library(ivreg)

iv_model <- ivreg(log(num_citation) ~ change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, data = patents_mult)
summary(iv_model)

#============================================================
# Other Performance Induced Switch
#============================================================
summary(lm(change_nber ~ log(lag_citation) + diff_growth, data = patents_mult))
summary(lm(change_nber ~ log(lag_citation) + diff_growth, data = patents_only_once))
