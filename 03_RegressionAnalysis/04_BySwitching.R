library(tidyverse)
library(lubridate)
library(stargazer)
library(ivreg)
library(xtable)

patents <- read_csv("./Data/processed.csv")

by_assignees <- patents |>
  group_by(assignee_id) |>
  summarize(
    citation = mean(num_citation),
    switches = sum(change_nber, na.rm = TRUE),
    count = n()
  )

by_assignees |>
  select(-assignee_id) |>
  mutate_all(log1p) -> corr_table

cutoff <- 1

high_assignees <- by_assignees[by_assignees$switches > cutoff, ]$assignee_id
low_assignees <- setdiff(by_assignees$assignee_id, high_assignees)

high <- patents[patents$assignee_id %in% high_assignees,]
low  <-  patents[patents$assignee_id %in% low_assignees,]

rm(patents, by_assignees)


# For HIGH
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_hs <- lm(change_nber ~ 
                         diff_growth + log(lag_citation), 
                       data = high)

summary(first_stage_hs)

stargazer(first_stage_hs)

#==============================================================
# IV estimate
#==============================================================

iv_model_hs <- ivreg(log(num_citation) ~ 
                         change_nber + log(lag_citation)  |
                         log(lag_citation) + diff_growth, 
                       data = high)
summary(iv_model_hs)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_hs <- lm(log(num_citation) ~ 
                   log(lag_citation) + change_nber, 
                 data = high)

summary(no_iv_hs)


# For LOW
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_ls <- lm(change_nber ~ 
                       diff_growth + log(lag_citation), 
                       data = low)

summary(first_stage_ls)

stargazer(first_stage_ls)

#==============================================================
# IV estimate
#==============================================================

iv_model_ls <- ivreg(log(num_citation) ~ 
                         change_nber + log(lag_citation)  |
                         log(lag_citation) + diff_growth, 
                       data = low)
summary(iv_model_ls)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_ls <- lm(log(num_citation) ~ 
                   log(lag_citation) + change_nber, 
                 data = low)

summary(no_iv_ls)

stargazer(first_stage_hc,first_stage_lc,first_stage_hs,first_stage_ls)
stargazer(iv_model_hc,iv_model_lc, iv_model_hs, iv_model_ls)
