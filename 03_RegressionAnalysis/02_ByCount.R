library(tidyverse)
library(lubridate)
library(stargazer)
library(ivreg)

patents <- read_csv("./Data/processed.csv")

count <- patents |>
  group_by(assignee_id) |>
  summarize(count = n())

cutoff <- quantile(count$count, probs = .75)

high_assignees <- count[count$count > cutoff, ]$assignee_id
low_assignees <- setdiff(count$assignee_id, high_assignees)

high <- patents[patents$assignee_id %in% high_assignees,]
low  <-  patents[patents$assignee_id %in% low_assignees,]

rm(patents, count)


# For HIGH
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_hc <- lm(change_nber ~ 
                  diff_growth + log(lag_citation), 
                  data = high)

summary(first_stage_hc)

stargazer(first_stage_hc)

#==============================================================
# IV estimate
#==============================================================

iv_model_hc <- ivreg(log(num_citation) ~ 
                    change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, 
                  data = high)
summary(iv_model_hc)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_hc <- lm(log(num_citation) ~ 
            log(lag_citation) + change_nber, 
            data = high)

summary(no_iv_hc)


# For LOW
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_lc <- lm(change_nber ~ 
                  diff_growth + log(lag_citation), 
                  data = low)
summary(first_stage_lc)

stargazer(first_stage_lc)

#==============================================================
# IV estimate
#==============================================================

iv_model_lc <- ivreg(log(num_citation) ~ 
                    change_nber + log(lag_citation)  |
                    log(lag_citation) + diff_growth, 
                  data = low)
summary(iv_model_lc)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_lc <- lm(log(num_citation) ~ 
              log(lag_citation) + change_nber, 
            data = low)

summary(no_iv_lc)

