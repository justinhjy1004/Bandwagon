library(tidyverse)
library(lubridate)
library(stargazer)
library(ivreg)

patents <- read_csv("./Data/processed.csv")

citation <- patents |>
  group_by(assignee_id) |>
  summarize(
    citation = mean(num_citation),
    switches = sum(change_nber, na.rm = TRUE),
    count = n()
    )

cutoff <- quantile(citation$citation, probs = .75)

high_assignees <- citation[citation$citation > cutoff, ]$assignee_id
low_assignees <- setdiff(citation$assignee_id, high_assignees)

high <- patents[patents$assignee_id %in% high_assignees,]
low  <-  patents[patents$assignee_id %in% low_assignees,]

rm(patents, citation)


# For HIGH
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_hcit <- lm(change_nber ~ 
                       diff_growth + log(lag_citation), 
                     data = high)

summary(first_stage_hcit)

stargazer(first_stage_hcit)

#==============================================================
# IV estimate
#==============================================================

iv_model_hcit <- ivreg(log(num_citation) ~ 
                       change_nber + log(lag_citation)  |
                       log(lag_citation) + diff_growth, 
                     data = high)
summary(iv_model_hcit)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_hcit <- lm(log(num_citation) ~ 
                 log(lag_citation) + change_nber, 
               data = high)

summary(no_iv_hcit)


# For LOW
#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage_lcit <- lm(change_nber ~ 
                       diff_growth + log(lag_citation), 
                     data = low)

summary(first_stage_lcit)

stargazer(first_stage_lcit)

#==============================================================
# IV estimate
#==============================================================

iv_model_lcit <- ivreg(log(num_citation) ~ 
                       change_nber + log(lag_citation)  |
                       log(lag_citation) + diff_growth, 
                     data = low)
summary(iv_model_lcit)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv_lcit <- lm(log(num_citation) ~ 
                 log(lag_citation) + change_nber, 
               data = low)

summary(no_iv_lcit)

