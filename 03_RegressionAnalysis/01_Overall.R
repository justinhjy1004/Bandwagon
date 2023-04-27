library(tidyverse)
library(stargazer)
library(ivreg)

patents <- read_csv("./Data/processed.csv")

#==============================================================
# FIRST STAGE in IV for Relevance of Instrument
#==============================================================

first_stage <- lm(change_nber ~ 
                  diff_growth + log(lag_citation), 
                  patents)
summary(first_stage)

stargazer(first_stage)

#==============================================================
# IV estimate
#==============================================================

iv_model <- ivreg(log(num_citation) ~ 
                  change_nber + log(lag_citation)  |
                  log(lag_citation) + diff_growth, 
                  data = patents)
summary(iv_model)

#===============================================================
# Comparison Model without IV
#===============================================================

no_iv <- lm(log(num_citation) ~ 
            log(lag_citation) + change_nber, 
            data = patents)

summary(no_iv)


#===============================================================
# Interaction Model OLS
#===============================================================

interaction_model <- lm(log(num_citation) ~ 
                        log(lag_citation) * change_nber, 
                        data = patents)
summary(interaction_model)

pois_model <- glm(log(num_citation) ~ 
                  log(lag_citation) * change_nber,
                  data = patents, family="poisson")
summary(pois_model)

stargazer(no_iv, iv_model, interaction_model, pois_model)

