# Install and load the glmmTMB package if needed
library(glmmTMB)
library(haven) 
library(ggplot2)
library(pROC)
library(DHARMa)
library(car)
library(lme4)

dat <- read_sav("C:/Users/jacki/OneDrive/Desktop/Master's Thesis paper/DATA/PNC.sav")

names(dat)

m1 <- glmer(
  ANC ~ Division +Wealth_combined+  Place_of_delivery + Woman_age +Had_fever_in_last_two_weeks+Age_of_Woman_at_first_birth+Total_children_ever_born+
    (1+Sex_of_household_head+Place_of_delivery+Household_head_age | Household_Ratio),
  data   = dat,
  family = binomial(link = "logit")
)



summary(m1)





