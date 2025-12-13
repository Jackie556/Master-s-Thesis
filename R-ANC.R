# Install and load the glmmTMB package if needed
library(glmmTMB)
library(haven) 
library(ggplot2)
library(pROC)
library(DHARMa)
library(car)
library(lme4)
library(survey)


dat <- read_sav("C:/Users/jacki/OneDrive/Desktop/Master's Thesis paper/DATA/PNC.sav")



dat$wgt <- dat$V005 / 100000

data <- data.frame(dat)


# Assuming your data frame is called data

# Example: convert several variables in data to factors

data$V106  <- as.factor(data$V106)
data$V701  <- as.factor(data$V701)
data$V714  <- as.factor(data$V714)
data$Woman_working_status <- as.factor(data$Woman_working_status)
data$Atleast_8_antenatal_visits_during_pregnancy <- as.factor(data$Atleast_8_antenatal_visits_during_pregnancy)
data$Had_fever_in_last_two_weeks <- as.factor(data$Had_fever_in_last_two_weeks)
data$Sex_child <- as.factor(data$Sex_child)


# and all variable names match the SPSS names

model_pnc <- glmer(
  PNC ~ V445 + V106 + V701 + V714 + Woman_working_status +
    Atleast_8_antenatal_visits_during_pregnancy +
    Had_fever_in_last_two_weeks + Woman_age +
    Age_of_Woman_at_first_birth + Husband_age + Sex_child +
    (1 | Division:Place_of_Residence),   # random intercept for Division*Place_of_Residence
  data   = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 1e5))
)

summary(model_pnc)

# Predicted probabilities (similar to SPSS SAVE PREDICTED_VALUES)
data$PredictedValue4 <- predict(model_pnc, type = "response")





