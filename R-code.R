# Install and load the glmmTMB package if needed

library(glmmTMB)
library(haven) 
library(ggplot2)
library(pROC)





dat <- read_sav("C:/Users/jacki/OneDrive/Desktop/Master's Thesis paper/DATA/F-6.sav")



dat$wgt <- dat$V005 / 1000000
names(dat)
# Fit a zero-inflated Poisson multilevel model
model_zip <- glmmTMB(
  Contraceptive_using~ Wealth_combined + Woman_age+ Desire_for_children+
    Opinion_on_use_of_contraception+V201
  +Age_of_Woman_at_first_sex+Religion+Own_a_house+
    Source_Of_Method+Opinion_on_use_of_contraception+
    Total_children_ever_born+(1 |Place_of_Residence), # Count model, with random intercept for group
  ziformula = ~ Household_Ratio+Sex_of_household_head+
    Husband_age+Husband_desire_for_children+
    (1 | Place_of_Residence),   # Zero-inflation model (predictors & group random effect)
  family = binomial,
  data = dat
)

# Check summary
summary(model_zip)



# For predicted probabilities (on response/probability scale)
dat$predicted_prob <- predict(model_zip, type = "response")

print(dat$predicted_prob)

#ROC curve



roc_obj <- roc(dat$Contraceptive_using, dat$predicted_prob)
plot(roc_obj, main="ROC Curve: Model Prediction vs Actual",col="#0072B2", lwd=2)

roc_overall <- roc(dat$Contraceptive_using, dat$predicted_prob)
plot(roc_overall, main="Overall ROC Curve", col="#0072B2", lwd=2)
cat("AUC:", round(auc(roc_overall), 3))


# Split ROC by cluster
clusters <- unique(dat$Place_of_Residence)
plot(NULL, xlim=c(1,0), ylim=c(0,1), xlab="Specificity", ylab="Sensitivity", main="ROC by Cluster")
colors <- c("#D55E00", "#0072B2", "#009E73", "#CC79A7")  # Add more colors for more clusters

for (i in seq_along(clusters)) {
  idx <- dat$Place_of_Residence == clusters[i]
  roc_curve <- roc(dat$Contraceptive_using[idx], dat$predicted_prob[idx])
  plot(roc_curve, col=colors[i], lwd=2, add=TRUE)
  cat("Cluster:", clusters[i], "- AUC:", round(auc(roc_curve), 3), "\n")
}
legend("bottomright", legend=clusters, col=colors[1:length(clusters)], lwd=2)





#Distribution plot


ggplot(dat, aes(x = predicted_prob, y = as.numeric(Contraceptive_using))) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3) +
  labs(
    x = "Predicted Probability of Use",
    y = "Actual Contraceptive Use (0 = No, 1 = Yes)",
    title = "Predicted Probability vs Actual Use"
  ) +
  theme_minimal()





ggplot(dat, aes(x = as.factor(Contraceptive_using), y = predicted_prob,
                fill = as.factor(Contraceptive_using))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    x = "Contraceptive Use (0 = No, 1 = Yes)",
    y = "Predicted Probability",
    title = "Distribution of Predicted Probabilities by Actual Use"
  ) +
  theme_minimal()



