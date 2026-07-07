getwd()
df <- read.csv('df_long.csv')
library(dplyr)
library(ordinal)
df <- df %>%
  mutate(
    mmse = as.numeric(mmse),  
    mmse_tertile = case_when(
      mmse >= 11 & mmse <= 24 ~ "Low",
      mmse >= 25 & mmse <= 28 ~ "Medium",
      mmse >= 29 & mmse <= 30 ~ "High"
    )
  )

df$mmse_tertile <- factor(df$mmse_tertile,
                          levels = c("Low","Medium","High"),
                          ordered = TRUE)

table(df$mmse_tertile)


# Cumulative Link Mixed Model (CLMM)
model_clmm <- clmm(
  mmse_tertile ~ k10sum * factor(timepoint) + age  + factor(education) + factor(pmh_any)+
    factor(ethnicity_cat) + factor(work_status) + factor(marital_status) + factor(house_cat) + factor(gender) +
    (1|LinkageID),  
  data = df,
  link = "logit"
)

summary(model_clmm)

#extract OR and 95% CI
coef_est <- coef(summary(model_clmm))
OR <- exp(coef_est[, "Estimate"])
CI_lower <- exp(coef_est[, "Estimate"] - 1.96 * coef_est[, "Std. Error"])
CI_upper <- exp(coef_est[, "Estimate"] + 1.96 * coef_est[, "Std. Error"])

results <- data.frame(
  Predictor = rownames(coef_est),
  Estimate = coef_est[, "Estimate"],
  StdError = coef_est[, "Std. Error"],
  z = coef_est[, "z value"],
  p = coef_est[, "Pr(>|z|)"],
  OR = OR,
  CI_lower = CI_lower,
  CI_upper = CI_upper
)

print(results)

library(lme4)

#Low vs Medium+High
library(sandwich)
library(lmtest)
df$low_binary <- ifelse(df$mmse_tertile == "Low", 1, 0)
model_low <- glm(
  low_binary ~ k10sum + age + factor(timepoint) + factor(education) + factor(pmh_any) +
    factor(ethnicity_cat) + factor(work_status) + factor(marital_status) + factor(house_cat) + factor(gender),
  family = binomial,
  data = df
)

# Clustered SE by LinkageID
coeftest(model_low, vcov = vcovCL(model_low, cluster = ~LinkageID))

#Medium vs Low+High
df$medium_binary <- ifelse(df$mmse_tertile == "Medium", 1, 0)

model_medium <- glm(
  medium_binary ~ k10sum + age + factor(timepoint) + factor(education) + factor(pmh_any) +
    factor(ethnicity_cat) + factor(work_status) + factor(marital_status) + factor(house_cat) + factor(gender),
  family = binomial,
  data = df
)

# Clustered SE by LinkageID
coeftest(model_medium, vcov = vcovCL(model_medium, cluster = ~LinkageID))

#High vs Low+Medium
df$high_binary <- ifelse(df$mmse_tertile == "High", 1, 0)

model_high <- glm(
  high_binary ~ k10sum + age + factor(timepoint) + factor(education) + factor(pmh_any) +
    factor(ethnicity_cat) + factor(work_status) + factor(marital_status) + factor(house_cat) + factor(gender),
  family = binomial,
  data = df
)

# Clustered SE by LinkageID
coeftest(model_high, vcov = vcovCL(model_high, cluster = ~LinkageID))

library(sandwich)
library(lmtest)


df$mmse_tertile <- factor(df$mmse_tertile, levels = c("Low", "Medium", "High"), ordered = TRUE)

# Linear regression
model_k10 <- lm(
  k10sum ~ factor(mmse_tertile) + age + factor(timepoint) + factor(education) + factor(pmh_any) +
    factor(ethnicity_cat) + factor(work_status) + factor(marital_status) + factor(house_cat) + factor(gender),
  data = df
)

# Clustered SE by LinkageID
coeftest(model_k10, vcov = vcovCL(model_k10, cluster = ~LinkageID))


