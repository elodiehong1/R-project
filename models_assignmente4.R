# Setup R package
library(survival)
library(ggplot2)
library(ggfortify)
# Question 1
# 1
# Male twins data
male_time <- c(53,51,52,59,63,64,69,62,67,74,73,51,66,64)
# Female twins data
female_time <- c(60,63,58,70,75,79,71,70,78,74,81,61,73,79)
# combine data
time <- c(male_time, female_time)
# Status: 0=censored, 1=not censored
status <- c(1,1,0,0,1,0,0,1,1,0,1,1,0,1,
                   1,0,1,0,1,1,0,0,1,0,0,1,0,1)
# combine data
group <- rep(c(0, 1), c(14, 14))
data <- data.frame(time, status, group)
# Estimate the survival function
data$group <- factor(data$group,
                     levels = c(0,1),
                     labels = c("Male twins","Female twins"))

km_fit <- survfit(Surv(time, status) ~ group, data=data)
summary(km_fit)
summary(km_fit)$surv[summary(km_fit)$strata=="group=Male twins"]
summary(km_fit)$surv[summary(km_fit)$strata=="group=Female twins"]
# Survival plot
autoplot(km_fit) +
  ggplot2::labs(
    title = "Kaplan-Meier Survival Curves",
    x = "Age",
    y = "Survival probability"
  )

# 2
logrank_test <- survdiff(Surv(time, status) ~ group, data = data)
logrank_test

# 3
weibull_model <- survreg(Surv(time, status) ~ group,
                         data = data,
                         dist = "weibull")

summary(weibull_model)
################################################################
# Question 2
################################################################
# 1
# Male data
male_time <- c(53,51,52,59,63,64,69,62,67,74,73,51,66,64)
# Female data
female_time <- c(60,63,58,70,75,79,71,70,78,74,81,61,73,79)
# combine data
time <- c(male_time, female_time)
# Status: 0=censored, 1=not censored
status <- c(1,1,0,0,1,0,0,1,1,0,1,1,0,1,
            1,0,1,0,1,1,0,0,1,0,0,1,0,1)
# combine data
group <- rep(c(0, 1), c(14, 14))
# Severe: 0 = not severe, 1 = severe
severe <- c(
  1,1,0,1,0,0,1,1,0,0,0,1,0,0,
  1,0,1,0,0,1,1,0,1,0,0,1,0,0
)
data_with_severe <- data.frame(time, status, severe, group)
# Convert severe to factor for better legend
data_with_severe$severe <- factor(data_with_severe$severe,
                           levels = c(0,1),
                           labels = c("Not severe","Severe"))
#Subset Male data
male_data <- subset(data_with_severe, group == 0)
# Estimate the survival function
km_male <- survfit(Surv(time, status) ~ severe, data = male_data)
sm <- summary(km_male)
sm$surv[sm$strata=="severe=Not severe"]
sm$surv[sm$strata=="severe=Severe"]
# Survival male plot
autoplot(km_male) +
  ggplot2::labs(
    title = "Kaplan-Meier Survival Curves of Male Twins",
    x = "Age",
    y = "Survival probability"
  )

#Subset female data
female_data <- subset(data_with_severe, group == 1)
# Estimate the survival function
km_female <- survfit(Surv(time, status) ~ severe, data = female_data)
sf <- summary(km_female)
sf$surv[sf$strata=="severe=Not severe"]
sf$surv[sf$strata=="severe=Severe"]
# Survival female plot
autoplot(km_female) +
  ggplot2::labs(
    title = "Kaplan-Meier Survival Curves of Female Twins",
    x = "Age",
    y = "Survival probability"
  )

# 2
#Cox PH model
cox_model <- coxph(Surv(time, status) ~ severe + group, data = data_with_severe)
summary(cox_model)
#Weibull model
weibull_model_2 <- survreg(Surv(time, status) ~ severe + group,
                         data = data_with_severe,
                         dist = "weibull")
summary(weibull_model_2)
scale_w <- weibull_model_2$scale
w_coef <- coef(weibull_model_2)[-1]
weibull_hr <- exp(-w_coef / scale_w)
weibull_hr
#Exponential model
exp_model_2 <- survreg(Surv(time, status) ~ severe + group,
                     data = data_with_severe,
                     dist = "exponential")
summary(exp_model_2)


