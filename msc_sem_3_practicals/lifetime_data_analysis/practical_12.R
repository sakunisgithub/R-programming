rm(list=ls())
library(survival)

data(veteran)
veteran$trt <- factor(veteran$trt, labels = c("Standard", "Test"))

fit_surv <- survfit(Surv(time, status) ~ trt, data = veteran)

plot(fit_surv, col = c("blue", "red"), lwd = 2,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves by Treatment",
     mark.time = TRUE)
legend("topright", legend = c("Standard", "Test"),
       col = c("blue", "red"), lwd = 2)

fit_haz <- survfit(Surv(time, status) ~ trt, data = veteran, type = "fh")

plot(fit_haz, fun = "cumhaz", col = c("blue", "red"), lwd = 2,
     xlab = "Time (days)", ylab = "Cumulative Hazard",
     main = "Cumulative Hazard Curves by Treatment")
legend("topleft", legend = c("Standard", "Test"),
       col = c("blue", "red"), lwd = 2)

wilcox_test <- survdiff(Surv(time, status) ~ trt, data = veteran, rho = 1)
wilcox_test
