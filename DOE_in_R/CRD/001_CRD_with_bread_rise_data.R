library(daewr)
View(bread)
class(bread$time)
class(bread$height)
mod1 <- lm(height ~ time, data = bread)
summary(mod1)

library(gmodels)
fit.contrast(mod1, "time", coeff = c(1, -1, 0))

mod1_aov <- aov(height ~ time, data = bread)
summary(mod1_aov)

plot(mod1, which = 5)
library(tidyverse)
ggplot(mod1, aes(x = bread$time, y = mod1$residuals)) +
  geom_point(size = 2, col = "blue") +
  geom_hline(yintercept = 0, col = "red", linewidth = 1) +
  labs(x = "Time (Factor Levels)", y = "Residuals", title = "Residual vs Factor Levels")

plot(mod1, which = 1)
ggplot(mod1, aes(x = mod1$fitted.values, y = mod1$residuals)) +
  geom_point(size = 2, col = "blue") +
  geom_hline(yintercept = 0, col = "red", linewidth = 1) +
  labs(x = "Fitted Values", y = "Residuals", title = "Residual vs Fitted Values")

plot(mod1, which = 2)
bread %>%
  ggplot(aes(sample = height)) +
  geom_qq(size = 2, col = "blue") +
  geom_qq_line(col = "red", linewidth = 1) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot")

plot(residuals(mod1) ~ loaf, main="Residuals vs Exp. Unit", font.main=1,data=bread)
bread %>%
  ggplot(aes(x = loaf, y = mod1$residuals)) +
  geom_point(size = 2, col = "blue") +
  labs(x = "Loaf", y = "Residuals", title = "Residual vs Experimental Units")

library(gglm)
gglm(mod1, which = 5)
