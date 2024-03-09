library(daewr)
View(sugarbeet)
mod1_aov <- aov(yield ~ treat, data = sugarbeet)
summary(mod1_aov)
con <- matrix(data = c(1, -1/3, -1/3, -1/3, 0, 1, -1, 0, 0, 0, 1, -1), byrow = TRUE, nrow = 3, ncol = 4)
con
rownames(con) <- c("-fertilizer effect", "-plowed vs. broadcast", "-January vs. April")
con                   

library(gmodels)
fit.contrast(mod1_aov, "treat", con)
