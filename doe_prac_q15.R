experiment_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q15_data.csv', stringsAsFactors = TRUE)
dim(experiment_data)
names(experiment_data)
str(experiment_data)
View(experiment_data)
summary(experiment_data)

options(digits = 10)

# by sp.plot() function
library(agricolae)
with(experiment_data, sp.plot(block = block, pplot = variety, splot = manure, Y = yield))

# by aov() function
experiment_data_anova <- aov(yield ~ variety * manure + Error(block / variety), data = experiment_data)
summary(experiment_data_anova)

library(tidyverse)

# whole-plot totals
whole_plot_totals <- experiment_data %>%
  group_by(variety, block) %>%
  summarise(totals = sum(yield))
View(whole_plot_totals)

# variety totals
variety_totals <- experiment_data %>%
  group_by(variety) %>%
  summarise(totals = sum(yield))
View(variety_totals)

# block totals
block_totals <- experiment_data %>%
  group_by(block) %>%
  summarise(totals = sum(yield))
View(block_totals)

# variety-manure treatment combination totals
variety_manure_treatment_combination_totals <- experiment_data %>%
  group_by(variety, manure) %>%
  summarise(totals = sum(yield))
View(variety_manure_treatment_combination_totals)

# manure totals
manure_totals <- experiment_data %>%
  group_by(manure) %>%
  summarise(totals = sum(yield))
View(manure_totals)

# grand total
G <- sum(experiment_data$yield)
G

CF <- G^2 / length(experiment_data$yield)
CF

RSS <- sum(experiment_data$yield^2)
RSS

SSblocks <- round(sum(block_totals$totals^2) / 12 - CF, digits = 3)
SSblocks

SSvariety <- round(sum(variety_totals$totals^2) / 16 - CF, digits = 3)
SSvariety

SSE_1 <- round(sum(whole_plot_totals$totals^2) / 4 - CF - SSblocks - SSvariety, digits = 3)
SSE_1

SSmanure <- round(sum(manure_totals$totals^2) / 12 - CF, digits = 3)
SSmanure

SSvariety_manure <- round(sum(variety_manure_treatment_combination_totals$totals^2) / 4 - CF - SSvariety - SSmanure, digits = 3)
SSvariety_manure

TSS <- round(RSS - CF, digits = 3)
TSS

SSE_2 <- TSS - SSblocks - SSvariety - SSE_1 - SSmanure - SSvariety_manure
SSE_2

MSblocks <- SSblocks / 3
MSblocks

MSvariety <- SSvariety / 2
MSvariety

MSE_1 <- round(SSE_1 / 6, digits = 3)
MSE_1

MSmanure <- SSmanure / 3
MSmanure

MSvariety_manure <- round(SSvariety_manure / 6, digits = 3)
MSvariety_manure

MSE_2 <- round(SSE_2 / 27, digits = 3)
MSE_2

round(MSvariety_manure / MSE_2, digits = 3)
qf(0.05, 6, 27, lower.tail = FALSE)
