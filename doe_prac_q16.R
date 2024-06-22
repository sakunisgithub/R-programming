experiment_data <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/master/doe_prac_q16_data.csv', stringsAsFactors = TRUE)
dim(experiment_data)
names(experiment_data)
str(experiment_data)
View(experiment_data)
summary(experiment_data)

options(digits = 10)

experiment_data_anova <- aov(yield ~ replicate + date * method + Error(replicate/date + replicate/method), data = experiment_data)
summary(experiment_data_anova)

# library(agricolae)
# attach(experiment_data)
# strip.plot(BLOCK = replicate,
#            COL = date,
#            ROW = method,
#            Y = yield)

library(tidyverse)

# totals by replicate-date
df1 <- experiment_data %>%
  group_by(replicate, date) %>%
  summarise(totals = sum(yield))
View(df1)

# replicate totals
replicate_totals <- experiment_data %>%
  group_by(replicate) %>%
  summarise(totals = sum(yield))
View(replicate_totals)

# date totals
date_totals <- experiment_data %>%
  group_by(date) %>%
  summarise(totals = sum(yield))
View(date_totals)

G <- sum(date_totals$totals)
G

CF <- G^2 / length(experiment_data$yield)
CF

SSdate <- round(sum(date_totals$totals^2) / 12 - CF, digits = 3)
SSdate

SSreplicate <- round(sum(replicate_totals$totals^2) / 9 - CF, digits = 3)
SSreplicate

Error_1 <- round(sum(df1$totals^2) / 3 - CF - SSdate - SSreplicate, digits = 3)
Error_1

# totals by replicate-method
df2 <- experiment_data %>%
  group_by(replicate, method) %>%
  summarise(totals = sum(yield))
View(df2)

# method totals
method_totals <- experiment_data %>%
  group_by(method) %>%
  summarise(totals = sum(yield))
View(method_totals)

SSmethod <- round(sum(method_totals$totals^2) / 12 - CF, digits = 3)
SSmethod

Error_2 <- round(sum(df2$totals^2) / 3 - CF - SSmethod - SSreplicate, digits = 3)
Error_2

# totals by date-method
df3 <- experiment_data %>%
  group_by(date, method) %>%
  summarise(totals = sum(yield))
View(df3)

SSdate_method <- round(sum(df3$totals^2) / 4 - CF - SSdate - SSmethod, digits = 3)
SSdate_method

RSS <- sum(experiment_data$yield^2)
RSS

TSS <- round(RSS - CF, digits = 3)
TSS

Error_3 <- TSS - SSdate - SSreplicate - Error_1 - SSmethod - Error_2 - SSdate_method
Error_3

MSreplicate <- round(SSreplicate / 3, digits = 3)
MSreplicate

MSdate <- SSdate / 2
MSdate

MSE_1 <- round(Error_1 / 6, digits = 3)
MSE_1

MSmethod <- SSmethod / 2
MSmethod

MSE_2 <- round(Error_2 / 6, digits = 3)
MSE_2

MSdate_method <- round(SSdate_method / 4, digits = 3)
MSdate_method

MSE_3 <- round(Error_3 / 12, digits = 3)
MSE_3

MSdate / MSE_1
qf(0.05, 2, 6, lower.tail = FALSE)

MSmethod / MSE_2

MSdate_method / MSE_3
