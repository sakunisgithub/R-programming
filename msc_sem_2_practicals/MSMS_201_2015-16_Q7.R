df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/msc_semester_2/MSMS_201_2015-16_Q7.csv', stringsAsFactors = TRUE)

dim(df)

names(df)

summary(df$age)

(p <- summary(df$age) / dim(df)[1])

( -sum(p * log2(p)) )
