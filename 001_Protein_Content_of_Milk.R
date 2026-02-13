# install.packages("nlme") # stands for non linear mixed effects

library(nlme) 

data("Milk")

View(Milk)

dim(Milk)

library(tidyverse)

Milk %>%
  filter(Diet == "barley") %>%
    ggplot(aes(x = Time, y = protein, group = Cow)) +
    geom_line(linewidth = 1) +
    labs(x = "Weeks", y = "Protein Content")

Milk %>%
  filter(Diet == "barley+lupins") %>%
  ggplot(aes(x = Time, y = protein, group = Cow)) +
  geom_line(linewidth = 1) +
  labs(x = "Weeks", y = "Protein Content")

Milk %>%
  filter(Diet == "lupins") %>%
  ggplot(aes(x = Time, y = protein, group = Cow)) +
  geom_line(linewidth = 1) +
  labs(x = "Weeks", y = "Protein Content")
