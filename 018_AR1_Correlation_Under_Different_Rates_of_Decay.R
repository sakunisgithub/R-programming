rho <- function(u, phi) exp(- phi * u)

u <- seq(0, 20, 0.1)

df1 <- data.frame(u,
                  autocorrelation1 = rho(u = u, phi = 0.1),
                  autocorrelation2 = rho(u = u, phi = 0.25),
                  autocorrelation3 = rho(u = u, phi = 1.0))

library(tidyverse)

df1_melted <- df1 %>%
  pivot_longer(cols = starts_with("autocorrelation"),
               names_to = "autocorrelation_case",
               values_to = "autocorrelation")

df1_melted %>%
  ggplot(aes(x = u, y = autocorrelation, linetype = autocorrelation_case)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c(
      "autocorrelation1" = "solid",
      "autocorrelation2" = "dotted",
      "autocorrelation3" = "dashed"),
    labels = expression(
      phi == 0.1,
      phi == 0.25,
      phi == 1.0)) +
  labs(x = expression(u), y = expression(rho(u))) +
  theme(legend.position = "top",
        legend.title = element_blank())
