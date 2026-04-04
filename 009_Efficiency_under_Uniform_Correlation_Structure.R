e <- function(delta, rho, n){
  a <- (1 + (n - 1) * rho) * (1 - rho)
  
  b <- n * (1 + delta) * (1 - rho + n * rho * delta / (1 + delta))
  
  return(a / b)
}

library(tidyverse)

delta <- seq(0, 2, 0.01)

# n = 2

df1 <- data.frame(delta,
                  efficiency1 = e(delta = delta, rho = 0.8, n = 2),
                  efficiency2 = e(delta = delta, rho = 0.5, n = 2),
                  efficiency3 = e(delta = delta, rho = 0.2, n = 2),
                  efficiency4 = e(delta = delta, rho = 0.0, n = 2))

df1_melted <- df1 %>%
  pivot_longer(cols = starts_with("efficiency"),
               names_to = "efficiency_type",
               values_to = "efficiency")

df1_melted %>%
  ggplot(aes(x = delta, y = efficiency, linetype = efficiency_type)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c(
      "efficiency1" = "solid",
      "efficiency2" = "dashed",
      "efficiency3" = "twodash",
      "efficiency4" = "dotted"),
    labels = expression(
      rho == 0.8,
      rho == 0.5,
      rho == 0.2,
      rho == 0.0)) +
  labs(x = expression(delta)) +
  theme(legend.position = "top",
        legend.title = element_blank())




# n = 5
df2 <- data.frame(delta,
                  efficiency1 = e(delta = delta, rho = 0.8, n = 5),
                  efficiency2 = e(delta = delta, rho = 0.5, n = 5),
                  efficiency3 = e(delta = delta, rho = 0.2, n = 5),
                  efficiency4 = e(delta = delta, rho = 0.0, n = 5))

df2_melted <- df2 %>%
  pivot_longer(cols = starts_with("efficiency"),
               names_to = "efficiency_type",
               values_to = "efficiency")

df2_melted %>%
  ggplot(aes(x = delta, y = efficiency, linetype = efficiency_type)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c(
      "efficiency1" = "solid",
      "efficiency2" = "dashed",
      "efficiency3" = "twodash",
      "efficiency4" = "dotted"),
    labels = expression(
      rho == 0.8,
      rho == 0.5,
      rho == 0.2,
      rho == 0.0)) +
  labs(x = expression(delta)) +
  theme(legend.position = "top",
        legend.title = element_blank())






# n = 10
df3 <- data.frame(delta,
                  efficiency1 = e(delta = delta, rho = 0.8, n = 10),
                  efficiency2 = e(delta = delta, rho = 0.5, n = 10),
                  efficiency3 = e(delta = delta, rho = 0.2, n = 10),
                  efficiency4 = e(delta = delta, rho = 0.0, n = 10))

df3_melted <- df3 %>%
  pivot_longer(cols = starts_with("efficiency"),
               names_to = "efficiency_type",
               values_to = "efficiency")

df3_melted %>%
  ggplot(aes(x = delta, y = efficiency, linetype = efficiency_type)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(
    values = c(
      "efficiency1" = "solid",
      "efficiency2" = "dashed",
      "efficiency3" = "twodash",
      "efficiency4" = "dotted"),
    labels = expression(
      rho == 0.8,
      rho == 0.5,
      rho == 0.2,
      rho == 0.0)) +
  labs(x = expression(delta)) +
  theme(legend.position = "top",
        legend.title = element_blank())
