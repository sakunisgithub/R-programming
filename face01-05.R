df <- read.csv('https://raw.githubusercontent.com/sakunisgithub/data_sets/refs/heads/master/face01.csv')

dim(df)

names(df)

head(df)

plot(NA, NA, xlab = "", ylab = "",
     xlim = c(min(df$x), max(df$x)),
     ylim = c(min(df$y), max(df$y)),
     xaxt = "n", yaxt = "n",
     frame.plot = FALSE)

abline(h = 0, col = "blue", lwd = 3)
abline(v = 0, col = "blue", lwd = 3)

points(df$x, df$y, pch = 19)

theta <- pi/2

A <- matrix(c(cos(theta), -sin(theta),
              sin(theta), cos(theta)),
            nrow = 2, ncol = 2,
            byrow = TRUE)

df_mat <- as.matrix(df)
df_mat <- t(df_mat)

df_transformed <- A %*% df_mat
df_transformed <- t(df_transformed)

df_transformed <- as.data.frame(df_transformed)
colnames(df_transformed) <- names(df)

plot(NA, NA, xlab = "", ylab = "",
     xlim = c(min(df_transformed$x), max(df_transformed$x)),
     ylim = c(min(df_transformed$y), max(df_transformed$y)),
     xaxt = "n", yaxt = "n",
     frame.plot = FALSE)

abline(h = 0, col = "blue", lwd = 3)
abline(v = 0, col = "blue", lwd = 3)

points(df_transformed$x, df_transformed$y, pch = 19)
