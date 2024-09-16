# Part A
friend_id <- c(1101, 2302, 3001, 4231, 5802)
friend_name <- c("Sachin", "Sourav", "Dravid", "Sehwag", "Dhoni")
SR_rate <- c(47.85, 97.22, 55.71, 79.27, 63.15)
Match_played <- c(192, 87, 89, 67, 45)

friend.data <- data.frame(friend_id, friend_name, SR_rate, Match_played)
View(friend.data)

typeof(friend.data$friend_name)

# Part B
initial_data <- read.csv("https://raw.githubusercontent.com/sakunisgithub/data_sets/master/msc_semester_1/practical_01_data.csv")

dim(initial_data)

View(initial_data)

player.data <- initial_data[1:5,]

dim(player.data)

View(player.data)

keep <- c(1, 2, 4, 5)
performance.data <- player.data[,keep]

dim(performance.data)

View(performance.data)
