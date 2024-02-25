assembly_defects <- read.csv("D:\\data_sets\\sqc_practical_q11_data.csv")
dim(assembly_defects)
names(assembly_defects)
View(assembly_defects)
u <- assembly_defects$number_of_defects / assembly_defects$sample_size
u
u_bar <- mean(u)
u_bar
lcl <- u_bar - 3 * sqrt(u_bar / assembly_defects$sample_size)
lcl <- round(lcl, digits = 2)
lcl
ucl <- u_bar + 3 * sqrt(u_bar / assembly_defects$sample_size)
ucl <- round(ucl, digits = 2)
ucl
