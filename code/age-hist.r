if (!require('pacman')) install.packages('pacman')
p_load(pacman, ggplot2)

data <- read.csv("./code/age.csv", sep = ";", header = TRUE, encoding = "UTF-8")
data$Klassenbreite <- cut(data$Alter, breaks = seq(0, 80, 5), right = FALSE)

ggplot(data, aes(x = Klassenbreite, fill = factor(Person > 8))) +
  geom_bar(position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("#619CFF", "#FF7A5A"), name = "Datensatz",
                    labels = c("Vorstudie", "Masterarbeit")) +
  labs(x = "Alter", y = "Personen")
