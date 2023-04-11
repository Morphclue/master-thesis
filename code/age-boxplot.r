if (!require('pacman')) install.packages('pacman')
p_load(pacman, ggplot2)

data <- read.csv("./code/age.csv", sep = ";", header = TRUE, encoding = "UTF-8")
data$Klassenbreite <- cut(data$Alter, breaks = seq(0, 80, 5), right = FALSE)

ggplot(data, aes(x = factor(Person > 8), y = Alter, fill = factor(Person > 8))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#619CFF", "#FF7A5A"), name = "Datensatz",
                    labels = c("Vorstudie", "Masterarbeit")) +
  scale_x_discrete(labels=c("Vorstudie", "Masterarbeit")) +
  labs(x = "Datensatz", y = "Alter") +
  theme_bw() +
  coord_flip()

study <- subset(data, Person <= 8)
summary(study$Alter)

thesis <- subset(data, Person > 8)
summary(thesis$Alter)
