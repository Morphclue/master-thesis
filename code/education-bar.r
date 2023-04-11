if (!require('pacman')) install.packages('pacman')
p_load(pacman, ggplot2, tidyr)

data <- read.csv("./code/education.csv", sep = ";", header = TRUE, encoding = "UTF-8")

counts_study <- aggregate(Person ~ Bildung, data[1:8,], length)
counts_thesis <- aggregate(Person ~ Bildung, data[9:18,], length)

data <- merge(counts_study, counts_thesis, by = "Bildung", all = TRUE)
data[is.na(data)] <- 0
colnames(data) <- c("Bildung", "Vorstudie", "Masterarbeit")

data_tidy <- data %>%
  gather(key = "Ursprung", value = "Anzahl", -Bildung)

ggplot(data_tidy, aes(x = Bildung, y = Anzahl, fill = Ursprung)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.35) +
  scale_x_discrete(limits = c("Realschule", "Abitur", "Bachelor", "Master", "Promotion"),
                   labels = c("Realschule", "Abitur", "Bachelor", "Master", "Promotion")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(name = "", values = c("Masterarbeit" = "#ff7f0e", "Vorstudie" = "#1f77b4")) +
  labs(x = "", y = "Anzahl der Personen") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", face = "bold"),
        axis.ticks = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
