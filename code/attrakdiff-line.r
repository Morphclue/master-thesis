if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2, tidyr)

data <- data.frame(
  Kategorie = c("PQ", "HQ-I", "HQ-S", "ATT"),
  Liste = c(1.24, 0.5, -0.39, 1.2),
  Begriffsverband = c(0.74, 0.69, 1.47, 1.37)
)
data$Kategorie <- factor(data$Kategorie, levels = c("PQ", "HQ-I", "HQ-S", "ATT"))

ggplot(data, aes(x = Kategorie)) +
  geom_segment(
    aes(
      x = Kategorie,
      xend = c(tail(levels(Kategorie), -1), NA),
      y = Liste,
      yend = c(tail(Liste, -1), NA)),
    color = "#619CFF") +
  geom_segment(
    aes(x = Kategorie,
        xend = c(tail(levels(Kategorie), -1), NA),
        y = Begriffsverband,
        yend = c(tail(Begriffsverband, -1), NA)),
    color = "#FF7A5A") +
  geom_point(aes(y = Liste, color = "Liste"), size = 4, shape = 15, fill = "#619CFF") +
  geom_point(aes(y = Begriffsverband, color = "Begriffsverband"), size = 4, shape = 15, fill = "#FF7A5A") +
  geom_text(aes(y = Liste - 0.2, label = round(Liste, digits = 2)), vjust = -0.5, size = 3.5) +
  geom_text(aes(y = Begriffsverband + 0.05, label = round(Begriffsverband, digits = 2)), vjust = -0.5, size = 3.5) +
  scale_color_manual(values = c("Liste" = "#619CFF", "Begriffsverband" = "#FF7A5A")) +
  labs(x = "Kategorie", y = "Mittelwert", colour = "Darstellungsform") +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_x_discrete(limits = c("PQ", "HQ-I", "HQ-S", "ATT")) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
  )
