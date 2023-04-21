if (!require('pacman')) install.packages('pacman')
p_load(pacman)

frequency <- c(0, 14, 1, 0.25, 14, 14, 2, 1, 3, 7)

hist(frequency, breaks = seq(0, 15, 3), col = '#619CFF', border = 'black',
     main = "",
     xlab = "News-Konsum pro Woche",
     ylab = "Anzahl der Personen",
     axes = FALSE)

class_widths <- diff(seq(0, 15, 3))
class_labels <- paste0("(", seq(0, 12, 3), ", ", seq(3, 15, 3), "]")
axis(side = 1, at = seq(1.5, 13.5, by = 3), labels = class_labels)
axis(side = 2)
