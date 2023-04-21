if (!require('pacman')) install.packages('pacman')
p_load(pacman)

frequency <- c(0, 14, 1, 0.25, 14, 14, 2, 1, 3, 7)
classes <- c(0, 6, 14)

hist(frequency, breaks = classes, col = '#619CFF', border = 'black',
     main = "",
     xlab = "News-Konsum pro Woche",
     ylab = "relative Anzahl der Personen")
