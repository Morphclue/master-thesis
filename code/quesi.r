if (!require('pacman')) install.packages('pacman')
p_load(pacman)

calculate_quesi <- function(data) {
  workload <- mean(data[, c(1, 6, 11)])
  goals <- mean(data[, c(2, 7, 12)])
  learning <- mean(data[, c(3, 8, 13)])
  familiarity <- mean(data[, c(4, 9, 14)])
  error <- mean(data[, c(5, 10)])

  sd_workload <- sd(data[, c(1, 6, 11)])
  sd_goals <- sd(data[, c(2, 7, 12)])
  sd_learning <- sd(data[, c(3, 8, 13)])
  sd_familiarity <- sd(data[, c(4, 9, 14)])
  sd_error <- sd(data[, c(5, 10)])

  results <- data.frame()
  results <- rbind(results, c(workload, goals, learning, familiarity, error))
  results <- rbind(results, c(sd_workload, sd_goals, sd_learning, sd_familiarity, sd_error))
  colnames(results) <- c("workload", "goals", "learning", "familiarity", "error")
  rownames(results) <- c("mean", "sd")

  quesi <- mean(data)
  sd_quesi <- sd(data)
  results <- cbind(results, c(quesi, sd_quesi))
  colnames(results)[6] <- "quesi"
  results <- round(results, 2)
  return(results)
}

data <- read.csv("./code/fca-quesi.csv", header = TRUE, check.names = FALSE, encoding = "UTF-8")
# data <- read.csv("./code/list-quesi.csv", header = TRUE, check.names = FALSE, encoding = "UTF-8")
data <- as.matrix(data[, 1:14])
data_high <- data[c(2, 5, 6, 10),]
data_low <- data[-c(2, 5, 6, 10),]

calculate_quesi(data)
print("High news consumption")
calculate_quesi(data_high)
print("Low news consumption")
calculate_quesi(data_low)
