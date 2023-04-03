if (!require('pacman')) install.packages('pacman')
p_load(pacman, reshape2, ggplot2, viridis)

hamming_distance <- function(a, b) {
  sum(a != b)
}

plot_hamming_heatmap <- function(file_name) {
  articles <- read.csv(paste0("./code/", file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")

  article_count <- nrow(articles)
  hamming_matrix <- matrix(0, nrow = article_count, ncol = article_count)

  for (i in 1:article_count) {
    for (j in 1:article_count) {
      distance <- hamming_distance(articles[i, -1], articles[j, -1])
      max_distance <- ncol(articles) - 1
      hamming_matrix[i, j] <- distance / max_distance
    }
  }

  hamming_df <- melt(hamming_matrix)
  colnames(hamming_df) <- c("Artikel1", "Artikel2", "Distanz")
  hamming_df$Artikel1 <- articles$Nr[hamming_df$Artikel1]
  hamming_df$Artikel2 <- articles$Nr[hamming_df$Artikel2]

  ggplot(hamming_df, aes(x = Artikel1, y = Artikel2, fill = Distanz)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(heat.colors(256))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x = "Artikel", y = "Artikel", fill = "Normierte\nHamming-Distanz", title = "Normierte Hamming-Distanz zwischen Artikeln") +
    scale_x_continuous(breaks = articles$Nr) +
    scale_y_continuous(breaks = articles$Nr)
}

plot_hamming_heatmap("list-articles")
# plot_hamming_heatmap("fca-articles")
