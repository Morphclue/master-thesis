if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2, reshape2, viridis, gridExtra)

plot_cosine_similarity <- function(file_name) {
  articles <- read.csv(paste0("./code/", file_name, ".csv"), sep = ";", header = TRUE)

  cosine_similarity <- as.matrix(proxy::simil(articles, method = "cosine"))
  cosine_similarity_df <- melt(cosine_similarity)
  colnames(cosine_similarity_df) <- c("Row", "Column", "CosineSimilarity")

  p <- ggplot(cosine_similarity_df, aes(x = Row, y = Column, fill = CosineSimilarity)) +
    geom_tile() +
    scale_fill_gradientn(colors = rev(heat.colors(256))) +
    theme_minimal() +
    scale_x_continuous(breaks = 1:13) +
    scale_y_continuous(breaks = 1:13) +
    labs(title = "Cosine Similarity Between Articles", x = "Article #", y = "Article #", fill = "Similarity") +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}

plot_cosine_similarity("list-articles")
# plot_cosine_similarity("fca-articles")
