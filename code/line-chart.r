if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2, reshape2, tidyr, dplyr)

hamming_distance <- function(a, b) {
  return(sum(a != b))
}

plot_line <- function(article_file_name, person_file_name) {
  articles <- read.csv(paste0("./code/", article_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")
  persons <- read.csv(paste0("./code/", person_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")

  person_matrices <- lapply(seq_len(nrow(persons)), function(i) {
    person <- persons[i, 2:ncol(persons)]
    person_articles <- articles[match(person, articles$Nr),]
    person_row <- person_articles[, -1]
    names(person_row) <- colnames(articles)[-1]

    return(person_row)
  })

  person_cum_distance <- lapply(person_matrices, function(person_row) {
    distance <- sapply(1:(nrow(person_row) - 1), function(j) {
      hamming_distance(person_row[j,], person_row[j + 1,])
    })

    cum_distance <- cumsum(distance)
    cum_distance <- c(0, cum_distance)

    return(cum_distance)
  })

  df <- data.frame(Person = rep(seq_len(length(person_cum_distance)), each = length(person_cum_distance[[1]])),
                   Article = rep(seq_along(person_cum_distance[[1]]), times = length(person_cum_distance)),
                   Distance = unlist(person_cum_distance))

  ggplot(df, aes(x = Article, y = Distance, group = Person)) +
    geom_line(aes(color = factor(Person))) +
    labs(x = "Artikel", y = "Kumulative Hamming-Distanz", color = "Person") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(1, length(person_cum_distance[[1]]), 1)) +
    scale_y_discrete(limits = 0:12) +
    coord_cartesian(ylim = c(0, 12))
}

plot_line("list-articles", "list-persons")
# plot_line("fca-articles", "fca-persons")
