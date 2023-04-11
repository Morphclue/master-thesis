if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2, reshape2, tidyr, dplyr)

hamming_distance <- function(a, b) {
  return(sum(a != b))
}

plot_hamming_distance <- function(article_file_name, person_file_name) {
  articles <- read.csv(paste0("./code/", article_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")
  persons <- read.csv(paste0("./code/", person_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")

  person_matrices <- lapply(seq_len(nrow(persons)), function(i) {
    person <- persons[i, 2:ncol(persons)]
    person_articles <- articles[match(person, articles$Nr),]
    person_row <- person_articles[, -1]
    names(person_row) <- colnames(articles)[-1]

    return(person_row)
  })

  person_distances <- lapply(person_matrices, function(person_row) {
    distance <- sum(sapply(1:(nrow(person_row) - 1), function(j) {
      hamming_distance(person_row[j,], person_row[j + 1,])
    }))

    return(distance)
  })

  person_distances_df <- data.frame(
    Person = seq_len(length(person_distances)),
    Distance = unlist(person_distances)
  )

  ggplot(person_distances_df, aes(x = Person, y = Distance, fill = as.factor(Person))) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = "Person", y = "Gesamtdistanz", fill = "Person", title = "Hamming-Distanzen zwischen aufeinanderfolgenden Artikeln") +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(1, nrow(persons), 1)) +
    scale_y_continuous(breaks = seq(1, 12, 1))
}

plot_hamming_distance("list-articles", "list-persons")
# plot_hamming_distance("fca-articles", "fca-persons")
