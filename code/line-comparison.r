if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2, reshape2, tidyr, dplyr)

hamming_distance <- function(a, b) {
  return(sum(a != b))
}

plot_single_line <- function(article_file_name, person_file_name) {
  articles <- read.csv(paste0("./code/", article_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")
  persons <- read.csv(paste0("./code/", person_file_name, ".csv"), sep = ";", header = TRUE, check.names = FALSE, encoding = "UTF-8")

  person_cum_distance <- lapply(seq_len(nrow(persons)), function(i) {
    person <- persons[i, 2:ncol(persons)]
    person_articles <- articles[match(person, articles$Nr),]
    person_row <- person_articles[, -1]
    names(person_row) <- colnames(articles)[-1]

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

  df_mean <- df %>%
    group_by(Article) %>%
    summarise(Distance = mean(Distance))

  return(df_mean)
}

df_list_mean <- plot_single_line("list-articles", "list-persons")
df_fca_mean <- plot_single_line("fca-articles", "fca-persons")

ggplot() +
  geom_line(data = df_list_mean, aes(x = Article, y = Distance, color = "List")) +
  geom_line(data = df_fca_mean, aes(x = Article, y = Distance, color = "FCA")) +
  labs(x = "Artikel", y = "Kumulative Hamming-Distanz",
       title = "Durchschnittliche kumulative Hamming-Distanz zwischen aufeinanderfolgenden Artikeln",
       color = "Prototyp") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1, length(df_list_mean$Article), 1)) +
  scale_y_discrete(limits = c(0, 8))
