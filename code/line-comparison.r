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
    summarise(Mean = mean(Distance), SD = sd(Distance))

  return(df_mean)
}

df_list_mean <- plot_single_line("list-articles", "list-persons")
df_fca_mean <- plot_single_line("fca-articles", "fca-persons")

ggplot() +
  geom_line(data = df_list_mean, aes(x = Article, y = Mean, color = "Liste")) +
  geom_line(data = df_fca_mean, aes(x = Article, y = Mean, color = "Begriffsverband")) +
  geom_ribbon(data = df_list_mean, aes(x = Article, ymin = ifelse(Mean - SD > 0, Mean - SD, 0), ymax = Mean + SD, fill = "Liste"), alpha = 0.2) +
  geom_ribbon(data = df_fca_mean, aes(x = Article, ymin = ifelse(Mean - SD > 0, Mean - SD, 0), ymax = Mean + SD, fill = "Begriffsverband"), alpha = 0.2) +
  geom_text(data = df_list_mean, aes(x = Article, y = Mean, label = round(Mean, 2)), hjust = -0.5, vjust = -1, size = 3, color = "blue") +
  geom_text(data = df_fca_mean, aes(x = Article, y = Mean, label = round(Mean, 2)), hjust = 1.5, vjust = -1, size = 3, color = "red") +
  labs(x = "Artikel", y = "Kumulative Hamming-Distanz", color = "Prototyp") +
  scale_x_continuous(breaks = seq(1, length(df_list_mean$Article), 1)) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
  )
