if (!require('pacman')) install.packages('pacman')
p_load(pacman, proxy, ggplot2)

# dataframe lead to a few problems
liste_pq <- 1.24
fca_pq <- 0.74
liste_hq <- 0.06
fca_hq <- 1.08
liste_pq_conf <- 0.76
fca_pq_conf <- 0.94
liste_hq_conf <- 0.82
fca_hq_conf <- 0.45

limit <- 3
intercept <- limit / 3
pos <- intercept * 2

ggplot() +
  geom_blank() +
  scale_x_continuous(limits = c(-limit, limit), expand = c(0, 0), name = "PQ") +
  scale_y_continuous(limits = c(-limit, limit), expand = c(0, 0), name = "HQ") +
  annotate("rect", xmin = -limit, xmax = limit, ymin = -limit, ymax = limit, fill = "gray60", color = NA) +
  geom_vline(xintercept = c(-intercept, intercept), color = "white", size = 2) +
  geom_hline(yintercept = c(-intercept, intercept), color = "white", size = 2) +
  annotate("text", x = -pos, y = pos, label = "zu selbstorientiert", size = 7) +
  annotate("text", x = 0, y = pos, label = "selbstorientiert", size = 7) +
  annotate("text", x = pos, y = pos, label = "begehrt", size = 7) +
  annotate("text", x = 0, y = 0, label = "neutral", size = 7) +
  annotate("text", x = pos, y = 0, label = "handlungsorientiert", size = 7) +
  annotate("text", x = -pos, y = -pos, label = "\u00FCberfl\u00FCssig", size = 7) +
  annotate("text", x = pos, y = -pos, label = "zu handlungsorientiert", size = 7) +
  geom_point(data = data.frame(x = liste_pq, y = liste_hq), aes(x = x, y = y), size = 4, shape = 15) +
  geom_point(data = data.frame(x = fca_pq, y = fca_hq), aes(x = x, y = y), size = 4, shape = 15) +
  geom_rect(aes(
    xmin = liste_pq - liste_pq_conf,
    xmax = liste_pq + liste_pq_conf,
    ymin = liste_hq - liste_hq_conf,
    ymax = liste_hq + liste_hq_conf,
    alpha = 0.2, fill = "Liste")) +
  geom_rect(aes(
    xmin = fca_pq - fca_pq_conf,
    xmax = fca_pq + fca_pq_conf,
    ymin = fca_hq - fca_hq_conf,
    ymax = fca_hq + fca_hq_conf,
    alpha = 0.2, fill = "Begriffsverband")) +
  guides(
    color = "none",
    alpha = "none",
    fill = guide_legend(title = "Legende")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold")
  )
