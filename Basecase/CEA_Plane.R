library(dampack)

strategies <- c(
  "AUG",
  "COM",
  "PSY alone",
  "PSY+AD",
  "ESK+AD",
  "rTMS+AD",
  "ECT+AD"
)

costs <- c(
  16185,
  16163,
  19538,
  21555,
  29061,
  30607,
  46471
)

qalys <- c(
  2.895,
  2.903,
  2.879,
  2.926,
  2.950,
  2.936,
  3.004
)

icer_my <- calculate_icers(
  cost = costs,
  effect = qalys,
  strategies = strategies
)

print(icer_my)

plot(icer_my)

library(ggplot2)
library(scales)

ref_cost <- 16185
ref_qaly <- 2.895

df <- data.frame(
  Strategy = strategies,
  DeltaCost = costs - ref_cost,
  DeltaQALY = qalys - ref_qaly
)

wtp <- 50000
wtp3 <- 3 * wtp

int <- ref_cost - wtp * ref_qaly
int3 <- ref_cost - wtp3 * ref_qaly

n_lines <- 400
offsets <- seq(-1e5, 2e6, length.out = n_lines)

frontier_strategies <- c("AUG", "COM", "PSY+AD", "ESK+AD", "ECT+AD")
df_frontier <- df[df$Strategy %in% frontier_strategies, ]
df_frontier <- df_frontier[order(df_frontier$DeltaQALY), ]

ggplot(df, aes(x = DeltaQALY, y = DeltaCost, color = Strategy)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_text(
    aes(
      label = Strategy,
      y = DeltaCost + ifelse(Strategy == "COM", -1e3, 1e3)
    ),
    fontface = "bold",
    show.legend = FALSE,
    size = 3.5
  ) +
  geom_line(
    data = df_frontier,
    aes(x = DeltaQALY, y = DeltaCost),
    color = "#5A5A89",
    alpha = 0.6,
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_abline(
    intercept = 0,
    slope = 50000,
    color = "red",
    linetype = "dashed",
    size = 0.6,
    alpha = 0.8
  ) +
  geom_abline(
    intercept = 0,
    slope = 150000,
    color = "red",
    linetype = "dashed",
    size = 0.6,
    alpha = 0.8
  ) +
  annotate(
    "text",
    x = 0.065,
    y = 0.055 * 50000,
    label = "WTP = US$50,000 per QALY",
    color = "black",
    hjust = 0,
    size = 4
  ) +
  annotate(
    "text",
    x = 0.065,
    y = 0.060 * 150000,
    label = "3xWTP = US$150,000 per QALY",
    color = "black",
    hjust = 0,
    size = 4
  ) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Incremental QALYs",
    y = "Incremental Cost (US$)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  lapply(offsets, function(off) {
    geom_abline(
      slope = wtp,
      intercept = int + off,
      linetype = "dashed",
      color = "grey85",
      size = 0.4
    )
  })

ggsave(
  filename = "cea_plane.tif",
  plot = last_plot(),
  width = 12,
  height = 7.5,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)
