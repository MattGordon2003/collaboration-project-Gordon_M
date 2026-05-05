pacman::p_load(showtext, ggplot2, tidyverse, harrypotter, patchwork, readr, here, ggrepel)

data <- read_csv(here("data", "clean_data.csv"))

# Add font
font_add(
  family = "times",
  regular = here::here("figs", "Times New Roman.ttf")
)

extremes <- data %>%
  group_by(treatment, sex) %>%
  slice_min(AF, n = 1, with_ties = FALSE) %>%
  bind_rows(
    data %>%
      group_by(treatment, sex) %>%
      slice_max(AF, n = 1, with_ties = FALSE)
  ) %>%
  ungroup()

plot <- ggplot(data, aes(x = treatment, y = AF, fill = sex)) +
  geom_boxplot(position = position_dodge(width = 1), alpha = 1) +
  
  # Mean (white square)
  stat_summary(
    aes(group = sex),
    fun = mean,
    geom = "point",
    shape = 22,
    size = 3,
    fill = "white",
    colour = "black",
    position = position_dodge(width = 1)
  ) +
  
  # Highlight min/max with coloured circles
  geom_point(
    data = extremes,
    aes(fill = sex),          # same mapping as boxplot
    shape = 21,               # circle with fill
    colour = "black",
    size = 2,
    position = position_dodge(width = 1)
  ) +
  
  # Label by ID
  geom_text_repel(
    data = extremes,
    aes(label = paste("ID:", ID), group = sex),
    family = "times",
    position = position_dodge(width = 1),
    size = 3,
    max.overlaps = Inf
  ) +
  
  scale_fill_hp_d("ravenclaw") +
  theme_minimal() +
  theme(legend.position = "top", 
        text = element_text(family = "times"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(0.15, "cm")
  ) +
  labs(
    x = "Treatment",
    y = "Activating Factor",
    fill = "Sex",
    title = "Boxplot of AF against treatment",
    subtitle = "White squares are sample means"
  )

ggsave(
  filename = here::here("figs", "AF_vs_treatment.tiff"),
  plot = plot,
  width = 9,
  height = 6,
  dpi = 500
)
