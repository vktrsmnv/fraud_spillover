data_grid(data = mpol$pol_inst_CEC$data, condition) %>%
  add_epred_draws(object = mpol$pol_inst_CEC) %>%
  median_qi(.width = 0.89) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = .epred,
      xmin = .lower,
      xmax = .upper,
      y = .category,
      color = condition,
      shape = condition
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_grid(
    rows = vars(condition),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category",
      ")"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  # geom_vline(
  #   xintercept = 0,
  #   alpha = 0.5
  # ) +
  guides(color = "none", alpha = "none",
         shape = "none")
