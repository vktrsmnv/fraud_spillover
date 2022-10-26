
## Differences for pooled data ####

pp <-
  prep_plotting("output/ol_main_npol.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE
  )


arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution_facet_name = levels(pp[[1]]$institution_facet_name)[1]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp[[1]]$institution_facet_name)
      )
  )

plt <- pp$plotting %>%
  filter(
    condition == "Control",
    institution %in% unique(pp[[1]]$institution)
  ) %>%
  ggplot(aes(
    x = median,
    y = category
  )) +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = condition,
      shape = condition,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_wrap(
    . ~ institution_facet_name,
    # cols = vars(institution_facet_name),
    # rows = vars(condition),
    ncol = 3
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  theme(strip.text.x = element_text(size = 12)) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Control)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(
    color = "none", alpha = "none",
    shape = "none"
  ) +
  geom_curve(
    data = arrows,
    aes(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2
    ),
    arrow = arrow(length = unit(0.08, "inch")),
    size = 0.5,
    alpha = 0.3,
    color = plasma(1),
    curvature = -0.3
  ) +
  geom_abline(
    intercept = 2.5,
    slope = -15,
    size = 0.5,
    alpha = 0.3,
    linetype = 2,
    color = plasma(1)
  ) +
  geom_text(
    data = data.frame(
      median = 0.2,
      category = "Not very\nmuch",
      condition = c("Control"),
      institution_facet_name = levels(pp[[1]]$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(pp[[1]]$institution_facet_name)
          ),
        category = as.factor(category) %>%
          fct_expand(levels(pp[[1]]$category))
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.1,
    color = plasma(1, 0.7)
  ) +
  xlim(-0.25, 0.25) +
  geom_label(aes(
    x = -0.25,
    label = paste0(bf_b_condition_control %>%
                     str_replace("evidence ", "evidence\n"), " H1"),
    y = "None\nat all"
  ),
  size = 2.7,
  nudge_y = -0.15,
  label.size = 0.05,
  lineheight = 0.8,
  alpha = 0.2,
  hjust = 0
  )

plt

ggsave(plt,
       filename = paste0("figs/diffs_ol_main_npol_control.png"),
       height = 6,
       width = 10
)


