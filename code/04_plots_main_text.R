source("code/functions.R")

# Main Paper Plots ####
## Predicted probabilities for pooled data ####

pp <-
  prep_plotting("output/ol_main_pol.rds",
                output = "probs",
                model = "condition",
                BF = FALSE
  )


plt <- pp %>%
  filter(condition %in% c("Fraud", "Control")) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = condition,
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_wrap(
    . ~(institution_facet_name),
    # rows = vars(condition),
    ncol = 4
  ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(3, 1)]) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  guides(
    # color = "none",
    alpha = "none",
    shape = "none"
  )

plt
ggsave(plt,
       filename = paste0("figs/probs_ol_main_pol_control.png"),
       height = 6,
       width = 10
)

## Differences for pooled data ####

pp <-
  prep_plotting("output/ol_main_pol.rds",
                output = "diffs",
                model = "condition"
                )


arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution_facet_name = levels(pp$institution_facet_name)[1]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp$institution_facet_name)
      )
  )

plt <- pp %>%
  filter(
    condition == "Control",
    institution %in% levels(pp$institution)
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
    ncol = 4
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
      institution_facet_name = levels(pp$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(pp$institution_facet_name)
          ),
        category = as.factor(category) %>%
          fct_expand(levels(pp$category))
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
       filename = paste0("figs/diffs_ol_main_pol_control.png"),
       height = 6,
       width = 10
)


## Conditional effect:  PPs for pooled data ####

pp <-
  prep_plotting("output/ol_cond_pol.rds",
                output = "probs",
                model = "condition + opponent",
                BF = FALSE,
                PD = FALSE
  )

plt <- pp %>%
  filter(!condition %in% c("Fraud", "Control")) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = condition,
      shape = opponent
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_nested(
    cols = vars(institution_facet_name),
    rows = vars(opponent),
    # ncol = 4
  ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(4, 2)]) +
  scale_shape_manual(values = c(15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  guides(
    # color = "none",
    alpha = "none",
    # shape = "none"
  )
plt

ggsave(plt,
       filename = paste0("figs/probs_ol_cond_pol.png"),
       height = 6,
       width = 10
)

## Conditional effect:  diffs for pooled data ####

pp <-
  prep_plotting("output/ol_cond_pol.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = FALSE
  ) %>%
  mutate(condition = fct_recode(condition, "Judicial\nPunishment" = "Judicial Punishment"))



arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  condition = c("Judicial\nPunishment"),
  opponent = "Opponent",
  institution_facet_name = levels(pp$institution_facet_name)[1]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp$institution_facet_name)
      )
  )
plt <- pp %>%
  filter(
    condition != "Control",
    institution %in% levels(pp$institution)
  ) %>%
  mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(
    x = median,
    y = category
  )) +
  geom_abline(
    intercept = 2.5,
    slope = 15,
    size = 0.5,
    linetype = 2,
    color = "black",
    alpha = 0.7
  ) +
  geom_abline(
    intercept = 2.5,
    slope = -15,
    size = 0.5,
    linetype = 3,
    color = plasma(3)[2],
    alpha = 0.7
  ) +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
       color = condition,
      shape = opponent,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_grid(
    # . ~ institution_facet_name,
    cols = vars(institution_facet_name),
    rows = vars(condition, opponent),
    # ncol = 4
  ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(2, 4)]) +
  scale_shape_manual(values = c(15, 17)) +
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
    color = "none",
    alpha = "none",
    shape = "none"
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      opponent = "Opponent",
      condition = levels(pp$condition)[4],
      institution_facet_name = levels(pp$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(pp$institution_facet_name)
          )
      ),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishment\nincreases\ntrust"
    ),
    size = 2,
    color = c("black", plasma(3)[2]),
    alpha = 0.7
  ) +
  xlim(-0.35, 0.35)
plt

ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond_punishment.png"),
       height = 6,
       width = 10
)





