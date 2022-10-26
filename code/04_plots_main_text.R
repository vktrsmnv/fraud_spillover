source("code/functions.R")

# Main Paper Plots: Pooled Data ####
## Differences for pooled data ####

pp_pol <-
  prep_plotting(path = "output/ol_main_pol.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )
pp_pol_83 <-
  prep_plotting(path = "output/ol_main_pol.rds",
                output = "diffs",
                model = "condition",
                BF = FALSE,
                ci = 0.83
  )
# main_pol_collapsed
pp <-
  prep_plotting(path = "output/ol_main_pol_collapsed.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_pol_correct.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_la_pol_872.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

pp <- # makes no difference
  prep_plotting(path = "output/ol_main_la_pol_collapsed.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_pol_la_fe.rds",
                output = "diffs",
                model = "condition + questnnr",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_pol_la_int.rds",
                output = "diffs",
                model = "condition + questnnr",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_ru_pol_1215.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

pp <-
  prep_plotting(path = "output/ol_main_ru_pol_667.rds",
                output = "diffs",
                model = "condition",
                BF = FALSE,
                ci = 0.95
  )


nested_settings <- strip_nested(
  text_x = list(element_text(face = "bold",
                             size = rel(1.2)), NULL),
  # bleed = TRUE,
  background_x = list(element_rect(fill = NA), NULL),
  size = "variable",
  by_layer_x = TRUE
  # by_layer_x = TRUE
)


arrows <- data.frame(
  x1 = 0.15,
  x2 = 0.1,
  y1 = 2.1,
  y2 = 1.2,
  Condition = c("Control"),
  type = "Institutions Related to Legislative Elections",
  institution_facet_name = levels(pp_pol$plotting$institution_facet_name)[2]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp_pol$plotting$institution_facet_name)
      )
  )

plt_pol <- pp_pol$plotting %>%
  filter(
    condition == "Control",
    institution %in% levels(pp_pol$plotting$institution)[2:9],
  ) %>%
  mutate( institution = droplevels(institution)) %>%
  mutate(type = case_when(institution %in% levels(pp_pol$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections",
                          institution %in% levels(pp_pol$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                          # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
  ) %>%
    as_factor(),
  # questnnr = questnnr %>% str_to_title()
  ) %>%
  ggplot(aes(
    x = median,
    y = category
  )) +
  # tidybayes::stat_pointinterval(
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
  geom_linerange(data = pp_pol_83$plotting %>%
                   filter(
                     condition == "Control",
                     institution %in% levels(pp_pol$plotting$institution)[2:9],
                   ) %>%
                   mutate( institution = droplevels(institution)) %>%
                   mutate(type = case_when(institution %in% levels(pp_pol$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections",
                                           institution %in% levels(pp_pol$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                                           # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
                   ) %>%
                     as_factor(),
                   ),
                 aes(
                   x = median,
                   xmin = lower,
                   xmax = upper,
                   y = category,
                   color = condition,
                   # shape = condition,
                   alpha = significant
                 ),
                 position = position_dodge(1),
                 size = 0.8
  ) +
  facet_manual(
    vars(type, institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    trim_blank = FALSE,
    remove_labels = "none",
    design = c(
      "
      AAAAABBBBBCCCCC##########
      DDDDDEEEEEFFFFFGGGGGHHHHH
      "
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  # theme(strip.text.x = element_text(size = 12)) +
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
    xintercept = 0, color = "grey50"
    # alpha =
  ) +
  guides(
    color = "none",
    alpha = "none",
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
    color = "grey20",
    curvature = -0.3
  ) +
  geom_abline(
    intercept = 2.5,
    slope = -15,
    size = 0.5,
    alpha = 0.3,
    linetype = 2,
    color = "grey20"
  ) +
  geom_text(
    data = data.frame(
      median = 0.15,
      category = "Quite\na Lot",
      condition = c("Control"),
      type = "Institutions Related to Legislative Elections",
      institution_facet_name = levels(pp_pol$plotting$institution_facet_name)[2]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(pp_pol$plotting$institution_facet_name)
          ),
        category = as.factor(category) %>%
          fct_expand(levels(pp_pol$plotting$category))
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.1,
    # color = plasma(1, 0.9)
    color = "grey20"
  )

plt_pol

ggsave(plt_pol,
       filename = paste0("figs/diffs_ol_main_pol.png"),
       height = 6,
       width = 10
)




## Non-political Institutions ####


pp_npol <-
  prep_plotting(path = "output/ol_main_npol.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95
  )

plt_npol <- pp_npol$plotting %>%
  filter(
    condition == "Control",
    institution %in% unique(pp_npol$plotting$institution)
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
      # shape = questnnr,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_manual(
    vars("", institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    remove_labels = "none",
    trim_blank = FALSE,
    design = c(
      "
      ABC##
      DFG##
      "
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  # theme(strip.text.x = element_text(size = 12)) +
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
    color = "grey50"
  ) +
  guides(
    color = "none",
    alpha = "none",
    shape = "none"
  ) +
  xlim(-0.15, 0.15)
plt_npol

ggsave(plt_npol,
       filename = paste0("figs/diffs_ol_main_npol.png"),
       height = 6,
       width = 10
)


## Conditional effect:  diffs for pooled data ####

pp_conditional <-
  prep_plotting(
    "output/ol_cond_pol.rds",
    output = "diffs",
    model = "condition + opponent",
    BF = FALSE,
    PD = FALSE,
    ci = 0.95
  )


pp_conditional$plotting <- pp_conditional$plotting %>%
  mutate(condition = fct_recode(condition,
                                "Judicial\nPunishment" = "Judicial Punishment"))

pp_conditional_control <-
  prep_plotting("output/ol_cond_pol.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = FALSE,
                baseline = "Control",
                ci = 0.95
  )

# pp <-
#   prep_plotting("output/ol_cond_pol_correct.rds",
#                 output = "diffs",
#                 model = "condition + opponent",
#                 BF = FALSE,
#                 PD = FALSE,
#                 ci = 0.95
#   )
#
# pp$plotting <- pp$plotting %>%
#   mutate(condition = fct_recode(condition, "Judicial\nPunishment" = "Judicial Punishment"))
#

arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  condition = c("Judicial\nPunishment"),
  opponent = "Opponent",
  institution_facet_name = levels(pp_conditional$plotting$institution_facet_name)[2]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp_conditional$plotting$institution_facet_name)
      )
  )
plt_conditional <- pp_conditional$plotting %>%
  filter(
    condition != "Control",
    # condition == "Control",
    condition != "Fraud",
    condition != "Punishment",
    institution %in% levels(pp_conditional$plotting$institution)[2:9]
  ) %>%
  mutate( institution = droplevels(institution)) %>%
  mutate(type = case_when(institution %in% levels(pp_pol$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections",
                          institution %in% levels(pp_pol$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                          # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
  ) %>%
    as_factor()
  ) %>%
  # mutate(condition = fct_rev(condition)) %>%
  ggplot(aes(
    x = median,
    y = category
  )) +
  # geom_abline(
  #   intercept = 2.5,
  #   slope = 15,
  #   size = 0.5,
  #   linetype = 2,
  #   color = "black",
  #   alpha = 0.7
  # ) +
  # geom_abline(
  #   intercept = 2.5,
  #   slope = -15,
#   size = 0.5,
#   linetype = 3,
#   color = plasma(3)[2],
#   alpha = 0.7
# ) +
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
  facet_manual(
    vars(type, institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    remove_labels = "none",
    design = c(
      "
      AAAAABBBBBCCCCC##########
      DDDDDEEEEEFFFFFGGGGGHHHHH
      "
    )
    # design = c(
    #   "
    #   ABCDEFGH
    #   "
    # )
  ) +
  # facet_grid(
  #   # . ~ institution_facet_name,
  #   # cols = vars(institution_facet_name),
  #   rows = vars(condition),
  #   # ncol = 4
  # ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(2, 4)]) +
  # scale_shape_manual(values = c(15, 17)) +
  # scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Judicial Punishment)"
      # "Probability(Category|Control) - Probability(Category|Punishment)"

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
    # shape = "none"
  ) +
  # geom_text(
  #   data = data.frame(
  #     median = c(0.2, -0.2),
  #     category = c(3.5, 3.5),
  #     opponent = "Opponent",
  #     condition = levels(pp[[1]]$condition)[4],
  #     institution_facet_name = levels(pp[[1]]$institution_facet_name)[1]
  #   ) %>%
  #     mutate(
  #       institution_facet_name = as_factor(.$institution_facet_name) %>%
  #         fct_expand(
#           levels(pp[[1]]$institution_facet_name)
#         )
#     ),
#   label = c(
#     "Punishment\ndecreases\ntrust",
#     "Punishment\nincreases\ntrust"
#   ),
#   size = 2,
#   color = c("black", plasma(3)[2]),
#   alpha = 0.7
# ) +
# xlim(-0.55, 0.55) +
# scale_color_manual(values = plasma(4, end = 0.8)[c(4, 2)]) +
scale_shape_manual(values = c(15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  theme(
    legend.position = c(0.8, 0.8), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = NA, colour = NA)
  )

plt_conditional

ggsave(plt_conditional,
       filename = paste0("figs/diffs_pooled_cond_punishment_1.png"),
       height = 6,
       width = 10
)


probs_pol <-
  prep_plotting(path = "output/ol_main_pol.rds",
                output = "probs",
                model = "condition",
                BF = FALSE,
                ci = 0.95
  )


# Main Paper Plots: Pooled with interaction ####
## Predicted probabilities for pooled data ####

pp <-
  prep_plotting("output/ol_main_pol_int.rds",
                output = "probs",
                model = "condition + questnnr",
                BF = FALSE
  )


plt <- pp[[1]] %>%
  filter(condition %in% c("Fraud", "Control")) %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
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
  facet_nested(
    cols = vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
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
       filename = paste0("figs/probs_ol_main_pol_control_int.png"),
       height = 6,
       width = 10
)

## Differences for pooled data ####

pp <-
  prep_plotting("output/ol_main_pol_int.rds",
                output = "diffs",
                model = "condition + questnnr",
                BF = FALSE,
                ci = 0.95
  )

pp <-
  prep_plotting("output/ol_main_pol.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE,
                ci = 0.95

  )

pp$plotting %>%
  filter(institution == "Elections") %>%
  mutate(category = fct_rev(category))


nested_settings <- strip_nested(
  text_x = list(element_text(face = "bold",
                             size = rel(1.2)), NULL),
  # bleed = TRUE,
  background_x = list(element_rect(fill = NA), NULL),
  size = "variable",
  by_layer_x = TRUE
  # by_layer_x = TRUE
)


arrows <- data.frame(
  x1 = 0.15,
  x2 = 0.1,
  y1 = 2.1,
  y2 = 1.2,
  Condition = c("Control"),
  type = "Institutions Related to Legislative Elections",
  institution_facet_name = levels(pp[[1]]$institution_facet_name)[2]
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
    institution %in% levels(pp$plotting$institution)[2:9],
  ) %>%
  mutate( institution = droplevels(institution)) %>%
  mutate(type = case_when(institution %in% levels(pp$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                          # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
                          institution %in% levels(pp$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections") %>%
           as_factor()
  ) %>%
  arrange(institution, type) %>%
  ggplot(aes(
    x = median,
    y = category
  )) +
  # tidybayes::stat_pointinterval(
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
  facet_manual(
    vars(type, institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    remove_labels = "none",
    design = c(
      "
      ###AAAAABBBBBCCCCC######
      DDDDEEEEEFFFFFGGGGGHHHHH
      "
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  # theme(strip.text.x = element_text(size = 12)) +
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
    xintercept = 0, color = "grey50"
    # alpha =
  ) +
  guides(
    color = "none",
    alpha = "none",
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
    color = "grey20",
    curvature = -0.3
  ) +
  geom_abline(
    intercept = 2.5,
    slope = -15,
    size = 0.5,
    alpha = 0.3,
    linetype = 2,
    color = "grey20"
  ) +
  geom_text(
    data = data.frame(
      median = 0.15,
      category = "Quite\na Lot",
      condition = c("Control"),
      type = "Institutions Related to Legislative Elections",
      institution_facet_name = levels(pp[[1]]$institution_facet_name)[2]
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
    # color = plasma(1, 0.9)
    color = "grey20"

  )

plt



ggsave(plt,
       filename = paste0("figs/diffs_ol_main_pol.png"),
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

plt <- pp$plotting %>%
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
    condition != "Control",
    condition != "Fraud",
    institution %in% levels(pp[[1]]$institution)
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
    # color = "none",
    # alpha = "none",
    # shape = "none"
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      opponent = "Opponent",
      condition = levels(pp[[1]]$condition)[4],
      institution_facet_name = levels(pp[[1]]$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(.$institution_facet_name) %>%
          fct_expand(
            levels(pp[[1]]$institution_facet_name)
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
  xlim(-0.55, 0.55) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(4, 2)]) +
  scale_shape_manual(values = c(15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1))
plt

ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond_punishment.png"),
       height = 6,
       width = 10
)






# ## Conditional effect:  PPs for pooled data ####
#
# pp <-
#   prep_plotting("output/ol_cond_pol.rds",
#                 output = "probs",
#                 model = "condition + opponent",
#                 BF = FALSE,
#                 PD = FALSE
#   )
#
# plt <- pp$plotting %>%
#   filter(!condition %in% c("Fraud", "Control")) %>%
#   ggplot() +
#   geom_pointrange(
#     aes(
#       x = median,
#       xmin = lower,
#       xmax = upper,
#       y = category,
#       color = condition,
#       shape = opponent
#     ),
#     position = position_dodge(1),
#     size = 0.4
#   ) +
#   facet_nested(
#     cols = vars(institution_facet_name),
#     rows = vars(opponent),
#     # ncol = 4
#   ) +
#   scale_color_manual(values = plasma(4, end = 0.8)[c(4, 2)]) +
#   scale_shape_manual(values = c(15, 17, 8)) +
#   scale_alpha_manual(values = c(0.4, 1)) +
#   labs(
#     y = "Confidence",
#     x = paste0(
#       "Probability(Category)"
#     ),
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   guides(
#     # color = "none",
#     alpha = "none",
#     # shape = "none"
#   )
# plt
#
# ggsave(plt,
#        filename = paste0("figs/probs_ol_cond_pol.png"),
#        height = 6,
#        width = 10
# )
#

pp_conditional_correct <-
  prep_plotting("output/ol_cond_pol_correct.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = FALSE,
                # baseline = "Control",
                ci = 0.95
  )

arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  condition = c("Judicial\nPunishment"),
  opponent = "Opponent",
  institution_facet_name = levels(pp_conditional_correct$plotting$institution_facet_name)[2]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp_conditional_correct$plotting$institution_facet_name)
      )
  )
plt_conditional_correct <- pp_conditional_correct$plotting %>%
  filter(
    condition != "Control",
    # condition == "Control",
    condition != "Fraud",
    condition != "Punishment",
    institution %in% levels(pp_conditional_correct$plotting$institution)[2:9]
  ) %>%
  mutate( institution = droplevels(institution)) %>%
  mutate(type = case_when(institution %in% levels(pp_conditional_correct$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections",
                          institution %in% levels(pp_conditional_correct$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                          # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
  ) %>%
    as_factor()
  ) %>%
  # mutate(condition = fct_rev(condition)) %>%
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
      shape = opponent,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_manual(
    vars(type, institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    remove_labels = "none",
    design = c(
      "
      AAAAABBBBBCCCCC##########
      DDDDDEEEEEFFFFFGGGGGHHHHH
      "
    )
  ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(2, 4)]) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Judicial Punishment)"
      # "Probability(Category|Control) - Probability(Category|Punishment)"

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
    # shape = "none"
  ) +
  scale_shape_manual(values = c(15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1))
plt_conditional_correct
ggsave(plt_conditional_correct,
       filename = paste0("figs/diffs_pooled_cond_judicial_punishment_correct.png"),
       height = 6,
       width = 10
)


pp_conditional_ru <-
  prep_plotting("output/ol_cond_ru_pol_UR_corect_1215.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = FALSE,
                # baseline = "Control",
                ci = 0.95
  )

arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  condition = c("Judicial\nPunishment"),
  opponent = "Opponent",
  institution_facet_name = levels(pp_conditional_ru$plotting$institution_facet_name)[2]
) %>%
  mutate(
    institution_facet_name = as_factor(institution_facet_name) %>%
      fct_expand(
        levels(pp_conditional_ru$plotting$institution_facet_name)
      )
  )


plt_conditional_ru <- pp_conditional_ru$plotting %>%
  filter(
    condition != "Control",
    # condition == "Control",
    condition != "Fraud",
    condition == "Punishment",
    institution %in% levels(pp_conditional_ru$plotting$institution)[2:9]
  ) %>%
  mutate( institution = droplevels(institution)) %>%
  mutate(type = case_when(institution %in% levels(pp_conditional_ru$plotting$institution)[1:4] ~ "Institutions Related to Legislative Elections",
                          institution %in% levels(pp_conditional_ru$plotting$institution)[5:9] ~ "No Direct Relationship to Legislative Elections",
                          # institution %in% levels(pp$plotting$institution)[4:6] ~ "",
  ) %>%
    as_factor()
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
      shape = opponent,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_manual(
    vars(type, institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    remove_labels = "none",
    design = c(
      "
      AAAAABBBBBCCCCC##########
      DDDDDEEEEEFFFFFGGGGGHHHHH
      "
    )
  ) +
  scale_color_manual(values = plasma(4, end = 0.8)[c(2, 4)]) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Judicial Punishment)"
      # "Probability(Category|Control) - Probability(Category|Punishment)"

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
    # shape = "none"
  ) +
  scale_shape_manual(values = c(15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1))

plt_conditional_ru
ggsave(plt_conditional_ru,
       filename = paste0("figs/diffs_pooled_cond_judicial_punishment_ru_correct.png"),
       height = 6,
       width = 10
)



