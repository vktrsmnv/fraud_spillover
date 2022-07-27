source("code/functions.R")
# Main Effect ####

## Split Sample Analysis ####
### Political ####
model_files <- list.files("output",
                          pattern = "ol_main_[a-z]+_pol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"),
                      2))

for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
    lst[[str_remove(pth,
                    "\\.rds")]] <- prep_plotting(
                      pth,
                      output = "diffs") %>%
      ggplot() +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = condition,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_wrap(
        . ~ institution_facet_name,
        ncol = 4
      ) +
      scale_alpha_manual(values = c(0.4, 1)) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(16, 15, 17, 8)) +
      labs(
        y = "Confidence",
        x = "Probability (Category|Fraud) - Probability (Category|Condition)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
      xlim(-0.3, 0.3)

    if (s == "only_correct"){
      lst[[str_remove(pth, "\\.rds")]] <-
        lst[[str_remove(pth, "\\.rds")]] +
        xlim(-0.42, 0.42)
    }
  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_", s, ".png"),
    height = 12,
    width = 10
  )
}

### Political: Control Condition Only ####
model_files <- list.files("output",
                          pattern = "ol_main_[a-z]+_pol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"),
                      2))
for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
    temp <- prep_plotting(
      pth,
      output = "diffs")

    arrows <- data.frame(
      x1 = 0.19,
      x2 = 0.1,
      y1 = 1.55,
      y2 = 1.2,
      Condition = c("Control"),
      institution_facet_name = levels(temp$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(temp$institution_facet_name)
          )
      )
    lst[[str_remove(pth,
                    "\\.rds")]] <- temp %>%
      filter(condition == "Control") %>%
      ggplot(aes(
        x = median,
        y = category
      )) +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = condition,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_wrap(
        . ~ institution_facet_name,
        ncol = 4
      ) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(16)) +
      scale_alpha_manual(values = c(0.4, 1)) +
      labs(
        y = "Confidence",
        x = "Pr(Category|Fraud) - Pr(Category|Control)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"),
                       "Russia",
                       "Latin America")
      ) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
      guides(alpha = "none",
             color = "none",
             shape = "none") +
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
          institution_facet_name = levels(temp$institution_facet_name)[1]
        ) %>%
          mutate(
            institution_facet_name = as_factor(institution_facet_name) %>%
              fct_expand(
                levels(temp$institution_facet_name)
              ),
            category = as.factor(category) %>%
              fct_expand(levels(temp$category))
          ),
        label = "fraud\ndecreases\ntrust",
        size = 2.7,
        nudge_y = 0.3,
        color = plasma(1, 0.7)
      ) +
      xlim(-0.35, 0.35)
  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_control_", s, ".png"),
    height = 12,
    width = 10
  )
}

### Non-political ####
model_files <- list.files("output",
                          pattern = "ol_main_[a-z]+_npol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"), 2))

for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
    lst[[str_remove(pth,
                    "\\.rds")]] <- prep_plotting(
                      pth,
                      output = "diffs") %>%
      ggplot() +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = condition,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_wrap(
        . ~ institution_facet_name,
        ncol = 3

      ) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(16, 15, 17, 8)) +
      scale_alpha_manual(values = c(0.4, 1)) +
      labs(
        y = "Confidence",
        x = "Probability (Category|Fraud) - Probability (Category|Condition)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      xlim(-0.35, 0.35) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
      guides(
        alpha = "none",
          # color = "none",
             # shape = "none"
             )
  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_npol_", s, ".png"),
    height = 12,
    width = 10
  )
}

## Pooled Sample (Full) ####
### Political ####
pp <-
  prep_plotting("output/ol_main_pol.rds",
                output = "diffs",
                model = "condition",
                BF = TRUE)

plt <- pp %>%
  ggplot() +
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
  facet_grid(
    vars(institution_facet_name),
    rows = vars(condition),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none",
         alpha = "none",
         shape = "none") +
  xlim(-0.22, 0.22)

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled.png"),
       height = 5,
       width = 10
)

### Political: Control Condition Only ####
# pp <-
#   prep_plotting("output/ol_main_pol.rds",
#                 output = "diffs",
#                 model = "condition")

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
  filter(condition == "Control",
         institution %in% levels(pp$institution)) %>%
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
  guides(color = "none", alpha = "none",
         shape = "none") +
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
  geom_label(aes(x = -0.25,
                label = paste0(bf_b_condition_control %>%
                  str_replace("evidence ", "evidence\n"), " H1"),
                y = "None\nat all"),
            size = 2.1,
            nudge_y = -0.18,
            label.size = 0.05,
            lineheight = 0.8,
            alpha = 0.2,
            hjust = 0)

plt

ggsave(plt,
       filename = paste0("figs/diffs_pooled_control.png"),
       height = 4,
       width = 10
)

### Non-political ####
pp <-
  prep_plotting("output/ol_main_npol.rds",
                output = "diffs",
                model = "condition",
                BF = FALSE)

plt <- pp %>%
  ggplot() +
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
  facet_grid(
    vars(institution_facet_name),
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
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none", alpha = "none",
         shape = "none") +
  scale_alpha_manual(values = c(0.4, 1))

ggsave(plt,
       filename = paste0("figs/diffs_pooled_npol.png"),
       height = 5,
       width = 10
)

## Pooled Sample with Questionnaire FE ####
### Political ####
pp <-
  prep_plotting("output/ol_main_pol_fe.rds",
                output = "diffs",
                model = "condition + questnnr",
                BF = FALSE)

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
    alpha = "none",
    # shape = "none"
  ) +
  xlim(-0.35, 0.35)


ggsave(plt,
       filename = paste0("figs/diffs_pooled_fe.png"),
       height = 6,
       width = 10
)

### Political: Control Condition Only ####

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  filter(condition == "Control") %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
  xlim(-0.35, 0.35)

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_fe_control.png"),
       height = 4,
       width = 10
)

### Non-political #####
pp <-
  prep_plotting("output/ol_main_npol_int.rds",
                output = "diffs",
                model = "condition + questnnr",
                BF = FALSE)

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
    alpha = "none",
    # shape = "none"
  ) +
  xlim(-0.35, 0.35)


ggsave(plt,
       filename = paste0("figs/diffs_pooled_fe_npol.png"),
       height = 6,
       width = 10
)


## Pooled Sample with Questionnaire Interaction ####
### Political ####
pp <-
  prep_plotting("output/ol_main_pol_int.rds",
                output = "diffs",
                BF = FALSE,
                model = "condition + questnnr")

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
         alpha = "none",
         # shape = "none"
         ) +
  xlim(-0.35, 0.35)

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_int.png"),
       height = 6,
       width = 10
)

### Political: Control Condition Only ####

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  filter(condition == "Control") %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
  xlim(-0.35, 0.35)

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_int_control.png"),
       height = 4,
       width = 10
)

### Non-political #####
pp <-
  prep_plotting("output/ol_main_npol_int.rds",
                output = "diffs",
                BF = FALSE,
                model = "condition + questnnr")

plt <- pp %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
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
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
    alpha = "none",
    # shape = "none"
  ) +
  xlim(-0.35, 0.35)

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_int_npol.png"),
       height = 6,
       width = 10
)

# Conditional Effect ####

## Split Sample Analysis ####
### Political: All ####
model_files <- list.files("output",
                          pattern = "ol_cond_[a-z]+_pol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"),
                      2))

for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
    lst[[str_remove(pth,
                    "\\.rds")]] <- prep_plotting(
                      pth,
                      model = "condition + opponent",
                      output = "diffs") %>%
      ggplot(aes(x = median,
              y = category
              )) +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = opponent,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_grid(
        rows = vars(opponent, condition),
        cols = vars(institution_facet_name),
        # ncol = 4
      ) +
      scale_alpha_manual(values = c(0.4, 1)) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(17, 15)) +
      labs(
        y = "Confidence",
        x = "Probability (Category|Fraud) - Probability (Category|Condition)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
      guides(
        color = "none",
        alpha = "none",
        shape = "none"
      )

    ggsave(
      lst[[str_remove(pth,
                      "\\.rds")]],
      filename = paste0("figs/diffs_split_cond_", s, "_",
                        ifelse(str_detect(pth, "ru"), "ru", "la"),
                        ".png"),
      height = 8.5,
      width = 10
    )

  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_cond_", s, ".png"),
    height = 12,
    width = 10
  )
}
### Political: Control ####
# model_files <- list.files("output",
#                           pattern = "ol_cond_[a-z]+_pol_[0-9]") %>%
#   str_split("_|\\.", simplify = T) %>%
#   as_tibble() %>%
#   mutate(V5 = as.numeric(V5)) %>%
#   arrange(V3, -V5) %>%
#   select(-V6) %>%
#   mutate(sample = rep(c("full",
#                         "acceptable",
#                         "only_correct"),
#                       2))

for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
    pp <- prep_plotting(
      pth,
      model = "condition + opponent",
      output = "diffs")
    arrows <- data.frame(
      x1 = 0.19,
      x2 = 0.1,
      y1 = 1.55,
      y2 = 1.2,
      opponent = "Opponent",
      Condition = c("Control"),
      institution_facet_name = levels(pp$institution_facet_name)[1]
    ) %>%
      mutate(
        institution_facet_name = as_factor(institution_facet_name) %>%
          fct_expand(
            levels(pp$institution_facet_name)
          )
      )
    lst[[str_remove(pth,
                    "\\.rds")]] <- pp %>%
      filter(condition == "Control") %>%
      ggplot(aes(x = median,
                 y = category
      )) +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = opponent,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_grid(
        rows = vars(opponent),
        cols = vars(institution_facet_name),
        # ncol = 4
      ) +
      scale_alpha_manual(values = c(0.4, 1)) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(17, 15)) +
      labs(
        y = "Confidence",
        x = "Probability (Category|Fraud) - Probability (Category|Control)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
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
          opponent = "Opponent",
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
        nudge_y = 0.3,
        color = plasma(1, 0.7)
      ) +
      xlim(-0.35, 0.35)

    ggsave(
      lst[[str_remove(pth,
                      "\\.rds")]],
      filename = paste0("figs/diffs_split_cond_control_", s, "_",
                        ifelse(str_detect(pth, "ru"), "ru", "la"),
                        ".png"),
      height = 4,
      width = 10
    )

  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_cond_control_", s, ".png"),
    height = 6,
    width = 10
  )
}

### Political: Punishment ####
model_files <- list.files("output",
                          pattern = "ol_cond_[a-z]+_pol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"),
                      2))

for (s in unique(model_files$sample)){
  pths <- model_files %>%
    filter(sample == s) %>%
    mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
    pull(pth) %>%
    paste0("output/", ., ".rds")
  lst <- list()
  for (pth in pths){
 pp <- prep_plotting(
                      pth,
                      model = "condition + opponent",
                      output = "diffs") %>%
   filter(condition != "Control")

 lst[[str_remove(pth,
                 "\\.rds")]] <- pp  %>%
      ggplot( aes(x = median,
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
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = opponent,
        color = condition,
        alpha = significant
      ),
      position = position_dodge(1),
      size = 0.4
      ) +
      facet_grid(
        rows = vars(opponent, condition),
        cols = vars(institution_facet_name),
        # ncol = 4
      ) +
      scale_alpha_manual(values = c(0.4, 1)) +
      scale_color_viridis(
        discrete = T,
        option = "C",
        end = 0.8
      ) +
      scale_shape_manual(values = c(17, 15)) +
      labs(
        y = "Confidence",
        x = "Probability (Category|Fraud) - Probability (Category|Condition)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      geom_vline(aes(xintercept = 0), alpha = 0.3) +
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

    ggsave(
      lst[[str_remove(pth,
                      "\\.rds")]],
      filename = paste0("figs/diffs_split_cond_punishment_", s, "_",
                        ifelse(str_detect(pth, "ru"), "ru", "la"),
                        ".png"),
      height = 8,
      width = 10
    )

  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/diffs_split_cond_punishment_", s, ".png"),
    height = 12,
    width = 10
  )
}

## Pooled Sample ####
### Political ####
pp <-
  prep_plotting("output/ol_cond_pol.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = TRUE)

plt <- pp %>%
  ggplot() +
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
    vars(institution_facet_name),
    rows = vars(condition, opponent),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 17)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none",
         alpha = "none",
          shape = "none"
         )

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond.png"),
       height = 6,
       width = 10
)

### Political: Control ####

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
  filter(condition == "Control",
         institution %in% levels(pp$institution)) %>%
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
  facet_grid(
    # . ~ institution_facet_name,
    cols = vars(institution_facet_name),
    rows = vars(opponent),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
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
  guides(color = "none",
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
    nudge_y = 0.3,
    color = plasma(1, 0.7)
  ) +
  xlim(-0.35, 0.35)
plt

ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond_control.png"),
       height = 4,
       width = 10
)

### Political: Punishment ####

arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  condition = c("Judicial Punishment"),
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
  filter(condition != "Control",
         institution %in% levels(pp$institution)) %>%
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
      # color = condition,
      # shape = opponent,
      alpha = significant
    ),
    color = plasma(1),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_grid(
    # . ~ institution_facet_name,
    cols = vars(institution_facet_name),
    rows = vars(condition, opponent),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8,
    direction = -1
  ) +
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
  guides(color = "none",
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


### Non-political ####
pp <-
  prep_plotting("output/ol_cond_npol.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = TRUE)

plt <- pp %>%
  ggplot() +
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
    vars(institution_facet_name),
    rows = vars(condition),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 17)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none",
         alpha = "none",
         # shape = "none"
  )

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond.png"),
       height = 6,
       width = 10
)

## Pooled Sample with Interaction ####
### Political: Control ####
pp <-
  prep_plotting("output/ol_cond_pol_int.rds",
                output = "diffs",
                model = "condition + opponent + questnnr",
                BF = FALSE,
                PD = TRUE)

plt <- pp %>%
  filter(condition == "Control",
         institution %in% levels(pp$institution)) %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
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
    vars(institution_facet_name),
    rows = vars(questnnr, opponent),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 17, 16)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none",
         alpha = "none",
         shape = "none"
  )

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond_int_control.png"),
       height = 8,
       width = 10
)


### Political: Punishment ####
plt <- pp %>%
  filter(condition != "Control",
         institution %in% levels(pp$institution)) %>%
  mutate(questnnr = str_to_title(questnnr)) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = condition,
      # shape = opponent,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  facet_grid(
    vars(institution_facet_name),
    rows = vars(questnnr, opponent),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8,
    direction = -1
  ) +
  # scale_shape_manual(values = c(15, 17)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
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
         alpha = "none",
         shape = "none"
  )

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond_int_punishment.png"),
       height = 8,
       width = 10
)



### Non-political ####
pp <-
  prep_plotting("output/ol_cond_npol.rds",
                output = "diffs",
                model = "condition + opponent",
                BF = FALSE,
                PD = TRUE)

plt <- pp %>%
  ggplot() +
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
    vars(institution_facet_name),
    rows = vars(condition),
    # ncol = 4
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 17)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|Condition)"
    ),
    shape = "",
    color = "",
    title = ""
  ) +
  geom_vline(
    xintercept = 0,
    alpha = 0.5
  ) +
  guides(color = "none",
         alpha = "none",
         # shape = "none"
  )

plt
ggsave(plt,
       filename = paste0("figs/diffs_pooled_cond.png"),
       height = 6,
       width = 10
)
