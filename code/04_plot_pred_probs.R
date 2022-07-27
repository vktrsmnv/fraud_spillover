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
  mutate(sample = rep(c("full", "acceptable", "only_correct"), 2))

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
                     output = "probs") %>%
      ggplot() +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = condition,
        color = condition
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
      scale_shape_manual(values = c(16, 15, 17, 8)) +
      labs(
        y = "Confidence",
        x = "Probability (Category)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      xlim(0, 0.8)

  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/probs_split_", s, ".png"),
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
  mutate(sample = rep(c("full", "acceptable", "only_correct"), 2))

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
                      output = "probs") %>%
      ggplot() +
      geom_pointrange(aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        shape = condition,
        color = condition
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
      labs(
        y = "Confidence",
        x = "Probability (Category)",
        shape = "",
        color = "",
        title = ifelse(str_detect(pth, "ru"), "Russia", "Latin America")
      ) +
      xlim(0, 0.8)

  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(
    plt,
    filename = paste0("figs/probs_split_npol_", s, ".png"),
    height = 12,
    width = 10
  )
}

## Pooled Sample ####
### Political ####
pp <-
  prep_plotting("output/ol_main_pol.rds",
                output = "probs",
                model = "condition")

plt <- pp %>%
  ggplot() +
      geom_pointrange(
        aes(
          x = median,
          xmin = lower,
          xmax = upper,
          y = category,
          color = condition,
          shape = condition
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
      # geom_vline(
      #   xintercept = 0,
      #   alpha = 0.5
      # ) +
      guides(color = "none", alpha = "none",
             shape = "none")
ggsave(plt,
         filename = paste0("figs/probs_all.png"),
         height = 12,
         width = 10
  )

## #Non-political ####
pp <-
  prep_plotting("output/ol_main_npol.rds",
                output = "probs",
                model = "condition")

plt <- pp %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = condition,
      shape = condition
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
  # geom_vline(
  #   xintercept = 0,
  #   alpha = 0.5
  # ) +
  guides(color = "none", alpha = "none",
         shape = "none")
ggsave(plt,
       filename = paste0("figs/probs_all_npol_", cnd, ".png"),
       height = 12,
       width = 10
)


## Pooled Sample with Questionnaire Interaction ####
### Political ####
pp <-
  prep_plotting("output/ol_main_pol_int.rds",
                output = "probs",
                model = "condition + questnnr")

for (cnd in levels(pp$condition)) {
  lst <- list()
  for (gr in 1:2) {
    temp_n <- length(pp$institution %>% levels()) %/% 2

    if (gr == 1){
      instss <- levels(pp$institution)[1:temp_n]
    }else{
      instss <- levels(pp$institution)[(temp_n + 1):length(levels(pp$institution))]
    }

    lst[[gr]] <- pp %>%
      filter(condition == cnd,
             institution %in% instss) %>%
      mutate(questnnr = str_to_title(questnnr)) %>%
      ggplot() +
      geom_pointrange(
        aes(
          x = median,
          xmin = lower,
          xmax = upper,
          y = category,
          color = questnnr,
        ),
        position = position_dodge(1),
        size = 0.4
      ) +
      facet_grid(
        rows = vars(questnnr),
        cols = vars(institution)
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
          "Probability(Category|",
          cnd,
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
      guides(color = "none", alpha = "none") +
      xlim(0, 0.8)
  }

  plt <- lst[[1]] / lst[[2]] +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(plt,
         filename = paste0("figs/probs_all_int_", cnd, ".png"),
         height = 12,
         width = 10
  )
}


# Conditional Effect: Political Institutions ####
### Full Sample ####
## Russia
### Political ####
model_files <- list.files("output",
                          pattern = "ol_cond_[a-z]+_pol_[0-9]") %>%
  str_split("_|\\.", simplify = T) %>%
  as_tibble() %>%
  mutate(V5 = as.numeric(V5)) %>%
  arrange(V3, -V5) %>%
  select(-V6) %>%
  mutate(sample = rep(c("full",
                        "acceptable",
                        "only_correct"), 2))
#
# for (s in unique(model_files$sample)){
#   pths <- model_files %>%
#     filter(sample == s) %>%
#     mutate(pth = paste(V1, V2, V3, V4, V5, sep = "_")) %>%
#     pull(pth) %>%
#     paste0("output/", ., ".rds")
#
#   lst <- list()
#   for (pth in pths){
#     lst[[str_remove(pth,
#                     "\\.rds")]] <- prep_plotting(
#                       pth,
#                       output = "probs") %>%
#       ggplot() +
pp <- prep_plotting(path = "output/ol_cond_ru_pol_1223.rds",
                          model = "condition + opponent",
                          output = "probs")
lst <- list()
for (gr in 1:2) {
  temp_n <- length(pp$institution %>% levels()) %/% 2

  if (gr == 1) {
    instss <- levels(pp$institution)[1:temp_n]
  } else{
    instss <-
      levels(pp$institution)[(temp_n + 1):length(levels(pp$institution))]
  }

  lst[[gr]] <- pp %>%
    filter(institution %in% instss) %>%
    mutate(opponent = str_to_title(opponent)) %>%
    ggplot() +
    geom_pointrange(
      aes(
        x = median,
        xmin = lower,
        xmax = upper,
        y = category,
        color = Condition,
        shape = opponent
      ),
      position = position_dodge(1),
      size = 0.4
    ) +
    facet_grid(rows = vars(opponent),
               cols = vars(institution),) +
    scale_color_viridis(discrete = T,
                        option = "C",
                        end = 0.8) +
    scale_shape_manual(values = c(16, 17)) +
    labs(
      y = "Confidence",
      x = "Probability(Category)",
      shape = "",
      color = "",
      title = ""
    ) +
    guides(alpha = "none",
           # color = "none",
           shape = "none") +
    scale_alpha_manual(values = c(0.3, 1))
  xlim(0, 0.8)
}

plt <- lst[[1]] / lst[[2]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
plt
ggsave(
  plt,
  filename = paste0("figs/probs_cond_", 1, ".png"),
  height = 12,
  width = 10
)

pp <- prep_plotting(path = "output/ol_cond_ru_pol_667.rds",
                    model = "condition + opponent",
                    output = "probs")
lst <- list()
for (gr in 1:2) {
  temp_n <- length(pp$institution %>% levels()) %/% 2

  if (gr == 1){
    instss <- levels(pp$institution)[1:temp_n]
  }else{
    instss <- levels(pp$institution)[(temp_n + 1):length(levels(pp$institution))]
  }

  lst[[gr]] <- pp %>%
    filter(institution %in% instss) %>%
    mutate(opponent = str_to_title(opponent)) %>%
    ggplot() +
    geom_pointrange(aes(
      x = median,
      xmin = lower,
      xmax = upper,
      y = category,
      color = Condition,
      shape = opponent
    ),
    position = position_dodge(1),
    size = 0.4
    ) +
    facet_grid(
      rows = vars(opponent),
      cols = vars(institution),
    ) +
    scale_color_viridis(
      discrete = T,
      option = "C",
      end = 0.8
    ) +
    scale_shape_manual(values = c(16, 17)) +
    labs(
      y = "Confidence",
      x = "Probability(Category)",
      shape = "",
      color = "",
      title = ""
    ) +
    guides(alpha = "none",
           # color = "none",
           shape = "none") +
    scale_alpha_manual(values = c(0.3, 1))
  xlim(0, 0.8)
}

plt <- lst[[1]] / lst[[2]] +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
ggsave(plt,
       filename = paste0("figs/probs_cond_", 3, ".png"),
       height = 12,
       width = 10
)

sensitivity_to_prior(mpol$pol_inst_CEC)

#
# plotting %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(
#     rows = vars(opponent),
#     cols = vars(institution),
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   guides(alpha = "none",
#          # color = "none",
#          shape = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(
#     # shape = guide_legend(override.aes = list(
#     # shape = c(16, 17)
#   )
# # ))
# plot_ru1
#
#
# plot_ru2 <- plotting %>%
#   filter(!institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#   )))
# plot_ru2
#
#
# pp <- plot_ru1 / plot_ru2 +
#   plot_annotation(title = "Russia") +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# pp
# ggsave(pp,
#   filename = paste0("figs/probs_cond_ru_", 1, ".png"),
#   height = 12,
#   width = 10
# )
#
#
# ## LA
# plotting <- prep_plotting("output/ol_cond_la_pol_847.rds")
#
# plot_la1 <- plotting %>%
#   filter(institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   # facet_grid(rows = vars(opponent), cols = vars(institution),
#   #   # ncol = 4,
#   #   labeller = label_glue(
#   #     "{institution}, N = {n_ru}"
#   #   )
#   # ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#     # labeller = label_glue(
#     #   "{institution}, N = {n_ru}")
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   # scale_y_discrete(
#   #   breaks = 1:4,
#   #   labels = c(
#   #     "None\nat all",
#   #     "Not very\nmuch",
#   #     "Quite\na Lot",
#   #     "A Great\nDeal"
#   #   )
#   # ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#     # color = viridis(2, end = 0.8, option = "C")
#   )))
# # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
# # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
# plot_la1
#
# plot_la2 <- plotting %>%
#   filter(!institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   # facet_grid(rows = vars(opponent), cols = vars(institution),
#   #   # ncol = 4,
#   #   labeller = label_glue(
#   #     "{institution}, N = {n_ru}"
#   #   )
#   # ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#     # labeller = label_glue(
#     #   "{institution}, N = {n_ru}")
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   # scale_y_discrete(
#   #   breaks = 1:4,
#   #   labels = c(
#   #     "None\nat all",
#   #     "Not very\nmuch",
#   #     "Quite\na Lot",
#   #     "A Great\nDeal"
#   #   )
#   # ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#     # color = viridis(2, end = 0.8, option = "C")
#   )))
# # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
# # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
# plot_la2
#
#
# pp <- plot_la1 / plot_la2 +
#   plot_annotation(title = "Latin America") +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# pp
# ggsave(pp,
#   filename = paste0("figs/probs_cond_la_", 1, ".png"),
#   height = 12,
#   width = 10
# )
#
# ### Only Correct Summaries ####
#
# plotting <- prep_plotting("output/ol_cond_ru_pol_667.rds")
#
# plot_ru1 <- plotting %>%
#   filter(institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#   )))
# plot_ru1
#
#
# plot_ru2 <- plotting %>%
#   filter(!institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#   )))
# plot_ru2
#
#
# pp <- plot_ru1 / plot_ru2 +
#   plot_annotation(title = "Russia") +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# pp
# ggsave(pp,
#   filename = paste0("figs/probs_cond_ru_", 3, ".png"),
#   height = 12,
#   width = 10
# )
#
# plotting <- prep_plotting("output/ol_cond_la_pol_376.rds")
# plot_la1 <- plotting %>%
#   filter(institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   # facet_grid(rows = vars(opponent), cols = vars(institution),
#   #   # ncol = 4,
#   #   labeller = label_glue(
#   #     "{institution}, N = {n_ru}"
#   #   )
#   # ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#     # labeller = label_glue(
#     #   "{institution}, N = {n_ru}")
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   # scale_y_discrete(
#   #   breaks = 1:4,
#   #   labels = c(
#   #     "None\nat all",
#   #     "Not very\nmuch",
#   #     "Quite\na Lot",
#   #     "A Great\nDeal"
#   #   )
#   # ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#     # color = viridis(2, end = 0.8, option = "C")
#   )))
# # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
# # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
# plot_la1
#
# plot_la2 <- plotting %>%
#   filter(!institution %in% c(
#     "Central Electoral\nCommission",
#     "Political Parties",
#     "Parliament",
#     "Courts"
#   )) %>%
#   mutate(institution = paste0(institution, "\n N = ", n) %>%
#     fct_inorder(.) %>% fct_rev()) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = median,
#     xmin = lower,
#     xmax = upper,
#     y = category,
#     color = Condition,
#     shape = opponent
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   # facet_grid(rows = vars(opponent), cols = vars(institution),
#   #   # ncol = 4,
#   #   labeller = label_glue(
#   #     "{institution}, N = {n_ru}"
#   #   )
#   # ) +
#   facet_grid(
#     rows = vars(Condition),
#     cols = vars(institution),
#     # labeller = label_glue(
#     #   "{institution}, N = {n_ru}")
#   ) +
#   scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(
#     y = "Confidence",
#     x = "Probability(Category)",
#     shape = "",
#     color = "",
#     title = ""
#   ) +
#   # scale_y_discrete(
#   #   breaks = 1:4,
#   #   labels = c(
#   #     "None\nat all",
#   #     "Not very\nmuch",
#   #     "Quite\na Lot",
#   #     "A Great\nDeal"
#   #   )
#   # ) +
#   guides(alpha = "none", color = "none") +
#   scale_alpha_manual(values = c(0.3, 1)) +
#   guides(shape = guide_legend(override.aes = list(
#     shape = c(16, 17)
#     # color = viridis(2, end = 0.8, option = "C")
#   )))
# # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
# # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
# plot_la2
#
#
# pp <- plot_la1 / plot_la2 +
#   plot_annotation(title = "Latin America") +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# pp
# ggsave(pp,
#   filename = paste0("figs/probs_cond_la_", 3, ".png"),
#   height = 12,
#   width = 10
# )



# Conditional Effect: Political Institutions ####
