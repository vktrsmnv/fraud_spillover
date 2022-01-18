# Plot for OL analysis 

# LA: Full Sample ####

mpol <- read_rds("output/ol_la_condition.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num
                                       ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: LA, depicted are 89% HDI, no copy-paste or incomplete cases"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/la_hdi89_full.png",
       height = 8,
       width = 10)

# LA: No Copy Paste or Incomplete ####


mpol <- read_rds("output/ol_la_condition_234.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
      median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                      esoph_plot$Trust == num
      ] -
        esoph_plot$.value[esoph_plot$condition == cond &
                            esoph_plot$Trust == num
        ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}
# 
# plotting <- plotting %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President",
#         TRUE ~ institution
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       ))

plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                     x = median,
                     group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: LA, depicted are 89% HDI"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/la_hdi89_no_copy_paste_incomplete.png",
       height = 8,
       width = 10)

# LA: No Copy Paste ######
# 
mpol <- read_rds("output/ol_la_condition_2.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num
                                       ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: LA, depicted are 89% HDI, no copy-paste or incomplete cases"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/la_hdi89_no_copy_paste.png",
       height = 8,
       width = 10)

# RU: No Copy Paste & Incomplete ####
# 
mpol <- read_rds("output/ol_ru_condition_234.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num
                                       ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}


plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: RU, depicted are 89% HDI, no copy-paste or incomplete cases"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/ru_hdi89_no_copy_paste_incomplete.png",
       height = 8,
       width = 10)

# RU: Full Sample ####

mpol <- read_rds("output/ol_rus_condition.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num
                                       ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: RU, depicted are 89% HDI"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/ru_hdi89_full.png",
       height = 8,
       width = 10)


# RU: No Copy Paste ####

mpol <- read_rds("output/ol_ru_condition_2.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num
                                       ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = paste0("Sample: RU, depicted are 89% HDI, no copy-paste cases"),
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4,
             labeller = label_glue('{pol}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot


ggsave(plot, 
       filename = "figs/ru_hdi89_no_copy_paste.png",
       height = 8,
       width = 10)








plotting %>%
  filter(str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: LA, depicted are 89% HDI",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 3) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "figs/la_npol_hdi89.png",
       height = 6,
       width = 10)

#### RUS##### 

mpol <- read_rds("output/ol_rus_condition.rds")
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12),
    lower = rep(NA, 12),
    upper = rep(NA, 12),
    category = rep(1:4, 3),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <- 
        # quantile(esoph_plot$.value[esoph_plot$condition == "Fraud" &
        #                              esoph_plot$Trust == num 
        # ] -
        #   esoph_plot$.value[esoph_plot$condition == cond &
        #                       esoph_plot$Trust == num 
        #   ], 
        # c(0.05, 0.5, 0.95))
        # hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
        #                              esoph_plot$Trust == num
        # ] -
        #   esoph_plot$.value[esoph_plot$condition == cond &
      #                       esoph_plot$Trust == num
      #   ], .width = 0.89)
      median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                     esoph_plot$Trust == num
      ] -
        esoph_plot$.value[esoph_plot$condition == cond &
                            esoph_plot$Trust == num
        ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}
# 
# plotting <- plotting %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President",
#         TRUE ~ institution
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       ))

plotting
plotting %>%
  filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: RU, depicted are 89% HDI",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 4) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "figs/ru_pol_hdi89.png",
       height = 6,
       width = 10)

plotting %>%
  filter(str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: RU, depicted are 89% HDI",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 3) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "figs/ru_npol_hdi89.png",
       height = 6,
       width = 10)

##### merged conditions####

mpol <- read_rds("output/ol_la_condition1.rds")
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition1) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 8),
    lower = rep(NA, 8),
    upper = rep(NA, 8),
    category = rep(1:4, 2),
    condition1 = rep(c(
      "Control",
      "Punishment"
    ),
    each = 4)
  ) %>%
    as.data.frame()
  
  for (cond in levels(mpol$pol_inst_armed$data$condition1)[2:3]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition1 == cond, 1:3] <- 
        # quantile(esoph_plot$.value[esoph_plot$condition == "Fraud" &
        #                              esoph_plot$Trust == num 
        # ] -
        #   esoph_plot$.value[esoph_plot$condition == cond &
        #                       esoph_plot$Trust == num 
        #   ], 
        # c(0.05, 0.5, 0.95))
        # hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
        #                              esoph_plot$Trust == num
        # ] -
        #   esoph_plot$.value[esoph_plot$condition == cond &
      #                       esoph_plot$Trust == num
      #   ], .width = 0.89)
      median_hdi(esoph_plot$.value[esoph_plot$condition1 == "Fraud" &
                                     esoph_plot$Trust == num
      ] -
        esoph_plot$.value[esoph_plot$condition1 == cond &
                            esoph_plot$Trust == num
        ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}
# 
# plotting <- plotting %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President",
#         TRUE ~ institution
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       ))

plotting
plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition1)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition1),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: LA, depicted are 89% HDI",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 3) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "figs/la_hdi89.png",
       height = 8,
       width = 10)

plotting %>%
  filter(str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category, 
                x = median,
                group = condition1)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition1),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: LA, depicted are 89% HDI",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             ncol = 3) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "figs/la_npol1_hdi89.png",
       height = 6,
       width = 10)


