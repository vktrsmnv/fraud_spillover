
# different sample sizes
number <- c(1226, 1203)

## Main Effects Figure

for (n in number){
### Political Institutions #####
mpol <- read_rds(paste0("output/ol_main_ru_pol_", n, ".rds"))
# mpol <- read_rds("output/ol_main_ru_pol_no_exclusions.rds")
n_cases_ru <- c()
pol <- names(mpol)
plotting_ru <- tibble()
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

  n_cases_ru <- c(n_cases_ru, nrow(mpol[[inst]]$data))

  for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
    for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == cond, 1:3] <-
        median_hdci(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                        esoph_plot$Trust == num
        ] %>% as.numeric() -
          esoph_plot$.value[esoph_plot$condition == cond &
                              esoph_plot$Trust == num
          ] %>% as.numeric(), .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting_ru <- bind_rows(plot_categories, plotting_ru)
}


#### Plot for Control vs. Fraud conditions alone

plotting_ru %>%
  mutate(significant = ifelse(lower < 0 &
                                upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "pol_inst_pres" ~ "President",
      institution == "pol_inst_police" ~ "Police",
      institution == "pol_inst_CEC" ~ "Central Electoral Commission",
      institution == "pol_inst_gov" ~ "Government",
      institution == "pol_inst_part" ~ "Political Parties",
      institution == "pol_inst_parl" ~ "Parliament",
      institution == "pol_inst_courts" ~ "Courts",
      institution == "pol_inst_armed" ~ "Armed Forces",
    ),
    Condition = condition %>%
      factor(., levels = c(
        "Control", "Punishment", "Judicial Punishment"
      ))
  ) %>%
  # filter(!str_detect(institution, "npol"))%>%
  filter(str_detect(string = condition, "Control")) %>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)) +
  theme_bw() +
  ggthemes::theme_base() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption.position =  "plot"
  ) +
  labs(
    # title = "Effect of Fraud Information on Confidence in Political Institutions in Russia",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Control)",
    y = "Confidence",
    color = "",
    alpha = ""
  ) +
  guides(alpha = "none", color = "none") +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap( ~ institution,
              ncol = 4,
              labeller = label_glue('{institution}, N = {n_cases_ru}')) +
  scale_color_viridis(discrete = T,
                      option = "C",
                      end = 0.8) -> plot_ru

# plot_ru
arrows <- data.frame(x1 = 0.19, x2 = 0.1,
                     y1 = 1.55, y2 = 1.2,
                     Condition =  c(
                       "Control"
                     ),
                     institution = "Armed Forces")
plot_ru <- plot_ru +
  xlim(-0.3, 0.3) +
  geom_abline(intercept = 2.5,
              slope = -15,
              size = 0.5,
              alpha = 0.3,
              linetype = 2,
              color = plasma(1)) +
  geom_text(data = data.frame(median = 0.2,
                              category = 1.7,
                              Condition =  c(
                                "Control"
                              ),
                              institution = "Armed Forces"),
            label = "fraud\ndecreases\ntrust",
            size = 2.7,
            nudge_y = 0.3,
            color = plasma(1, 0.7)) +
  geom_curve(
    data = arrows,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    alpha = 0.3, color = plasma(1),
    curvature = -0.3)

plot_ru

# load the estimation results from main model
mpol <- read_rds(paste0("output/ol_main_la_pol_", n, ".rds"))

# extract the posterior samples
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
        median_hdci(esoph_plot$.value[esoph_plot$condition == "Fraud" &
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

#### Main Effect: Fraud vs. Control

plotting %>%
  mutate(significant = ifelse(lower < 0 &
                                upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "pol_inst_pres" ~ "President",
      institution == "pol_inst_police" ~ "Police",
      institution == "pol_inst_CEC" ~ "Central Electoral Commission",
      institution == "pol_inst_gov" ~ "Government",
      institution == "pol_inst_part" ~ "Political Parties",
      institution == "pol_inst_parl" ~ "Parliament",
      institution == "pol_inst_courts" ~ "Courts",
      institution == "pol_inst_armed" ~ "Armed Forces",
    ),
    Condition = condition %>%
      factor(., levels = c(
        "Control", "Punishment", "Judicial Punishment"
      ))
  ) %>%
  filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)) +
  labs(
    title = "Latin America",
    # title = "Effect of Fraud Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    x = "",
    # x = "Pr(Category|Fraud) - Pr(Category|Control)",
    y = "Confidence",
    color = "",
    alpha = ""
  ) +
  guides(alpha = "none", color = "none") +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap( ~ institution,
              ncol = 4,
              labeller = label_glue('{institution}, N = {n_cases}')) +
  scale_color_viridis(discrete = T,
                      option = "C",
                      end = 0.8) +
  xlim(-0.3, 0.3) +
  geom_abline(intercept = 2.5,
              slope = -15,
              size = 0.5,
              alpha = 0.3,
              linetype = 2,
              color = plasma(1)) +
  geom_text(data = data.frame(median = 0.2,
                              category = 1.7,
                              Condition =  c(
                                "Control"
                              ),
                              institution = "Armed Forces"),
            label = "fraud\ndecreases\ntrust",
            size = 2.7,nudge_y = 0.25,
            color = plasma(1, 0.7)) +
  geom_curve(
    data = arrows,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    alpha = 0.3, color = plasma(1),
    curvature = -0.3) -> plot_la

pp <- plot_la / plot_ru +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

ggsave(pp,
       filename = paste0("figs/main_hdi89_", n, ".png"),
       height = 8,
       width = 10)
}
