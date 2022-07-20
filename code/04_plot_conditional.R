setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "magrittr",
    "sjlabelled",
    "patchwork",
    "styler",
    "here",
    "BayesPostEst",
    "bayesplot",
    "brms",
    "bayestestR",
    "parallel",
    "tidybayes",
    "modelr",
    "remotes",
    "viridis",
    "showtext",
    "ggthemes"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  remotes::install_github("rensa/stickylabeller")
  library(stickylabeller)
  lapply(p_needed, require, character.only = TRUE)
  options(mc.cores = parallel::detectCores())

  # Let's Add a Different Font for Plots!
  font_add_google("Source Sans Pro")
  showtext_auto()

  # setting a global theme for the entire document
  theme_set(theme_bw(base_family = "Source Sans Pro") +
              # theme_bw(base_family = "Source Sans Pro") +
              theme(
                # plot.background = element_blank(),
                plot.title.position = "plot",
                plot.caption.position = "plot",
                legend.title = element_blank(),
                legend.position = "bottom"
              ))
}

setup()

formals(plasma)$end <- 0.8
formals(scale_color_viridis)$end <- 0.8
formals(scale_alpha_manual)$values <- c(0.2, 1)


prep_plotting <- function(mpol_path = "output/ol_cond_ru_pol_1223.rds") {
  mpol <- read_rds(mpol_path)
  pol <- names(mpol)
  plotting <- tibble()

  for (inst in pol) {
    esoph_plot <- mpol$pol_inst_armed$data %>%
      data_grid(condition, opponent) %>%
      add_epred_draws(mpol[[inst]],
                       category = "Trust"
      )
    plot_categories <- tibble(
      median = rep(NA, 12 * 2),
      lower = rep(NA, 12 * 2),
      upper = rep(NA, 12 * 2),
      category = rep(1:4, 3 * 2),
      condition = rep(c(
        "Control",
        "Punishment",
        "Judicial Punishment",
        "Control_Opponent",
        "Punishment_Opponent",
        "Judicial Punishment_Opponent"
      ),
      each = 4
      )
    ) %>%
      as.data.frame()

    plot_categories$n <- nrow(mpol[[inst]]$data)

    # for (cond in plot_categories$condition[1:3]){
    for (num in 1:4) {
      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Control", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "No" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Control" &
      #                           esoph_plot$opponent == "No" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]
      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Control_Opponent", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "Yes" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Control" &
      #                           esoph_plot$opponent == "Yes" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]

      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Punishment", 1:3] <-
          median_hdci(
            esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                                esoph_plot$opponent == "No" &
                                esoph_plot$Trust == num] -
              esoph_plot$.epred[esoph_plot$condition == "Punishment" &
                                  esoph_plot$opponent == "No" &
                                  esoph_plot$Trust == num],
            .width = 0.89
        )[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Punishment_Opponent", 1:3] <-
        median_hdci(
          esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                              esoph_plot$opponent == "Yes" &
                              esoph_plot$Trust == num] -
            esoph_plot$.epred[esoph_plot$condition == "Punishment" &
                                esoph_plot$opponent == "Yes" &
                                esoph_plot$Trust == num],
          .width = 0.89
        )[1:3]

      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Judicial Punishment", 1:3] <-
        median_hdci(
          esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                              esoph_plot$opponent == "No" &
                              esoph_plot$Trust == num] -
            esoph_plot$.epred[esoph_plot$condition == "Judicial Punishment" &
                                esoph_plot$opponent == "No" &
                                esoph_plot$Trust == num],
          .width = 0.89
        )[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Judicial Punishment_Opponent", 1:3] <-
        median_hdci(
          esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                              esoph_plot$opponent == "Yes" &
                              esoph_plot$Trust == num] -
            esoph_plot$.epred[esoph_plot$condition == "Judicial Punishment" &
                                esoph_plot$opponent == "Yes" &
                                esoph_plot$Trust == num],
          .width = 0.89
        )[1:3]
      plot_categories$institution <- inst
    }
    # }
    plotting <- bind_rows(plot_categories, plotting)
  }


  plotting %>%
    mutate(significant = ifelse(lower < 0 & upper > 0, "no", "yes")) %>%
    mutate(
      institution = case_when(
        institution == "pol_inst_pres" ~ "President",
        institution == "pol_inst_police" ~ "Police",
        institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
        institution == "pol_inst_gov" ~ "Government",
        institution == "pol_inst_part" ~ "Political Parties",
        institution == "pol_inst_parl" ~ "Parliament",
        institution == "pol_inst_courts" ~ "Courts",
        institution == "pol_inst_armed" ~ "Armed Forces",
      ),
      institution = as_factor(institution) %>%
        fct_relevel(
          "Central Electoral\nCommission",
          "Political Parties",
          "Parliament",
          "Courts",
          "President",
          "Government",
          "Police",
          "Armed Forces"
        ),
      Condition = str_remove(condition, "_Opponent") %>%
        factor(., levels = c(
          "Control", "Punishment", "Judicial Punishment"
        )),
      condition = condition %>%
        factor(
          .,
          levels = c(
            "Judicial Punishment",
            "Judicial Punishment_Opponent",
            "Punishment",
            "Punishment_Opponent",
            "Control",
            "Control_Opponent"
          )
        ),
      opponent = ifelse(
        str_detect(condition, pattern = "Opponent"),
        "Opponent",
        "Supporter"
      ),
      significant_opponent = paste0(significant, "_", opponent)
    ) %>%
    filter(!str_detect("Control", string = condition)) -> plotting

  return(plotting)
}

## Political Institutions ####
### All Cases #####
## Russia
plotting <- prep_plotting("output/ol_cond_ru_pol_1223.rds")
n_ru <- plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)

# arrows for annotation on the plots
arrows <- data.frame(
  # x1 = 0.19, x2 = 0.1,
  # y1 = 3.1, y2 = 1.4,
  x1 = c(0.16, -0.15), x2 = c(0.07, -0.07),
  y1 = c(3.3, 3.3), y2 = c(3.4, 3.4),
  condition = c(
    "Control", "Control"
  ),
  significant_opponent = c("no_Opponent", "no_Opponent"),
  institution = c("Central Electoral\nCommission", "Central Electoral\nCommission")
) %>%
  mutate(institution = as_factor(institution) %>%
           fct_expand(
             "Central Electoral\nCommission",
             "Political Parties",
             "Parliament",
             "Courts",
             "President",
             "Government",
             "Police",
             "Armed Forces"
           ))


plot_ru <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  # xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(~institution,
             ncol = 4,
             labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )
plot_ru

# Latin America
plotting <- prep_plotting("output/ol_cond_la_pol_881.rds")

plot_la <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Latin America",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(
    ~ institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
       filename = paste0("figs/cond_hdi89_", 1, ".png"),
       height = 10,
       width = 10)

### Exclude Response = 3 #####
## Russia
plotting <- prep_plotting("output/ol_cond_ru_pol_1203.rds")
n_ru <- plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)

# arrows for annotation on the plots
arrows <- data.frame(
  # x1 = 0.19, x2 = 0.1,
  # y1 = 3.1, y2 = 1.4,
  x1 = c(0.16, -0.15), x2 = c(0.07, -0.07),
  y1 = c(3.3, 3.3), y2 = c(3.4, 3.4),
  condition = c(
    "Control", "Control"
  ),
  significant_opponent = c("no_Opponent", "no_Opponent"),
  institution = c("Central Electoral\nCommission", "Central Electoral\nCommission")
) %>%
  mutate(institution = as_factor(institution) %>%
           fct_expand(
             "Central Electoral\nCommission",
             "Political Parties",
             "Parliament",
             "Courts",
             "President",
             "Government",
             "Police",
             "Armed Forces"
           ))


plot_ru <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(~institution,
             ncol = 4,
             labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )


# Latin America
plotting <- prep_plotting("output/ol_cond_la_pol_854.rds")

plot_la <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Latin America",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(
    ~ institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
       filename = paste0("figs/cond_hdi89_", 2, ".png"),
       height = 10,
       width = 10)

### Exclude Response = 3 & 2 #####
## Russia
plotting <- prep_plotting("output/ol_cond_ru_pol_1191.rds")
n_ru <- plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)

# arrows for annotation on the plots
arrows <- data.frame(
  # x1 = 0.19, x2 = 0.1,
  # y1 = 3.1, y2 = 1.4,
  x1 = c(0.16, -0.15), x2 = c(0.07, -0.07),
  y1 = c(3.3, 3.3), y2 = c(3.4, 3.4),
  condition = c(
    "Control", "Control"
  ),
  significant_opponent = c("no_Opponent", "no_Opponent"),
  institution = c("Central Electoral\nCommission", "Central Electoral\nCommission")
) %>%
  mutate(institution = as_factor(institution) %>%
           fct_expand(
             "Central Electoral\nCommission",
             "Political Parties",
             "Parliament",
             "Courts",
             "President",
             "Government",
             "Police",
             "Armed Forces"
           ))


plot_ru <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(~institution,
             ncol = 4,
             labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )


# Latin America
plotting <- prep_plotting("output/ol_cond_la_pol_851.rds")

plot_la <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Latin America",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  xlim(-0.3, 0.3) +
  guides(alpha = "none") +
  facet_wrap(
    ~ institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T, option = "C", direction = -1,
    begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
       filename = paste0("figs/cond_hdi89_", 3, ".png"),
       height = 10,
       width = 10)


prep_plotting <- function(mpol_path = "output/ol_cond_ru_pol_1223.rds") {
  mpol <- read_rds(mpol_path)
  pol <- names(mpol)
  plotting <- tibble()

  for (inst in pol) {
    esoph_plot <- mpol$pol_inst_armed$data %>%
      data_grid(condition, opponent) %>%
      add_epred_draws(mpol[[inst]],
                      category = "Trust"
      )
    plot_categories <- tibble(
      median = rep(NA, 12 * 2),
      lower = rep(NA, 12 * 2),
      upper = rep(NA, 12 * 2),
      category = rep(1:4, 3 * 2),
      condition = rep(c(
        "Control",
        "Punishment",
        "Judicial Punishment",
        "Control_Opponent",
        "Punishment_Opponent",
        "Judicial Punishment_Opponent"
      ),
      each = 4
      )
    ) %>%
      as.data.frame()

    plot_categories$n <- nrow(mpol[[inst]]$data)

    # for (cond in plot_categories$condition[1:3]){
    for (num in 1:4) {
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Control", 1:3] <-
        median_hdci(
          esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                              esoph_plot$opponent == "No" &
                              esoph_plot$Trust == num] -
            esoph_plot$.epred[esoph_plot$condition == "Control" &
                                esoph_plot$opponent == "No" &
                                esoph_plot$Trust == num],
          .width = 0.89
        )[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Control_Opponent", 1:3] <-
        median_hdci(
          esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                              esoph_plot$opponent == "Yes" &
                              esoph_plot$Trust == num] -
            esoph_plot$.epred[esoph_plot$condition == "Control" &
                                esoph_plot$opponent == "Yes" &
                                esoph_plot$Trust == num],
          .width = 0.89
        )[1:3]

      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Punishment", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "No" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Punishment" &
      #                           esoph_plot$opponent == "No" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]
      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Punishment_Opponent", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "Yes" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Punishment" &
      #                           esoph_plot$opponent == "Yes" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]
      #
      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Judicial Punishment", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "No" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Judicial Punishment" &
      #                           esoph_plot$opponent == "No" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]
      # plot_categories[plot_categories$category == num &
      #                   plot_categories$condition == "Judicial Punishment_Opponent", 1:3] <-
      #   median_hdci(
      #     esoph_plot$.epred[esoph_plot$condition == "Fraud" &
      #                         esoph_plot$opponent == "Yes" &
      #                         esoph_plot$Trust == num] -
      #       esoph_plot$.epred[esoph_plot$condition == "Judicial Punishment" &
      #                           esoph_plot$opponent == "Yes" &
      #                           esoph_plot$Trust == num],
      #     .width = 0.89
      #   )[1:3]
      plot_categories$institution <- inst
    }
    # }
    plotting <- bind_rows(plot_categories, plotting)
  }


  plotting %>%
    mutate(significant = ifelse(lower < 0 & upper > 0, "no", "yes")) %>%
    mutate(
      institution = case_when(
        institution == "pol_inst_pres" ~ "President",
        institution == "pol_inst_police" ~ "Police",
        institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
        institution == "pol_inst_gov" ~ "Government",
        institution == "pol_inst_part" ~ "Political Parties",
        institution == "pol_inst_parl" ~ "Parliament",
        institution == "pol_inst_courts" ~ "Courts",
        institution == "pol_inst_armed" ~ "Armed Forces",
      ),
      institution = as_factor(institution) %>%
        fct_relevel(
          "Central Electoral\nCommission",
          "Political Parties",
          "Parliament",
          "Courts",
          "President",
          "Government",
          "Police",
          "Armed Forces"
        ),
      Condition = str_remove(condition, "_Opponent") %>%
        factor(., levels = c(
          "Control", "Punishment", "Judicial Punishment"
        )),
      condition = condition %>%
        factor(
          .,
          levels = c(
            "Judicial Punishment",
            "Judicial Punishment_Opponent",
            "Punishment",
            "Punishment_Opponent",
            "Control",
            "Control_Opponent"
          )
        ),
      opponent = ifelse(
        str_detect(condition, pattern = "Opponent"),
        "Opponent",
        "Supporter"
      ),
      significant_opponent = paste0(significant, "_", opponent)
    ) %>%
    filter(str_detect("Control", string = condition)) -> plotting

  return(plotting)
}

## Political Institutions ####
### All Cases #####
## Russia
plotting <- prep_plotting("output/ol_cond_ru_pol_1223.rds")
n_ru <- plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)

# arrows for annotation on the plots
arrows <- data.frame(
  # x1 = 0.19, x2 = 0.1,
  # y1 = 3.1, y2 = 1.4,
  x1 = c(0.16, -0.15), x2 = c(0.07, -0.07),
  y1 = c(3.3, 3.3), y2 = c(3.4, 3.4),
  condition = c(
    "Control", "Control"
  ),
  significant_opponent = c("no_Opponent", "no_Opponent"),
  institution = c("Central Electoral\nCommission", "Central Electoral\nCommission")
) %>%
  mutate(institution = as_factor(institution) %>%
           fct_expand(
             "Central Electoral\nCommission",
             "Political Parties",
             "Parliament",
             "Courts",
             "President",
             "Government",
             "Police",
             "Armed Forces"
           ))


plot_ru <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  # xlim(-0.3, 0.3) +
  guides(alpha = "none", color = "none") +
  facet_wrap(~institution,
             ncol = 4,
             labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T, option = "C", #direction = -1,
    # begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )
plot_ru

# Latin America
plotting <- prep_plotting("output/ol_cond_la_pol_872.rds")

plot_la <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Latin America",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  # xlim(-0.3, 0.3) +
  guides(alpha = "none", color = "none") +
  scale_color_viridis(
    discrete = T, option = "C", #direction = -1,
    # begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )
  facet_wrap(
    ~ institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)}"
    )
  )

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
       filename = paste0("figs/cond_hdi89_", 4, ".png"),
       height = 10,
       width = 10)


### Only Correct Summaries #####
## Russia
plotting <- prep_plotting("output/ol_cond_ru_pol_667.rds")
n_ru <- plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)

# arrows for annotation on the plots
arrows <- data.frame(
  # x1 = 0.19, x2 = 0.1,
  # y1 = 3.1, y2 = 1.4,
  x1 = c(0.16, -0.15), x2 = c(0.07, -0.07),
  y1 = c(3.3, 3.3), y2 = c(3.4, 3.4),
  condition = c(
    "Control", "Control"
  ),
  significant_opponent = c("no_Opponent", "no_Opponent"),
  institution = c("Central Electoral\nCommission", "Central Electoral\nCommission")
) %>%
  mutate(institution = as_factor(institution) %>%
           fct_expand(
             "Central Electoral\nCommission",
             "Political Parties",
             "Parliament",
             "Courts",
             "President",
             "Government",
             "Police",
             "Armed Forces"
           ))


plot_ru <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Russia",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  # xlim(-0.3, 0.3) +
  guides(alpha = "none", color = "none") +
  facet_wrap(~institution,
             ncol = 4,
             labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T, option = "C", #direction = -1,
    # begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )
plot_ru

# Latin America
plotting <- prep_plotting("output/ol_cond_la_pol_376.rds")

plot_la <- plotting %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = condition
  )) +
  labs(
    # title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    # subtitle = paste0(
    #   "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    # ),
    title = "Latin America",
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "Confidence",
    shape = ""
  ) +
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
    color = "black",
    alpha = 0.7
  ) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  # scale_shape_manual(
  #   values = c(0, 2, 15, 17),
  #   labels = c("Opponent", "Supporter", "", "")
  # ) +
  scale_shape_manual(
    values = c(15, 17),
    labels = c("Opponent", "Supporter")
  ) +
  scale_alpha_manual(values = c(0.4, 1)) +
  geom_vline(aes(xintercept = 0), alpha = 0.33) +
  # xlim(-0.3, 0.3) +
  guides(alpha = "none", color = "none") +
  scale_color_viridis(
    discrete = T, option = "C", #direction = -1,
    # begin = 0.27 # if control category omitted
  ) +
  geom_curve(
    data = arrows[1, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = -0.2
  ) +
  geom_curve(
    data = arrows[2, ],
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.2,
    color = "black",
    alpha = 0.7,
    curvature = 0.2
  ) +
  geom_text(
    data = data.frame(
      median = c(0.2, -0.2),
      category = c(3.5, 3.5),
      condition = c(
        "Control"
      ),
      significant_opponent = "no_Opponent",
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(institution = as_factor(institution) %>%
               fct_expand(
                 "Central Electoral\nCommission",
                 "Political Parties",
                 "Parliament",
                 "Courts",
                 "President",
                 "Government",
                 "Police",
                 "Armed Forces"
               )),
    label = c(
      "Punishment\ndecreases\ntrust",
      "Punishnment\nincreases\ntrust"
    ),
    size = 3,
    color = "black",
    alpha = 0.7
  )
facet_wrap(
  ~ institution,
  ncol = 4,
  labeller = label_glue(
    "{institution}, N = {plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)}"
  )
)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
       filename = paste0("figs/cond_hdi89_", 5, ".png"),
       height = 10,
       width = 10)
