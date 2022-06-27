# Plot for OL analysis
# install.packages(c("StanHeaders", "rstan"))
setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "magrittr",
    "sjlabelled",
    "styler",
    "here",
    "BayesPostEst",
    "bayesplot",
    "brms",
    "bayestestR",
    "parallel",
    "tidybayes",
    "modelr",
    "remotes"
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
}
setup()

# LA: Full Sample ####

### Political Institutions #####

mpol <- read_rds("output/ol_main_la_pol.rds")
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
  mutate(institution = case_when(institution == "pol_inst_pres" ~ "President",
                   institution == "pol_inst_police" ~ "Police",
                   institution == "pol_inst_CEC" ~ "Central Electoral Commission",
                   institution == "pol_inst_gov" ~ "Government",
                   institution == "pol_inst_part" ~ "Political Parties",
                   institution == "pol_inst_parl" ~ "Parliament",
                   institution == "pol_inst_courts" ~ "Courts",
                   institution == "pol_inst_armed" ~ "Armed Forces",
                   ),
         # Condition = condition,
         Condition = condition %>%
           factor(., levels = c(
             "Control", "Punishment", "Judicial Punishment"
           ))) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
             labeller = label_glue('{institution}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/la_hdi89.png",
       height = 8,
       width = 10)

names(mpol) <- case_when(names(mpol) == "pol_inst_pres" ~ "President",
                         names(mpol) == "pol_inst_police" ~ "Police",
                         names(mpol) == "pol_inst_CEC" ~ "Central EC",
                         names(mpol) == "pol_inst_gov" ~ "Government",
                         names(mpol) == "pol_inst_part" ~ "Political Parties",
                         names(mpol) == "pol_inst_parl" ~ "Parliament",
                         names(mpol) == "pol_inst_courts" ~ "Courts",
                         names(mpol) == "pol_inst_armed" ~ "Armed Forces",
)

BayesPostEst::mcmcReg(mpol[1:4], ci = 0.89,
                      pars = "b",
                      regex = T,
                      custom.gof.rows = list("Observations" = n_cases[1:4]),
                      custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                      and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                      caption = "Ordinal logistic regression results for Latin American sample",
                      custom.model.names = names(mpol)[1:4],
                      coefnames = rep(list(c("Intercept$_1$",
                                             "Intercept$_2$",
                                             "Intercept$_3$",
                                             "Control",
                                             "Punishment",
                                             "Judicial Punishment"
                                             )), 4),
                      float.pos = "h",
                      threeparttable = TRUE,
                      file = "tables/la_pol1.tex")

BayesPostEst::mcmcReg(mpol[5:8], ci = 0.89,
                      pars = "b",
                      regex = T,
                      custom.gof.rows = list("Observations" = n_cases[5:8]),
                      custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                      and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                      caption = "Ordinal logistic regression results for Latin American sample",
                      custom.model.names = names(mpol)[5:8],
                      coefnames = rep(list(c("Intercept$_1$",
                                             "Intercept$_2$",
                                             "Intercept$_3$",
                                             "Control",
                                             "Punishment",
                                             "Judicial Punishment"
                      )), 4),
                      float.pos = "h",
                      threeparttable = TRUE,
                      file = "tables/la_pol2.tex")

### Non-political Institutions #####
mpol <- read_rds("output/ol_main_la_npol.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$npol_inst_comp$data %>%
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

  for (cond in levels(mpol$npol_inst_comp$data$condition)[2:4]){
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
  mutate(institution = case_when(institution == "npol_inst_comp" ~ "Companies",
                                 institution == "npol_inst_banks" ~ "Banks",
                                 institution == "npol_inst_env" ~ "Environmental Organizations",
                                 institution == "npol_inst_UN" ~ "United Nations",
                                 institution == "npol_inst_WB" ~ "World Bank",
                                 institution == "npol_inst_WTO" ~ "WTO",
  ),
  Condition = condition %>%
    factor(., levels = c(
      "Control", "Punishment", "Judicial Punishment"))
) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Non-political Institutions in Latin America",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
             ncol = 3,
             labeller = label_glue('{institution}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/la_hdi89_npol.png",
       height = 8,
       width = 10)

names(mpol) <- case_when(names(mpol) == "npol_inst_comp" ~ "Companies",
                         names(mpol) == "npol_inst_banks" ~ "Banks",
                         names(mpol) == "npol_inst_env" ~ "Environmental Organizations",
                         names(mpol) == "npol_inst_UN" ~ "United Nations",
                         names(mpol) == "npol_inst_WB" ~ "World Bank",
                         names(mpol) == "npol_inst_WTO" ~ "WTO",
)

BayesPostEst::mcmcReg(mpol, ci = 0.89,
                      pars = "b",
                      regex = T,
                      float.pos = "h",
                      custom.gof.rows = list("Observations" = n_cases),
                      custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                      and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                      caption = "Ordinal logistic regression results for Latin American sample",
                      custom.model.names = names(mpol),
                      coefnames = rep(list(c("Intercept$_1$",
                                             "Intercept$_2$",
                                             "Intercept$_3$",
                                             "Control",
                                             "Punishment",
                                             "Judicial Punishment"
                      )),
                      length(names(mpol))),
                      threeparttable = TRUE,
                      file = "tables/la_npol.tex")

# RU: Full Sample ####

### Political Institutions #####
mpol <- read_rds("output/ol_main_ru_pol.rds")
# mpol <- read_rds("output/ol_main_ru_pol_no_exclusions.rds")
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
        ] %>% as.numeric() -
          esoph_plot$.value[esoph_plot$condition == cond &
                              esoph_plot$Trust == num
          ] %>% as.numeric(), .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  mutate(institution = case_when(institution == "pol_inst_pres" ~ "President",
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
      "Control", "Punishment", "Judicial Punishment"))) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Russia",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
             labeller = label_glue('{institution}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/ru_hdi89.png",
       height = 8,
       width = 10)

names(mpol) <- case_when(names(mpol) == "pol_inst_pres" ~ "President",
                         names(mpol) == "pol_inst_police" ~ "Police",
                         names(mpol) == "pol_inst_CEC" ~ "Central EC",
                         names(mpol) == "pol_inst_gov" ~ "Government",
                         names(mpol) == "pol_inst_part" ~ "Political Parties",
                         names(mpol) == "pol_inst_parl" ~ "Parliament",
                         names(mpol) == "pol_inst_courts" ~ "Courts",
                         names(mpol) == "pol_inst_armed" ~ "Armed Forces",
)

BayesPostEst::mcmcReg(
  mpol[1:4],
  ci = 0.89,
  pars = "b",
  regex = T,
  float.pos = "h",
  custom.gof.rows = list("Observations" = n_cases[1:4]),
  custom.note = "",
  caption = "",
  custom.model.names = names(mpol)[1:4],
  coefnames = rep(list(
    c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ),
  threeparttable = TRUE,
  length(names(mpol)[1:4])),
  file = "tables/ru_pol1.tex"
)

BayesPostEst::mcmcReg(
  mpol[5:8],
  ci = 0.89,
  pars = "b",
  regex = T,
  threeparttable = TRUE,
  float.pos = "h",
  custom.gof.rows = list("Observations" = n_cases[5:8]),
  custom.note = "%stars.
  \\\\\nThis table examines the effect of information about fraud and punitive measures on institutional trust.
  \\\\\nFraud treatment group serves as the baseline.",
  caption = "Ordinal logistic regression results for Russian sample",
  custom.model.names = names(mpol)[5:8],
  coefnames = rep(list(
    c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ),
  length(names(mpol)[5:8])),
  file = "tables/ru_pol2.tex"
)

### Non-political Institutions #####
mpol <- read_rds("output/ol_main_ru_npol.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$npol_inst_comp$data %>%
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

  for (cond in levels(mpol$npol_inst_comp$data$condition)[2:4]){
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
  mutate(institution = case_when(institution == "npol_inst_comp" ~ "Companies",
                                 institution == "npol_inst_banks" ~ "Banks",
                                 institution == "npol_inst_env" ~ "Environmental Organizations",
                                 institution == "npol_inst_UN" ~ "United Nations",
                                 institution == "npol_inst_WB" ~ "World Bank",
                                 institution == "npol_inst_WTO" ~ "WTO",
  ),
  Condition = condition %>%
    factor(., levels = c(
      "Control", "Punishment", "Judicial Punishment"))) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Non-political Institutions in Russia",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
             ncol = 3,
             labeller = label_glue('{institution}, N = {n_cases}')) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/ru_hdi89_npol.png",
       height = 8,
       width = 10)

names(mpol) <- case_when(names(mpol) == "npol_inst_comp" ~ "Companies",
                         names(mpol) == "npol_inst_banks" ~ "Banks",
                         names(mpol) == "npol_inst_env" ~ "Environmental Organizations",
                         names(mpol) == "npol_inst_UN" ~ "United Nations",
                         names(mpol) == "npol_inst_WB" ~ "World Bank",
                         names(mpol) == "npol_inst_WTO" ~ "WTO",
)

BayesPostEst::mcmcReg(
  mpol,
  ci = 0.89,
  pars = "b",
  regex = T,
  float.pos = "h",
  custom.gof.rows = list("Observations" = n_cases),
  custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                      and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
  caption = "Ordinal logistic regression results for Russian sample",
  custom.model.names = names(mpol),
  coefnames = rep(list(
    c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ),
  length(names(mpol))),
  threeparttable = TRUE,
  file = "tables/ru_npol.tex"
)

# LA: Conditional Effects ####

mpol <- read_rds("output/ol_conditional_la_pol.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(fraud, punishment, judicial_punishment, opponent) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12*2),
    lower = rep(NA, 12*2),
    upper = rep(NA, 12*2),
    category = rep(1:4, 3*2),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Control_Opponent",
      "Punishment_Opponent",
      "Judicial Punishment_Opponent"
    ),
    each = 4)
  ) %>%
    as.data.frame()

  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))

  # for (cond in plot_categories$condition[1:3]){
  for (num in 1:4){
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Control", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 0 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Control_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 0 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]

    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Punishment", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Punishment_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]

    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Judicial Punishment", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 1 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Judicial Punishment_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 1 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories$institution <- inst
  }
  # }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
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
    )
  ) %>%
  ggplot(., aes(
    y = category,
    x = median,
    shape = opponent,
    group = condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent
  ),
  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Latin America",
    subtitle = paste0(
      "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    ),
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "",
    shape = ""
  ) +
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
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/la_hdi89_conditional.png",
       height = 8,
       width = 10)

# RU: Conditional Effects ####

mpol <- read_rds("output/ol_conditional_ru_pol.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(fraud, punishment, judicial_punishment, opponent) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12*2),
    lower = rep(NA, 12*2),
    upper = rep(NA, 12*2),
    category = rep(1:4, 3*2),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Control_Opponent",
      "Punishment_Opponent",
      "Judicial Punishment_Opponent"
    ),
    each = 4)
  ) %>%
    as.data.frame()

  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))

  # for (cond in plot_categories$condition[1:3]){
  for (num in 1:4){
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Control", 1:3] <-
        median_hdi(
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num] -
            esoph_plot$.value[esoph_plot$fraud == 0 &
                                esoph_plot$punishment == 0 &
                                esoph_plot$judicial_punishment == 0 &
                                esoph_plot$opponent == 0 &
                                esoph_plot$Trust == num],
          .width = 0.89)[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Control_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 0 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]

      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Punishment", 1:3] <-
        median_hdi(
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num] -
            esoph_plot$.value[esoph_plot$fraud == 1 &
                                esoph_plot$punishment == 1 &
                                esoph_plot$judicial_punishment == 0 &
                                esoph_plot$opponent == 0 &
                                esoph_plot$Trust == num],
          .width = 0.89)[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Punishment_Opponent", 1:3] <-
        median_hdi(
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num] -
            esoph_plot$.value[esoph_plot$fraud == 1 &
                                esoph_plot$punishment == 1 &
                                esoph_plot$judicial_punishment == 0 &
                                esoph_plot$opponent == 1 &
                                esoph_plot$Trust == num],
          .width = 0.89)[1:3]

      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Judicial Punishment", 1:3] <-
        median_hdi(
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num] -
            esoph_plot$.value[esoph_plot$fraud == 1 &
                                esoph_plot$punishment == 1 &
                                esoph_plot$judicial_punishment == 1 &
                                esoph_plot$opponent == 0 &
                                esoph_plot$Trust == num],
          .width = 0.89)[1:3]
      plot_categories[plot_categories$category == num &
                        plot_categories$condition == "Judicial Punishment_Opponent", 1:3] <-
        median_hdi(
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num] -
            esoph_plot$.value[esoph_plot$fraud == 1 &
                                esoph_plot$punishment == 1 &
                                esoph_plot$judicial_punishment == 1 &
                                esoph_plot$opponent == 1 &
                                esoph_plot$Trust == num],
          .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  # }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
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
    )
  ) %>%
  ggplot(., aes(
    y = category,
    x = median,
    shape = opponent,
    group = condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent
  ),
  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Russia",
    subtitle = paste0(
      "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    ),
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "",
    shape = ""
  ) +
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
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/ru_hdi89_conditional.png",
       height = 8,
       width = 10)


###################################### Unused so far
# LA: No Copy Paste or Incomplete ####
# mpol <- read_rds("output/ol_la_condition_234.rds")
# n_cases <- c()
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 12),
#     lower = rep(NA, 12),
#     upper = rep(NA, 12),
#     category = rep(1:4, 3),
#     condition = rep(c(
#       "Control",
#       "Punishment",
#       "Judicial Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition == cond, 1:3] <-
#       median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#                                       esoph_plot$Trust == num
#       ] -
#         esoph_plot$.value[esoph_plot$condition == cond &
#                             esoph_plot$Trust == num
#         ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
# #
# # plotting <- plotting %>%
# #   mutate(
# #     institution =
# #       case_when(
# #         institution == "pol_inst_armed" ~ "Armed Forces",
# #         institution == "pol_inst_police" ~ "Police",
# #         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
# #         institution == "pol_inst_gov" ~ "Government",
# #         institution == "pol_inst_part" ~ "Parties",
# #         institution == "pol_inst_parl" ~ "Parliament",
# #         institution == "pol_inst_courts" ~ "Courts",
# #         institution == "pol_inst_pres" ~ "President",
# #         TRUE ~ institution
# #       ),
# #     institution = as_factor(institution) %>%
# #       fct_relevel(
# #         "Central Electoral\nCommission",
# #         "Parties",
# #         "Parliament",
# #         "Courts",
# #         "President",
# #         "Government",
# #         "Police",
# #         "Armed Forces"
# #       ))
#
# plotting %>%
#   # filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                      x = median,
#                      group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.4)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = paste0("Sample: LA, depicted are 89% HDI"),
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 4,
#              labeller = label_glue('{pol}, N = {n_cases}')) +
#   viridis::scale_color_viridis(discrete = T, option = "C") -> plot
#
#
# ggsave(plot,
#        filename = "figs/la_hdi89_no_copy_paste_incomplete.png",
#        height = 8,
#        width = 10)
#
# # LA: No Copy Paste ######
# #
# mpol <- read_rds("output/ol_la_condition_2.rds")
# n_cases <- c()
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 12),
#     lower = rep(NA, 12),
#     upper = rep(NA, 12),
#     category = rep(1:4, 3),
#     condition = rep(c(
#       "Control",
#       "Punishment",
#       "Judicial Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition == cond, 1:3] <-
#         median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#                                        esoph_plot$Trust == num
#                                      ] -
#                      esoph_plot$.value[esoph_plot$condition == cond &
#                                          esoph_plot$Trust == num
#                                        ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
#
# plotting %>%
#   # filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.4)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = paste0("Sample: LA, depicted are 89% HDI, no copy-paste or incomplete cases"),
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 4,
#              labeller = label_glue('{pol}, N = {n_cases}')) +
#   viridis::scale_color_viridis(discrete = T, option = "C") -> plot
#
#
# ggsave(plot,
#        filename = "figs/la_hdi89_no_copy_paste.png",
#        height = 8,
#        width = 10)
#
# # RU: No Copy Paste & Incomplete ####
# #
# mpol <- read_rds("output/ol_ru_condition_234.rds")
# n_cases <- c()
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 12),
#     lower = rep(NA, 12),
#     upper = rep(NA, 12),
#     category = rep(1:4, 3),
#     condition = rep(c(
#       "Control",
#       "Punishment",
#       "Judicial Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition == cond, 1:3] <-
#         median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#                                        esoph_plot$Trust == num
#                                      ] -
#                      esoph_plot$.value[esoph_plot$condition == cond &
#                                          esoph_plot$Trust == num
#                                        ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
#
#
# plotting %>%
#   # filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.4)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = paste0("Sample: RU, depicted are 89% HDI, no copy-paste or incomplete cases"),
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 4,
#              labeller = label_glue('{pol}, N = {n_cases}')) +
#   viridis::scale_color_viridis(discrete = T, option = "C") -> plot
#
#
# ggsave(plot,
#        filename = "figs/ru_hdi89_no_copy_paste_incomplete.png",
#        height = 8,
#        width = 10)
#
#
#
#
# # RU: No Copy Paste ####
#
# mpol <- read_rds("output/ol_ru_condition_2.rds")
# n_cases <- c()
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 12),
#     lower = rep(NA, 12),
#     upper = rep(NA, 12),
#     category = rep(1:4, 3),
#     condition = rep(c(
#       "Control",
#       "Punishment",
#       "Judicial Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   n_cases <- c(n_cases, nrow(mpol[[inst]]$data))
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition == cond, 1:3] <-
#         median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#                                        esoph_plot$Trust == num
#                                      ] -
#                      esoph_plot$.value[esoph_plot$condition == cond &
#                                          esoph_plot$Trust == num
#                                        ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
#
# plotting %>%
#   # filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.4)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = paste0("Sample: RU, depicted are 89% HDI, no copy-paste cases"),
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 4,
#              labeller = label_glue('{pol}, N = {n_cases}')) +
#   viridis::scale_color_viridis(discrete = T, option = "C") -> plot
#
#
# ggsave(plot,
#        filename = "figs/ru_hdi89_no_copy_paste.png",
#        height = 8,
#        width = 10)
#
#
#
#
#
#
#
#
# plotting %>%
#   filter(str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.3)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = "Sample: LA, depicted are 89% HDI",
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 3) +
#   viridis::scale_color_viridis(discrete = T) -> plot
#
# ggsave(plot,
#        filename = "figs/la_npol_hdi89.png",
#        height = 6,
#        width = 10)
#
# #### RUS#####
#
# mpol <- read_rds("output/ol_rus_condition.rds")
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 12),
#     lower = rep(NA, 12),
#     upper = rep(NA, 12),
#     category = rep(1:4, 3),
#     condition = rep(c(
#       "Control",
#       "Punishment",
#       "Judicial Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition == cond, 1:3] <-
#         # quantile(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#         #                              esoph_plot$Trust == num
#         # ] -
#         #   esoph_plot$.value[esoph_plot$condition == cond &
#         #                       esoph_plot$Trust == num
#         #   ],
#         # c(0.05, 0.5, 0.95))
#         # hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#         #                              esoph_plot$Trust == num
#         # ] -
#         #   esoph_plot$.value[esoph_plot$condition == cond &
#       #                       esoph_plot$Trust == num
#       #   ], .width = 0.89)
#       median_hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#                                      esoph_plot$Trust == num
#       ] -
#         esoph_plot$.value[esoph_plot$condition == cond &
#                             esoph_plot$Trust == num
#         ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
# #
# # plotting <- plotting %>%
# #   mutate(
# #     institution =
# #       case_when(
# #         institution == "pol_inst_armed" ~ "Armed Forces",
# #         institution == "pol_inst_police" ~ "Police",
# #         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
# #         institution == "pol_inst_gov" ~ "Government",
# #         institution == "pol_inst_part" ~ "Parties",
# #         institution == "pol_inst_parl" ~ "Parliament",
# #         institution == "pol_inst_courts" ~ "Courts",
# #         institution == "pol_inst_pres" ~ "President",
# #         TRUE ~ institution
# #       ),
# #     institution = as_factor(institution) %>%
# #       fct_relevel(
# #         "Central Electoral\nCommission",
# #         "Parties",
# #         "Parliament",
# #         "Courts",
# #         "President",
# #         "Government",
# #         "Police",
# #         "Armed Forces"
# #       ))
#
# plotting
# plotting %>%
#   filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.3)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = "Sample: RU, depicted are 89% HDI",
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 4) +
#   viridis::scale_color_viridis(discrete = T) -> plot
#
# ggsave(plot,
#        filename = "figs/ru_pol_hdi89.png",
#        height = 6,
#        width = 10)
#
# plotting %>%
#   filter(str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition),
#                   position = position_dodge(0.3)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = "Sample: RU, depicted are 89% HDI",
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 3) +
#   viridis::scale_color_viridis(discrete = T) -> plot
#
# ggsave(plot,
#        filename = "figs/ru_npol_hdi89.png",
#        height = 6,
#        width = 10)
#
# ##### merged conditions####
#
# mpol <- read_rds("output/ol_la_condition1.rds")
# pol <- names(mpol)
# plotting <- tibble()
# for (inst in pol){
#   esoph_plot = mpol$pol_inst_armed$data %>%
#     data_grid(condition1) %>%
#     add_fitted_draws(mpol[[inst]],
#                      category = "Trust")
#   plot_categories <- tibble(
#     median = rep(NA, 8),
#     lower = rep(NA, 8),
#     upper = rep(NA, 8),
#     category = rep(1:4, 2),
#     condition1 = rep(c(
#       "Control",
#       "Punishment"
#     ),
#     each = 4)
#   ) %>%
#     as.data.frame()
#
#   for (cond in levels(mpol$pol_inst_armed$data$condition1)[2:3]){
#     for (num in 1:4){
#       plot_categories[plot_categories$category == num &
#                         plot_categories$condition1 == cond, 1:3] <-
#         # quantile(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#         #                              esoph_plot$Trust == num
#         # ] -
#         #   esoph_plot$.value[esoph_plot$condition == cond &
#         #                       esoph_plot$Trust == num
#         #   ],
#         # c(0.05, 0.5, 0.95))
#         # hdi(esoph_plot$.value[esoph_plot$condition == "Fraud" &
#         #                              esoph_plot$Trust == num
#         # ] -
#         #   esoph_plot$.value[esoph_plot$condition == cond &
#       #                       esoph_plot$Trust == num
#       #   ], .width = 0.89)
#       median_hdi(esoph_plot$.value[esoph_plot$condition1 == "Fraud" &
#                                      esoph_plot$Trust == num
#       ] -
#         esoph_plot$.value[esoph_plot$condition1 == cond &
#                             esoph_plot$Trust == num
#         ], .width = 0.89)[1:3]
#       plot_categories$institution <- inst
#     }
#   }
#   plotting <- bind_rows(plot_categories, plotting)
# }
# #
# # plotting <- plotting %>%
# #   mutate(
# #     institution =
# #       case_when(
# #         institution == "pol_inst_armed" ~ "Armed Forces",
# #         institution == "pol_inst_police" ~ "Police",
# #         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
# #         institution == "pol_inst_gov" ~ "Government",
# #         institution == "pol_inst_part" ~ "Parties",
# #         institution == "pol_inst_parl" ~ "Parliament",
# #         institution == "pol_inst_courts" ~ "Courts",
# #         institution == "pol_inst_pres" ~ "President",
# #         TRUE ~ institution
# #       ),
# #     institution = as_factor(institution) %>%
# #       fct_relevel(
# #         "Central Electoral\nCommission",
# #         "Parties",
# #         "Parliament",
# #         "Courts",
# #         "President",
# #         "Government",
# #         "Police",
# #         "Armed Forces"
# #       ))
#
# plotting
# plotting %>%
#   # filter(!str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition1)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition1),
#                   position = position_dodge(0.3)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = "Sample: LA, depicted are 89% HDI",
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 3) +
#   viridis::scale_color_viridis(discrete = T) -> plot
#
# ggsave(plot,
#        filename = "figs/la_hdi89.png",
#        height = 8,
#        width = 10)
#
# plotting %>%
#   filter(str_detect(institution, "npol"))%>%
#   ggplot(., aes(y = category,
#                 x = median,
#                 group = condition1)) +
#   geom_pointrange(aes(xmin = lower,
#                       xmax = upper,
#                       color = condition1),
#                   position = position_dodge(0.3)) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(title = "Change in Probabilities for Trust Ranking Categories",
#        subtitle = "Sample: LA, depicted are 89% HDI",
#        x = "Pr(Category|Fraud) - Pr(Category|Condition)",
#        y = "") +
#   scale_y_continuous(
#     breaks = 1:4,
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   geom_vline(aes(xintercept = 0), alpha = 0.3) +
#   facet_wrap(~ institution,
#              ncol = 3) +
#   viridis::scale_color_viridis(discrete = T) -> plot
#
# ggsave(plot,
#        filename = "figs/la_npol1_hdi89.png",
#        height = 6,
#        width = 10)

# LA: Countries FE ####

### Political Institutions #####

mpol <- read_rds("output/ol_main_la_pol_colombia.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition,
              # questnnr
              ) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust",
                     # questnnr = "colombia"
                     )
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
                                       # esoph_plot$questnnr == "mexico"

        ] -
          esoph_plot$.value[esoph_plot$condition == cond &
                              esoph_plot$Trust == num
                              # esoph_plot$questnnr == "mexico"
          ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  mutate(institution = case_when(institution == "pol_inst_pres" ~ "President",
                                 institution == "pol_inst_police" ~ "Police",
                                 institution == "pol_inst_CEC" ~ "Central Electoral Commission",
                                 institution == "pol_inst_gov" ~ "Government",
                                 institution == "pol_inst_part" ~ "Political Parties",
                                 institution == "pol_inst_parl" ~ "Parliament",
                                 institution == "pol_inst_courts" ~ "Courts",
                                 institution == "pol_inst_armed" ~ "Armed Forces",
  ),
  # Condition = condition,
  Condition = condition %>%
    factor(., levels = c(
      "Control", "Punishment", "Judicial Punishment"
    ))) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Colombia",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
  # facet_wrap(~ institution, ncol = 4)
  facet_wrap(~ institution,
             ncol = 4,
             labeller = label_glue('{institution}, N = {n_cases}'
                                   )
             ) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/colombia_hdi89.png",
       height = 8,
       width = 10)


mpol <- read_rds("output/ol_main_la_pol_mexico.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(condition,
              # questnnr
    ) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust",
                     # questnnr = "colombia"
    )
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
                                     # esoph_plot$questnnr == "mexico"

        ] -
          esoph_plot$.value[esoph_plot$condition == cond &
                              esoph_plot$Trust == num
                            # esoph_plot$questnnr == "mexico"
          ], .width = 0.89)[1:3]
      plot_categories$institution <- inst
    }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
  mutate(institution = case_when(institution == "pol_inst_pres" ~ "President",
                                 institution == "pol_inst_police" ~ "Police",
                                 institution == "pol_inst_CEC" ~ "Central Electoral Commission",
                                 institution == "pol_inst_gov" ~ "Government",
                                 institution == "pol_inst_part" ~ "Political Parties",
                                 institution == "pol_inst_parl" ~ "Parliament",
                                 institution == "pol_inst_courts" ~ "Courts",
                                 institution == "pol_inst_armed" ~ "Armed Forces",
  ),
  # Condition = condition,
  Condition = condition %>%
    factor(., levels = c(
      "Control", "Punishment", "Judicial Punishment"
    ))) %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(y = category,
                x = median,
                group = Condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = Condition),
                  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Mexico",
       subtitle = paste0(
         "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
       ),
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
  # facet_wrap(~ institution, ncol = 4)
  facet_wrap(~ institution,
             ncol = 4,
             labeller = label_glue('{institution}, N = {n_cases}'
             )
  ) +
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
ggsave(plot,
       filename = "figs/mexico_hdi89.png",
       height = 8,
       width = 10)


### Political Institutions #####
mpol <- read_rds("output/ol_ru_pol_involvement.rds")
n_cases <- c()
pol <- names(mpol)
plotting <- tibble()
for (inst in pol){
  esoph_plot = mpol$pol_inst_armed$data %>%
    data_grid(fraud, punishment, judicial_punishment, involvement) %>%
    add_fitted_draws(mpol[[inst]],
                     category = "Trust")
  plot_categories <- tibble(
    median = rep(NA, 12*2),
    lower = rep(NA, 12*2),
    upper = rep(NA, 12*2),
    category = rep(1:4, 3*2),
    condition = rep(c(
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Control_involvement",
      "Punishment_involvement",
      "Judicial Punishment_Opponent"
    ),
    each = 4)
  ) %>%
    as.data.frame()

  n_cases <- c(n_cases, nrow(mpol[[inst]]$data))

  # for (cond in plot_categories$condition[1:3]){
  for (num in 1:4){
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Control", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 0 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Control_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 0 &
                              esoph_plot$punishment == 0 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]

    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Punishment", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Punishment_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 0 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]

    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Judicial Punishment", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 0 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 1 &
                              esoph_plot$opponent == 0 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories[plot_categories$category == num &
                      plot_categories$condition == "Judicial Punishment_Opponent", 1:3] <-
      median_hdi(
        esoph_plot$.value[esoph_plot$fraud == 1 &
                            esoph_plot$punishment == 0 &
                            esoph_plot$judicial_punishment == 0 &
                            esoph_plot$opponent == 1 &
                            esoph_plot$Trust == num] -
          esoph_plot$.value[esoph_plot$fraud == 1 &
                              esoph_plot$punishment == 1 &
                              esoph_plot$judicial_punishment == 1 &
                              esoph_plot$opponent == 1 &
                              esoph_plot$Trust == num],
        .width = 0.89)[1:3]
    plot_categories$institution <- inst
  }
  # }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting %>%
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
    )
  ) %>%
  ggplot(., aes(
    y = category,
    x = median,
    shape = opponent,
    group = condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    shape = opponent
  ),
  position = position_dodge(0.4)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Effect of Fraud and Punishment Information on Confidence in Political Institutions in Russia",
    subtitle = paste0(
      "89% HDIs for differences in probabilities for categories based on draws from expectation of the posterior predictive distributions"
    ),
    x = "Pr(Category|Fraud) - Pr(Category|Condition)",
    y = "",
    shape = ""
  ) +
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
  viridis::scale_color_viridis(discrete = T, option = "C") -> plot

plot
