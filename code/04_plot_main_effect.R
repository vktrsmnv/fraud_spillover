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
  theme_set(
    theme_bw(base_family = "Source Sans Pro") +
      # theme_bw(base_family = "Source Sans Pro") +
      theme(
        # plot.background = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "bottom"
      )
  )
}

setup()

formals(plasma)$end <- 0.8
formals(scale_color_viridis)$end <- 0.8
formals(scale_alpha_manual)$values <- c(0.5, 1)

## Political Institutions ####
### All Cases #####

plotting_prep <-
  function(mpol_path = "output/ol_main_ru_pol_1226.rds") {
    mpol <- read_rds(mpol_path)
    pol <- names(mpol)
    plotting <- tibble()
    for (inst in pol) {
      esoph_plot <- mpol$pol_inst_armed$data %>%
        data_grid(condition) %>%
        add_epred_draws(mpol[[inst]],
          category = "Trust"
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
        each = 4
        )
      ) %>%
        as.data.frame()

      plot_categories$n <- nrow(mpol[[inst]]$data)

      for (cond in levels(mpol$pol_inst_armed$data$condition)[2:4]) {
        for (num in 1:4) {
          plot_categories[plot_categories$category == num &
            plot_categories$condition == cond, 1:3] <-
            median_hdci(
              esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                esoph_plot$Trust == num] %>% as.numeric() -
                esoph_plot$.epred[esoph_plot$condition == cond &
                  esoph_plot$Trust == num] %>% as.numeric(),
              .width = 0.89
            )[1:3]
          plot_categories$institution <- inst
        }
      }
      plotting <- bind_rows(plot_categories, plotting)
    }


    #### Plot for Control vs. Fraud conditions alone

    plotting %>%
      mutate(significant = ifelse(lower < 0 &
        upper > 0, "no", "yes")) %>%
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
        Condition = condition %>%
          factor(., levels = c(
            "Control", "Punishment", "Judicial Punishment"
          ))
      ) -> plotting

    return(plotting)
  }


plotting <-
  plotting_prep(mpol_path = "output/ol_main_ru_pol_1223.rds")
n_ru <-
  plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)
# plot_ru
arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution = "Central Electoral\nCommission"
) %>%
  mutate(
    institution = as_factor(institution) %>%
      fct_expand(
        "Central Electoral\nCommission",
        "Political Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      )
  )

plot_ru <- plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  filter(str_detect(string = condition, "Control")) %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.3,
    color = plasma(1, 0.7)
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
  facet_wrap(~institution,
    ncol = 4,
    labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3)

plot_ru

# load the estimation results from main model
plotting <-
  plotting_prep(mpol_path = "output/ol_main_la_pol_872.rds")

plotting %>%
  filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(
    ~institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.25,
    color = plasma(1, 0.7)
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
  ) -> plot_la

plot_la
pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  pp,
  filename = paste0("figs/main_hdi89_", 1, ".png"),
  height = 8,
  width = 10
)


### Exclude Attention Check == Unaccpetable #####

plotting <-
  plotting_prep(mpol_path = "output/ol_main_ru_pol_1201.rds")
n_ru <-
  plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)
# plot_ru
arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution = "Central Electoral\nCommission"
) %>%
  mutate(
    institution = as_factor(institution) %>%
      fct_expand(
        "Central Electoral\nCommission",
        "Political Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      )
  )

plot_ru <- plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  filter(str_detect(string = condition, "Control")) %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.3,
    color = plasma(1, 0.7)
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
  facet_wrap(~institution,
    ncol = 4,
    labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3)

plot_ru

# load the estimation results from main model
plotting <-
  plotting_prep(mpol_path = "output/ol_main_la_pol_847.rds")

plotting %>%
  filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(
    ~institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.25,
    color = plasma(1, 0.7)
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
  ) -> plot_la

plot_la

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  pp,
  filename = paste0("figs/main_hdi89_", 2, ".png"),
  height = 8,
  width = 10
)


### Only Correct Summaries #####

plotting <-
  plotting_prep(mpol_path = "output/ol_main_ru_pol_667.rds")
n_ru <-
  plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)
# plot_ru
arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution = "Central Electoral\nCommission"
) %>%
  mutate(
    institution = as_factor(institution) %>%
      fct_expand(
        "Central Electoral\nCommission",
        "Political Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      )
  )

plot_ru <- plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  filter(str_detect(string = condition, "Control")) %>%
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.3,
    color = plasma(1, 0.7)
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
  facet_wrap(~institution,
    ncol = 4,
    labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3)

plot_ru

# load the estimation results from main model
plotting <-
  plotting_prep(mpol_path = "output/ol_main_la_pol_376.rds")

plotting %>%
  filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(
    ~institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  # xlim(-0.3, 0.3) +
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
      category = 1.9,
      Condition = c("Control"),
      institution = "Central Electoral\nCommission"
    ) %>%
      mutate(
        institution = as_factor(institution) %>%
          fct_expand(
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "Courts",
            "President",
            "Government",
            "Police",
            "Armed Forces"
          )
      ),
    label = "fraud\ndecreases\ntrust",
    size = 2.7,
    nudge_y = 0.25,
    color = plasma(1, 0.7)
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
  ) -> plot_la

plot_la
pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  pp,
  filename = paste0("figs/main_hdi89_", 3, ".png"),
  height = 8,
  width = 10
)






### All Conditions, All Cases ####

plotting <-
  plotting_prep(mpol_path = "output/ol_main_ru_pol_1223.rds")
n_ru <-
  plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)
# plot_ru
arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution = "Central Electoral\nCommission"
) %>%
  mutate(
    institution = as_factor(institution) %>%
      fct_expand(
        "Central Electoral\nCommission",
        "Political Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      )
  )

plot_ru <- plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(
    aes(
      xmin = lower,
      xmax = upper,
      color = Condition,
      shape = Condition,
      alpha = significant
    ),
    position = position_dodge(0.4)
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
  guides(shape = guide_legend(override.aes = list(
    shape = c(16, 17, 15),
    color = viridis(3, end = 0.8, option = "C")
  ))) +
  # labels = c("Opponent", "Supporter")) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  # geom_abline(intercept = 2.5,
  #             slope = -15,
  #             size = 0.5,
  #             alpha = 0.3,
  #             linetype = 2,
  #             color = plasma(1)) +
  # geom_text(data = data.frame(median = 0.2,
  #                             category = 1.9,
  #                             Condition =  c(
  #                               "Control"
  #                             ),
  #                             institution = "Central Electoral\nCommission") %>%
  #             mutate(institution = as_factor(institution) %>%
  #                      fct_expand(
  #                        "Central Electoral\nCommission",
  #                        "Political Parties",
  #                        "Parliament",
  #                        "Courts",
  #                        "President",
  #                        "Government",
  #                        "Police",
  #                        "Armed Forces"
  #                      )),
  #           label = "fraud\ndecreases\ntrust",
  #           size = 2.7,
  #           nudge_y = 0.3,
  #           color = plasma(1, 0.7)) +
  # geom_curve(
  #   data = arrows,
  #   aes(x = x1, y = y1, xend = x2, yend = y2),
  #   arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
  #   alpha = 0.3, color = plasma(1),
  #   curvature = -0.3)+
  facet_wrap(~institution,
    ncol = 4,
    labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3)
xlm <- c(min(plotting$lower), max(plotting$upper)) %>%
  plyr::round_any(x = ., accuracy = 0.1)
plot_ru

# load the estimation results from main model
plotting <-
  plotting_prep(mpol_path = "output/ol_main_la_pol_872.rds")

plot_la <- plotting %>%
  # filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(
    ~institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.3, 0.3) +
  guides(shape = guide_legend(override.aes = list(
    shape = c(16, 17, 15),
    color = viridis(3, end = 0.8, option = "C")
  )))
  # labels = c("Opponent", "Supporter"))
  # geom_abline(intercept = 2.5,
  #             slope = -15,
  #             size = 0.5,
  #             alpha = 0.3,
  #             linetype = 2,
  #             color = plasma(1)) +
  # geom_text(data = data.frame(median = 0.2,
  #                             category = 1.9,
  #                             Condition =  c(
  #                               "Control"
  #                             ),
  #                             institution = "Central Electoral\nCommission") %>%
  #             mutate(institution = as_factor(institution) %>%
  #                      fct_expand(
  #                        "Central Electoral\nCommission",
  #                        "Political Parties",
  #                        "Parliament",
  #                        "Courts",
  #                        "President",
  #                        "Government",
  #                        "Police",
  #                        "Armed Forces"
  #                      )),
  #           label = "fraud\ndecreases\ntrust",
  #           size = 2.7,nudge_y = 0.25,
  #           color = plasma(1, 0.7)) +
  # geom_curve(
  #   data = arrows,
  #   aes(x = x1, y = y1, xend = x2, yend = y2),
  #   arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
  #   alpha = 0.3, color = plasma(1),
  #   curvature = -0.3)

  plot_la

# comparable xlim
xlm[1] <- ifelse(
  min(plotting$lower) %>%
    plyr::round_any(x = ., accuracy = 0.1) < xlm[1],
  min(plotting$lower) %>%
    plyr::round_any(x = ., accuracy = 0.1),
  xlm[1]
)

xlm[2] <- ifelse(
  max(plotting$upper) %>%
    plyr::round_any(x = ., accuracy = 0.1) > xlm[2],
  max(plotting$upper) %>%
    plyr::round_any(x = ., accuracy = 0.1),
  xlm[2]
)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  xlim(xlm[1] - 0.05, xlm[2] + 0.05)
pp

ggsave(
  pp,
  filename = paste0("figs/main_hdi89_", 4, ".png"),
  height = 8,
  width = 10
)

### All Conditions, Correct Summaries ####

plotting <-
  plotting_prep(mpol_path = "output/ol_main_ru_pol_667.rds")
n_ru <-
  plotting %>%
  select(institution, n) %>%
  distinct() %>%
  arrange(institution) %>%
  pull(n)
# plot_ru
arrows <- data.frame(
  x1 = 0.19,
  x2 = 0.1,
  y1 = 1.55,
  y2 = 1.2,
  Condition = c("Control"),
  institution = "Central Electoral\nCommission"
) %>%
  mutate(
    institution = as_factor(institution) %>%
      fct_expand(
        "Central Electoral\nCommission",
        "Political Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      )
  )

plot_ru <- plotting %>%
  # filter(!str_detect(institution, "npol"))%>%
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(
    aes(
      xmin = lower,
      xmax = upper,
      color = Condition,
      shape = Condition,
      alpha = significant
    ),
    position = position_dodge(0.4)
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
  guides(shape = guide_legend(override.aes = list(
    shape = c(16, 17, 15),
    color = viridis(3, end = 0.8, option = "C")
  ))) +
  # labels = c("Opponent", "Supporter")) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~institution,
    ncol = 4,
    labeller = label_glue("{institution}, N = {n_ru}")
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  )

xlm <- c(min(plotting$lower), max(plotting$upper)) %>%
  plyr::round_any(x = ., accuracy = 0.1)

plot_ru

# load the estimation results from main model
plotting <-
  plotting_prep(mpol_path = "output/ol_main_la_pol_376.rds")

plot_la <- plotting %>%
  # filter(str_detect(string = condition, "Control")) %>% # only plot the control condition for main effects
  ggplot(., aes(
    y = category,
    x = median,
    group = Condition
  )) +
  geom_pointrange(aes(
    xmin = lower,
    xmax = upper,
    color = Condition,
    alpha = significant
  ),
  position = position_dodge(0.4)
  ) +
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
    labels = c(
      "None\nat all",
      "Not very\nmuch",
      "Quite\na Lot",
      "A Great\nDeal"
    )
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(
    ~institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  xlim(-0.35, 0.45) +
  guides(shape = guide_legend(override.aes = list(
    shape = c(16, 17, 15),
    color = viridis(3, end = 0.8, option = "C")
  )))
# labels = c("Opponent", "Supporter"))
# geom_abline(intercept = 2.5,
#             slope = -15,
#             size = 0.5,
#             alpha = 0.3,
#             linetype = 2,
#             color = plasma(1)) +
# geom_text(data = data.frame(median = 0.2,
#                             category = 1.9,
#                             Condition =  c(
#                               "Control"
#                             ),
#                             institution = "Central Electoral\nCommission") %>%
#             mutate(institution = as_factor(institution) %>%
#                      fct_expand(
#                        "Central Electoral\nCommission",
#                        "Political Parties",
#                        "Parliament",
#                        "Courts",
#                        "President",
#                        "Government",
#                        "Police",
#                        "Armed Forces"
#                      )),
#           label = "fraud\ndecreases\ntrust",
#           size = 2.7,nudge_y = 0.25,
#           color = plasma(1, 0.7)) +
# geom_curve(
#   data = arrows,
#   aes(x = x1, y = y1, xend = x2, yend = y2),
#   arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
#   alpha = 0.3, color = plasma(1),
#   curvature = -0.3)

plot_la

# comparable xlim
xlm[1] <- ifelse(
  min(plotting$lower) %>%
    plyr::round_any(x = ., accuracy = 0.1) < xlm[1],
  min(plotting$lower) %>%
    plyr::round_any(x = ., accuracy = 0.1),
  xlm[1]
)

xlm[2] <- ifelse(
  max(plotting$upper) %>%
    plyr::round_any(x = ., accuracy = 0.1) > xlm[2],
  max(plotting$upper) %>%
    plyr::round_any(x = ., accuracy = 0.1),
  xlm[2]
)

pp <- plot_la / plot_ru +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  xlim(xlm[1] - 0.05, xlm[2] + 0.05)
pp
ggsave(
  pp,
  filename = paste0("figs/main_hdi89_", 5, ".png"),
  height = 8,
  width = 10
)
