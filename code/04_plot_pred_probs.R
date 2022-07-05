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
                plot.background = element_blank(),
                plot.title.position = "plot",
                plot.caption.position = "plot",
                legend.title = element_blank(),
                legend.position = "bottom"
              ))
}

setup()

formals(plasma)$end <- 0.8
formals(scale_color_viridis)$end <- 0.8
formals(scale_alpha_manual)$values <- c(0.5, 1)

## Russia
mpol <- read_rds("output/ol_main_ru_pol_1226.rds")
plotting4 <- tibble()
pol <- names(mpol)
for (i in pol) {
  temp <- mpol[[i]] %>%
    conditional_effects(
      prob = 0.89,
      method = "posterior_epred",
      plot = FALSE,
      categorical = T
    )
  plotting4 <- temp[[1]] %>%
    mutate(institution = i,
           n = nrow(mpol[[i]]$data)) %>%
    dplyr::select(-i) %>%
    bind_rows(., plotting4)
}
n_ru <- plotting4 %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)


plot_ru <- plotting4 %>%
  mutate(
    cats__ == as.character(cats__),
    institution =
      case_when(
        institution == "pol_inst_armed" ~ "Armed Forces",
        institution == "pol_inst_police" ~ "Police",
        institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
        institution == "pol_inst_gov" ~ "Government",
        institution == "pol_inst_part" ~ "Parties",
        institution == "pol_inst_parl" ~ "Parliament",
        institution == "pol_inst_courts" ~ "Courts",
        institution == "pol_inst_pres" ~ "President"
      ),
    institution = as_factor(institution) %>%
      fct_relevel(
        "Central Electoral\nCommission",
        "Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      ),
    # opponent = ifelse(opponent == 1, "Opponents", "Supporters")
  ) %>%
  ggplot() +
  # geom_abline(
  #   intercept = 0.5,
  #   slope = 10,
  #   size = 0.5,
  #   linetype = 2,
  #   color = "black",
  #   alpha = 0.5
  # ) +
  # geom_abline(
  #   intercept = 5,
  #   slope = -10,
  #   size = 0.5,
  #   linetype = 3,
  #   color = "black",
  #   alpha = 0.5
  # ) +
  geom_pointrange(aes(
    x = estimate__,
    xmin = lower__,
    xmax = upper__,
    y = cats__,
    shape = condition,
    color = condition
  ),
  position = position_dodge(1),
  size = 0.4
  ) +
  facet_wrap(
    . ~ institution,
    ncol = 4,
    labeller = label_glue(
      '{institution}, N = {n_ru}'
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 16, 17, 8)) +
  labs(
    y = "Confidence",
    x = "Probability(Category)",
    shape = "",
    color = "",
    title = "Russia"
  ) +
  scale_y_discrete(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  )
  # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
  # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
plot_ru
## LA
mpol <- read_rds("output/ol_main_la_pol_881.rds")
plotting4 <- tibble()

for (i in pol) {
  temp <- mpol[[i]] %>%
    conditional_effects(
      prob = 0.89,
      method = "posterior_epred",
      plot = FALSE,
      categorical = T
    )
  plotting4 <- temp[[1]] %>%
    mutate(institution = i,
           n = nrow(mpol[[i]]$data)) %>%
    dplyr::select(-i) %>%
    bind_rows(., plotting4)
}

plot_la <- plotting4 %>%
  mutate(
    cats__ == as.character(cats__),
    institution =
      case_when(
        institution == "pol_inst_armed" ~ "Armed Forces",
        institution == "pol_inst_police" ~ "Police",
        institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
        institution == "pol_inst_gov" ~ "Government",
        institution == "pol_inst_part" ~ "Parties",
        institution == "pol_inst_parl" ~ "Parliament",
        institution == "pol_inst_courts" ~ "Courts",
        institution == "pol_inst_pres" ~ "President"
      ),
    institution = as_factor(institution) %>%
      fct_relevel(
        "Central Electoral\nCommission",
        "Parties",
        "Parliament",
        "Courts",
        "President",
        "Government",
        "Police",
        "Armed Forces"
      ),
    # opponent = ifelse(opponent == 1, "Opponents", "Supporters")
  ) %>%
  ggplot() +
  # geom_abline(
  #   intercept = 1.8,
  #   slope = 7,
  #   size = 0.5,
  #   linetype = 2,
  #   color = "black",
  #   alpha = 0.5
  # ) +
  # geom_abline(
  #   intercept = 3.8,
  #   slope = -7,
  #   size = 0.5,
  #   linetype = 3,
  #   color = "black",
  #   alpha = 0.5
  # ) +
  geom_pointrange(aes(
    x = estimate__,
    xmin = lower__,
    xmax = upper__,
    y = cats__,
    shape = condition,
    color = condition
  ),
  position = position_dodge(1),
  size = 0.4
  ) +
  facet_wrap(
    . ~ institution,
    ncol = 4,
    labeller = label_glue(
      '{institution}, N = { plotting4 %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}'
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 16, 17, 8)) +
  labs(
    y = "Confidence",
    x = "Probability(Category)",
    shape = "",
    color = "",
    title = "Latin America"
  ) +
  scale_y_discrete(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  )
  # annotate("text", x = 0.38, y = 4, label = "much trust", size = 3, angle = 51) + # annotation on all facets
  # annotate("text", x = 0.42, y = 1.1, label = "little trust", size = 3, angle = -51) # annotation on all facets
plot_la


pp <- plot_la / plot_ru +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
pp
ggsave(pp,
       filename = paste0("figs/probs_hdi89_", 1, ".png"),
       height = 8,
       width = 10)
