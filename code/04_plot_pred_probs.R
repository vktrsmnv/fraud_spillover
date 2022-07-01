#### OL ####
mpol <- read_rds("output/ol_main_ru_pol_1226.rds")

plotting4 <- plotting3 <- tibble()
for (i in pol) {
  temp <- mpol[[i]] %>%
    conditional_effects(
      prob = 0.89,
      method = "posterior_epred",
      plot = FALSE,
      categorical = T
    )
  plotting4 <- temp[[1]] %>%
    mutate(institution = i) %>%
    dplyr::select(-i) %>%
    bind_rows(., plotting4)

  plotting3 <- data_rus %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[i]]) %>%
    mutate(institution = i) %>%
    bind_rows(plotting3, .)
}




plotting <- tibble()
for (i in pol) {
  temp <- mpol[[i]] %>%
    conditional_effects(
      prob = 0.89,
      method = "posterior_epred",
      plot = FALSE,
    categorical = TRUE
    )
  plotting <- temp[[1]] %>%
    mutate(institution = i) %>%
    dplyr::select(-i) %>%
    bind_rows(., plotting)
}

plotting4 %>%
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
  facet_wrap(. ~ institution,
             ncol = 4,
             labeller = label_glue('{institution}, N = {n_cases}'
             )
  ) +
theme_bw() +
  theme(
    strip.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top"
  ) +
  # scale_y_continuous(
  #   limits = c(1, 4),
  #   labels = c(
  #     "None\nat all",
  #     "Not very\nmuch",
  #     "Quite\na Lot",
  #     "A Great\nDeal"
  #   )
  # ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  scale_shape_manual(values = c(15, 16, 17, 8)) +
  labs(
    y = "",
    x = "Probability",
    shape = "",
    color = "",
    subtitle = "Expected Values of the Posterior Predictive Distribution"
  ) +
  scale_y_discrete(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  )

