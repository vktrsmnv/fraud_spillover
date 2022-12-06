source("code/functions.R")

y_vars <- c(
  "inst_police", "inst_courts", "inst_parl",
  "inst_gov", "inst_parties", "inst_comp", "inst_UN", "inst_banks",
  "inst_wto",
  "inst_wb",
  "inst_armed"
)


# store all NN models in one file
for (y_var in y_vars){
  names <- list.files("output/",
                      pattern = paste0("models_", y_var, "_"),
                      include.dirs = T)

  models <- list()

  for (i in names){
    models <- c(models, readRDS(paste0("output/", i)))
  }

  saveRDS(models, file = paste0("output/models_", y_var, "_nn.rds"))
}

##### nearest neigbour ######
# function for clmm prediction

fake.predict.clmm <- function(model, newdata) {
  # Actual prediction function
  pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }

  # Multiply each row by the coefficients
  coefs <- c(model$beta, unlist(model$ST))
  xbetas <- sweep(newdata, MARGIN=2, coefs, `*`)

  # Make predictions
  pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=model$Theta))
  colnames(pred.mat) <- levels(model$model[,1])
  pred.mat
}

plotting <- tibble()
for (inst in y_vars){
  temp <- readRDS(paste0("output/models_", inst, "_nn.rds"))

  # get predictions from NN matching (all 1000 models, no extra uncertainty)
  pps <- tibble()
  for (i in 1:1000){
    pps <- bind_rows(pps,
                     fake.predict.clmm(temp[[i]],
                                       newdata = data.frame(fraud1d = c(0,1))) %>%
                       bind_cols(fraud1d = c(0,1)))
  }

  pps0 <- pps %>% filter(fraud1d == 0)
  pps1 <- pps %>% filter(fraud1d == 1)


  quants <- function(x) {
    c(quantile(x, c(0.025, 0.05, 0.5, 0.95, 0.975)), mean(x))
  }
  diffs <- (pps0[, 1:4] - pps1[, 1:4]) %>%
    apply(X = ., 2, quants) %>%
    t() %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(lower = x2_5_percent,
           upper = x97_5_percent,
           median = x50_percent,
           mean = v6) %>%
    mutate(institution = inst,
           n = temp[[1]]$model %>%
             nrow()) %>%
    rownames_to_column(var = "category") %>%
    mutate(significant = ifelse(lower < 0 &
                                  upper > 0, "no", "yes")) %>%
    mutate(
      institution = case_when(
        institution == "inst_comp" ~ "Companies",
        institution == "inst_banks" ~ "Banks",
        institution == "inst_env" ~ "Environmental Organizations",
        institution == "inst_UN" ~ "United Nations",
        institution == "inst_wb" ~ "World Bank",
        institution == "inst_wto" ~ "WTO",
        institution == "inst_pres" ~ "President",
        institution == "inst_police" ~ "Police",
        institution == "inst_gov" ~ "Government",
        institution == "inst_parties" ~ "Political Parties",
        institution == "inst_parl" ~ "Parliament",
        institution == "inst_courts" ~ "Courts",
        institution == "inst_armed" ~ "Armed Forces",
      ),
      institution_facet_name = paste0(institution),
      category = case_when(
        category == 1 ~ "None\nat all",
        category == 2 ~ "Not very\nmuch",
        category == 3 ~ "Quite\na Lot",
        category == 4 ~ "A Great\nDeal",
      ) %>%
        fct_inorder()
    ) %>%
    arrange(institution)

  plotting <- bind_rows(plotting, diffs) %>%
    mutate(institution_facet_name = fct_inorder(institution_facet_name))

}

plotting_nn <- plotting

# N = 29758
# settings for the facet style
nested_settings <- strip_nested(
  text_x = list(element_text(face = "bold",
                             size = rel(1.2)), NULL),
  background_x = list(element_rect(fill = NA), NULL),
  size = "variable",
  by_layer_x = TRUE
)


plotting %>%
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
      # color = condition,
      # shape = condition,
      # alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  ggh4x::facet_manual(
    vars(institution_facet_name),
    strip = nested_settings,
    axes = "margins",
    trim_blank = FALSE,
    remove_labels = "none",
    design = c(
      "
      EDCBAK
      FGHIJ#
      "
    )
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
      "Probability(Category|Fraud) - Probability(Category|No Fraud)"
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
  )

ggsave(
       filename = paste0("figs/matching_nn.png"),
       height = 6,
       width = 10
)


### cem ######
ci <- 0.95
baseline <- 0 # fraud1d == 0
plotting <- tibble()
cem <- readRDS("output/models_cem.RDS")
for (inst in 1:length(y_vars)) {
    esoph_plot <- cem[[inst]]$data %>%
      data_grid(fraud1d, cntry) %>%
      add_epred_draws(cem[[inst]],
                      category = "Trust"
      ) %>%
      filter(cntry == "ARG")

    plot_categories <- cem[[inst]]$data %>%
      data_grid(
        median = NA,
        lower = NA,
        upper = NA,
        fraud1d,
        cntry,
        category = 1:4
      ) %>%
      filter(cntry == "ARG")

    for (num in 1:4) {
      for (cntr in unique(plot_categories$cntry)) {
        for (cnd in unique(plot_categories$fraud1d)) {
          plot_categories[plot_categories$category == num &
                            plot_categories$cntry == cntr &
                            plot_categories$fraud1d == cnd, 1:3] <-
            median_hdci(esoph_plot$.epred[esoph_plot$fraud1d == baseline &
                                            esoph_plot$cntry == cntr &
                                            esoph_plot$Trust == num] -
                          esoph_plot$.epred[esoph_plot$fraud1d == cnd &
                                              esoph_plot$cntry == cntr &
                                              esoph_plot$Trust == num],
                        .width = ci
            )[1:3]

          plot_categories$institution <- y_vars[inst]
        }
      }
    }
    plotting <- bind_rows(plotting, plot_categories)
}

plotting_cem <- plotting %>%
  filter(fraud1d != baseline)

plotting  %>%
  janitor::clean_names() %>%
  mutate(significant = ifelse(lower < 0 &
                                upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "inst_comp" ~ "Companies",
      institution == "inst_banks" ~ "Banks",
      institution == "inst_env" ~ "Environmental Organizations",
      institution == "inst_UN" ~ "United Nations",
      institution == "inst_wb" ~ "World Bank",
      institution == "inst_wto" ~ "WTO",
      institution == "inst_pres" ~ "President",
      institution == "inst_police" ~ "Police",
      institution == "inst_gov" ~ "Government",
      institution == "inst_parties" ~ "Political Parties",
      institution == "inst_parl" ~ "Parliament",
      institution == "inst_courts" ~ "Courts",
      institution == "inst_armed" ~ "Armed Forces",
    ),
    institution_facet_name = paste0(institution) %>%  fct_inorder(),
    category = case_when(
      category == 1 ~ "None\nat all",
      category == 2 ~ "Not very\nmuch",
      category == 3 ~ "Quite\na Lot",
      category == 4 ~ "A Great\nDeal",
    ) %>%
      fct_inorder()
  ) %>%
  arrange(institution) %>%
  filter(
    cntry %in% unique(plot_categories$cntry)[1],
    fraud1d == 1
  ) %>%
  mutate(institution = as_factor(institution) %>%
           fct_inorder()) %>%
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
      # color = cntry,
      # shape = condition,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  ggh4x::facet_manual(
    vars(institution),
    strip = nested_settings,
    axes = "margins",
    trim_blank = FALSE,
    remove_labels = "none",
    design = c(
      "
      HEFDGA
      BCIJK#
      "
  )
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
      "Probability(Category|Fraud) - Probability(Category|No Fraud)"
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
  )

ggsave(
  filename = paste0("figs/matching_cem.png"),
  height = 6,
  width = 10
)


### exact ######

baseline <- 0 # fraud1d == 0
plotting <- tibble()
exact <- readRDS("output/models_exact.RDS")
for (inst in 1:length(y_vars)) {
  esoph_plot <- exact[[inst]]$data %>%
    data_grid(fraud1d, cntry) %>%
    add_epred_draws(exact[[inst]],
                    category = "Trust"
    ) %>%
    filter(cntry == "ARG")

  plot_categories <- exact[[inst]]$data %>%
    data_grid(
      median = NA,
      lower = NA,
      upper = NA,
      fraud1d,
      cntry,
      category = 1:4
    ) %>%
    filter(cntry == "ARG")

  for (num in 1:4) {
    for (cntr in unique(plot_categories$cntry)) {
      for (cnd in unique(plot_categories$fraud1d)) {
        plot_categories[plot_categories$category == num &
                          plot_categories$cntry == cntr &
                          plot_categories$fraud1d == cnd, 1:3] <-
          median_hdci(esoph_plot$.epred[esoph_plot$fraud1d == baseline &
                                          esoph_plot$cntry == cntr &
                                          esoph_plot$Trust == num] -
                        esoph_plot$.epred[esoph_plot$fraud1d == cnd &
                                            esoph_plot$cntry == cntr &
                                            esoph_plot$Trust == num],
                      .width = ci
          )[1:3]

        plot_categories$institution <- y_vars[inst]
      }
    }
  }
  plotting <- bind_rows(plotting, plot_categories)
}

plotting_exact <- plotting %>%
  filter(fraud1d != baseline)

plotting %>%
  janitor::clean_names() %>%
  mutate(significant = ifelse(lower < 0 &
                                upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "inst_comp" ~ "Companies",
      institution == "inst_banks" ~ "Banks",
      institution == "inst_env" ~ "Environmental Organizations",
      institution == "inst_UN" ~ "United Nations",
      institution == "inst_wb" ~ "World Bank",
      institution == "inst_wto" ~ "WTO",
      institution == "inst_pres" ~ "President",
      institution == "inst_police" ~ "Police",
      institution == "inst_gov" ~ "Government",
      institution == "inst_parties" ~ "Political Parties",
      institution == "inst_parl" ~ "Parliament",
      institution == "inst_courts" ~ "Courts",
      institution == "inst_armed" ~ "Armed Forces",
    ),
    institution_facet_name = paste0(institution) %>%  fct_inorder(),
    category = case_when(
      category == 1 ~ "None\nat all",
      category == 2 ~ "Not very\nmuch",
      category == 3 ~ "Quite\na Lot",
      category == 4 ~ "A Great\nDeal",
    ) %>%
      fct_inorder()
  ) %>%
  arrange(institution) %>%
  filter(
    cntry %in% unique(plot_categories$cntry)[1],
    fraud1d == 1
  ) %>%
  mutate(institution = as_factor(institution) %>%
           fct_inorder()) %>%
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
      # color = cntry,
      # shape = condition,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  ggh4x::facet_manual(
    vars(institution),
    strip = nested_settings,
    axes = "margins",
    trim_blank = FALSE,
    remove_labels = "none",
    design = c(
      "
      HEFDGA
      BCIJK#
      "
    )
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
      "Probability(Category|Fraud) - Probability(Category|No Fraud)"
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
  )

ggsave(
  filename = paste0("figs/matching_exact.png"),
  height = 6,
  width = 10
)

## all matching together


plotting_all <-
  bind_cols(plotting_nn, type = "Nearest Neighbour") %>%
  bind_rows(
    plotting_cem %>%
      mutate(
        category = case_when(
          category == 1 ~ "None\nat all",
          category == 2 ~ "Not very\nmuch",
          category == 3 ~ "Quite\na Lot",
          category == 4 ~ "A Great\nDeal",
        )
      ) %>% bind_cols(., type = "Coarsened Exact")
  ) %>%
  bind_rows(plotting_exact %>%
              mutate(
                category = case_when(
                  category == 1 ~ "None\nat all",
                  category == 2 ~ "Not very\nmuch",
                  category == 3 ~ "Quite\na Lot",
                  category == 4 ~ "A Great\nDeal",
                )
              ) %>% bind_cols(., type = "Exact")) %>%
  janitor::clean_names() %>%
  mutate(significant = ifelse(lower < 0 &
                                upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "inst_comp" ~ "Companies",
      institution == "inst_banks" ~ "Banks",
      institution == "inst_env" ~ "Environmental Organizations",
      institution == "inst_UN" ~ "United Nations",
      institution == "inst_wb" ~ "World Bank",
      institution == "inst_wto" ~ "WTO",
      institution == "inst_pres" ~ "President",
      institution == "inst_police" ~ "Police",
      institution == "inst_gov" ~ "Government",
      institution == "inst_parties" ~ "Political Parties",
      institution == "inst_parl" ~ "Parliament",
      institution == "inst_courts" ~ "Courts",
      institution == "inst_armed" ~ "Armed Forces",
      TRUE  ~ institution
    ),
    institution_facet_name = paste0(institution) %>%  fct_inorder(),
    category = category %>%
      fct_inorder()
  ) %>%
  arrange(institution) %>%
  mutate(institution = as_factor(institution) %>%
           fct_inorder(),
         type = as_factor(type) %>% fct_inorder())

plotting_all %>%
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
      color = type,
      shape = type,
      alpha = significant
    ),
    position = position_dodge(1),
    size = 0.4
  ) +
  ggh4x::facet_manual(
    vars(institution),
    strip = nested_settings,
    axes = "margins",
    trim_blank = FALSE,
    remove_labels = "none",
    design = c(
      "
      HEFDGA
      BCIJK#
      "
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  # scale_shape_manual(values = c(16, 15, 17, 8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(
    y = "Confidence",
    x = paste0(
      "Probability(Category|Fraud) - Probability(Category|No Fraud)"
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
    # color = "none",
    alpha = "none",
    # shape = "none"
  )


ggsave(
  filename = paste0("figs/matching_all.png"),
  height = 6,
  width = 10
)
