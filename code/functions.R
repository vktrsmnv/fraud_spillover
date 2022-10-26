matching <- function(z,
                     score,
                     shuffle = TRUE,
                     random_ties = TRUE) {
  # set.seed(1201) # to get the same random draws if shuffling
  n <- length(score)
  nt <- sum(z)
  nc <- sum(1 - z)
  names(score) <- 1:n # set names to index in the main dataset
  matched <- rep(0, nt)
  if (shuffle) {
    score <- sample(score, size = n, replace = F)
  }
  scorec <- score[z == 0]
  scoret <- score[z == 1]
  if (random_ties) { # if shuffle == TRUE, this may not even be necessary
    which.min <- function(x) {
      y <- which(x == min(x))
      if (length(y) > 1L) sample(y, 1L) else y
    }
  }
  for (i in 1:nt) {
    min_score <- which.min(abs(scorec - scoret[i]))
    scorec <- scorec[-min_score]
    matched[i] <-
      as.numeric(names(min_score)) # name in the main dataset
    if (length(scorec) == 0) {
      break
    }
  }

  matched_ind <- c(matched, as.numeric(names(scoret)[1:i]))
  cnts <- as.numeric(1:n %in% matched_ind)
  out <- list(match.ind = matched_ind, cnts = cnts)
  # print(score)
  return(out)
}

model_calc <- function(data,
                       inst,
                       IVs,
                       model = "ol",
                       iter = 20000,
                       cores = 4,
                       chains = 4,
                       warmup = 10000,
                       seed = 1201,
                       name,
                       prior) {
  # empty objects for storing
  mods <- list()
  # polinst_mods_ol <- npolinst_mods_ol <- list()

  # transform variables to factors for OL model
  data1 <- data %>%
    mutate(
      across(starts_with("pol_inst_"), ~ as.numeric(.x)),
      across(starts_with("npol_inst"), ~ as.numeric(.x))
    )

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")

  for (DV in inst) {
    if (match(DV, inst) == 1) {
      if (model == "ol") {
        mods[[DV]] <-
          first <-
          brm(
            paste(DV, IVs, sep = "~"),
            data = data1,
            family = cumulative("logit"),
            iter = iter,
            warmup = warmup,
            chains = chains,
            cores = cores,
            seed = seed,
            prior = prior
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
    } else {
      if (model == "ol") {
        mods[[DV]] <-
          update(first,
            formula. = paste(DV, IVs, sep = "~"),
            newdata = data1
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
    }
  }
}

mediation_calc <- function(data,
                           inst,
                           IVs,
                           model,
                           iter = 20000,
                           cores = 4,
                           chains = 4,
                           warmup = 10000,
                           seed = 1201,
                           name) {
  # empty objects for storing
  mods <- list()
  # polinst_mods_ol <- npolinst_mods_ol <- list()

  # transform variables to factors for OL model
  data1 <- data %>%
    mutate(
      across(starts_with("pol_inst_"), ~ as.numeric(.x)),
      across(starts_with("npol_inst"), ~ as.numeric(.x))
    ) #

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")
  for (DV in inst) {
    f1 <- bf(paste("pol_election", IVs, sep = "~"),
      family = "cumulative"
    )
    f2 <- bf(paste(DV,
      paste("pol_election", IVs, sep = "+"),
      sep = "~"
    ),
    family = "cumulative"
    )
    mods[[DV]] <-
      brm(
        formula = f1 + f2 + set_rescor(FALSE),
        data = data1,
        family = cumulative("logit"),
        iter = iter,
        warmup = warmup,
        chains = chains,
        cores = cores,
        seed = seed
      )
    write_rds(
      mods,
      paste0(
        "output/",
        model,
        "_",
        name,
        ".rds"
      )
    )
  }
}

prep_plotting <- function(path = "output/ol_main_ru_pol_1223.rds",
                          model = "condition",
                          output = "diffs",
                          baseline = "Fraud",
                          ci = 0.89,
                          PD = TRUE,
                          BF = FALSE) {
  models <- read_rds(path)
  insts <- names(models)
  plotting <- tibble()
  sup_opp <- tibble()

  for (inst in insts) {
    if (model == "condition") {
      esoph_plot <- models[[inst]]$data %>%
        data_grid(condition) %>%
        add_epred_draws(models[[inst]],
          category = "Trust"
        )

      plot_categories <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          category = 1:4
        )
      sup_op_diff <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          category = 1:4
        )
    }
    if (model == "condition + questnnr") {
      esoph_plot <- models[[inst]]$data %>%
        data_grid(condition, questnnr) %>%
        add_epred_draws(models[[inst]],
          category = "Trust"
        )

      plot_categories <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          questnnr,
          category = 1:4
        )
      sup_op_diff <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          category = 1:4
        )
    }
    if (model == "condition + opponent") {
      esoph_plot <- models[[inst]]$data %>%
        data_grid(condition, opponent) %>%
        add_epred_draws(models[[inst]],
                        category = "Trust"
        )

      plot_categories <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          opponent,
          category = 1:4
        )

      sup_op_diff <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          category = 1:4
        )
    }
    if (model == "condition + opponent + questnnr"){
      esoph_plot <- models[[inst]]$data %>%
        data_grid(condition, opponent, questnnr) %>%
        add_epred_draws(models[[inst]],
                        category = "Trust"
        )

      plot_categories <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          questnnr,
          opponent,
          category = 1:4
        )
      sup_op_diff <- models[[inst]]$data %>%
        data_grid(
          median = NA,
          lower = NA,
          upper = NA,
          condition,
          category = 1:4
        )
    }

    plot_categories$n <- nrow(models[[inst]]$data)

    if (output == "probs") {
      if (model == "condition + questnnr") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$questnnr)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                plot_categories$questnnr == cntr &
                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == cnd &
                  esoph_plot$questnnr == cntr &
                  esoph_plot$Trust == num],
                .width = ci
                )[1:3]

              plot_categories$institution <- inst
            }
          }
        }
      }
      if (model == "condition + opponent") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$opponent)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                                plot_categories$opponent == cntr &
                                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == cnd &
                                                esoph_plot$opponent == cntr &
                                                esoph_plot$Trust == num],
                            .width = ci
                )[1:3]

              plot_categories$institution <- inst
            }
          }
        }
      }
      if (model == "condition") {
        for (num in 1:4) {
          for (cnd in unique(plot_categories$condition)) {
            plot_categories[plot_categories$category == num &
              plot_categories$condition == cnd, 1:3] <-
              median_hdci(esoph_plot$.epred[esoph_plot$condition == cnd &
                esoph_plot$Trust == num],
              .width = ci
              )[1:3]
          }
        }
      }
      if (model == "condition + opponent + questnnr"){
        for (num in 1:4) {
          for (cntr in unique(plot_categories$questnnr)) {
            for (cnd in unique(plot_categories$condition)) {
              for (tp in unique(plot_categories$opponent)){
              plot_categories[plot_categories$category == num &
                                plot_categories$questnnr == cntr &
                                plot_categories$opponent == tp &
                                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == cnd &
                                                esoph_plot$questnnr == cntr &
                                                esoph_plot$opponent == tp &
                                                esoph_plot$Trust == num],
                            .width = ci
                )[1:3]

              plot_categories$institution <- inst
            }}
          }
        }
      }
    }
    if (output == "diffs") {
      if (model == "condition + questnnr") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$questnnr)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                plot_categories$questnnr == cntr &
                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == baseline &
                  esoph_plot$questnnr == cntr &
                  esoph_plot$Trust == num] -
                  esoph_plot$.epred[esoph_plot$condition == cnd &
                    esoph_plot$questnnr == cntr &
                    esoph_plot$Trust == num],
                .width = ci
                )[1:3]
            }
          }
        }
        plot_categories %<>% filter(condition != baseline)
      }
      if (model == "condition + opponent + questnnr") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$questnnr)) {
            for (cnd in unique(plot_categories$condition)) {
              for (tp in unique(plot_categories$opponent)){
              plot_categories[plot_categories$category == num &
                                plot_categories$questnnr == cntr &
                                plot_categories$opponent == tp &
                                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == baseline &
                                                esoph_plot$questnnr == cntr &
                                                esoph_plot$opponent == tp &
                                                esoph_plot$Trust == num] -
                              esoph_plot$.epred[esoph_plot$condition == cnd &
                                                  esoph_plot$opponent == tp &
                                                  esoph_plot$questnnr == cntr &
                                                  esoph_plot$Trust == num],
                            .width = ci
                )[1:3]
            }
          }
          }
        }
        plot_categories %<>% filter(condition != baseline)
      }
      if (model == "condition") {
        for (num in 1:4) {
          for (cnd in unique(plot_categories$condition)) {
            plot_categories[plot_categories$category == num &
              plot_categories$condition == cnd, 1:3] <-
              median_hdci(esoph_plot$.epred[esoph_plot$condition == baseline &
                esoph_plot$Trust == num] -
                esoph_plot$.epred[esoph_plot$condition == cnd &
                  esoph_plot$Trust == num],
              .width = ci
              )[1:3]
          }
        }
      }
      if (model == "condition + opponent") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$opponent)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                                plot_categories$opponent == cntr &
                                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == baseline &
                                                esoph_plot$opponent == cntr &
                                                esoph_plot$Trust == num] -
                              esoph_plot$.epred[esoph_plot$condition == cnd &
                                                  esoph_plot$opponent == cntr &
                                                  esoph_plot$Trust == num],
                            .width = ci
                )[1:3]
            }
          }
        }

        plot_categories %<>% filter(condition != baseline)

        for (num in 1:4) {
          for (cnd in unique(sup_op_diff$condition)) {
            sup_op_diff[sup_op_diff$category == num &
                          sup_op_diff$condition == cnd, 1:3] <-
              median_hdci((esoph_plot$.epred[esoph_plot$condition == baseline &
                                              esoph_plot$opponent == "Yes" &
                                              esoph_plot$Trust == num] -
                            esoph_plot$.epred[esoph_plot$condition == cnd &
                                                esoph_plot$opponent == "Yes" &
                                                esoph_plot$Trust == num]) -
                            (esoph_plot$.epred[esoph_plot$condition == baseline &
                                              esoph_plot$opponent == "No" &
                                              esoph_plot$Trust == num] -
                            esoph_plot$.epred[esoph_plot$condition == cnd &
                                                esoph_plot$opponent == "No" &
                                                esoph_plot$Trust == num]),
                          .width = ci
              )[1:3]
          }
        }
        sup_op_diff$institution <- inst

      }
      plot_categories %<>% filter(condition != baseline)

    }

    plot_categories$institution <- inst

    stats <- tibble()

    if (BF) {
    # calculate the Bayes factor for H0: beta <= 0 (ie HA: beta > 0)
    bf <- bayesfactor_parameters(models[[inst]], null = c(-Inf, 0)) %>%
      as_tibble() %>%
      mutate(inst = colnames(models[[inst]]$data)[1],
             BF = paste0(insight::format_bf(exp(log_BF), protect_ratio = T), ":\n",
                         effectsize::interpret_bf(exp(log_BF), include_value = F))) %>%
      pivot_wider(names_from = Parameter,
                  values_from = c(log_BF, BF)) %>%
      clean_names() %>%
      select(-effects, -component)
    stats <- bf
    }

    if (PD) {
      pds <- models[[inst]] %>%
        pd() %>%
        as_tibble() %>%
        mutate(inst = colnames(models[[inst]]$data)[1]) %>%
        pivot_wider(names_from = Parameter, values_from = pd) %>%
        clean_names()
      stats <- pds
    }

    if (BF & PD){
      stats <- pds %>% left_join(bf, .)
      }

    if(nrow(stats) > 0) {
      plot_categories %<>% bind_cols(stats)
    }

    plotting <- bind_rows(plot_categories, plotting)
    sup_opp <- bind_rows(sup_op_diff, sup_opp)
  }

  if (str_detect(string = insts[1], pattern = "npol")) {
    plotting %<>%
      mutate(significant = ifelse(lower < 0 & upper > 0, "no", "yes")) %>%
      mutate(
        institution = case_when(
          institution == "npol_inst_comp" ~ "Companies",
          institution == "npol_inst_banks" ~ "Banks",
          institution == "npol_inst_env" ~ "Environmental Organizations",
          institution == "npol_inst_UN" ~ "United Nations",
          institution == "npol_inst_WB" ~ "World Bank",
          institution == "npol_inst_WTO" ~ "WTO"
        ),
        institution_facet_name = paste0(institution, "\nN = ", n),
        Condition = condition %>%
          factor(., levels = c(
            "Control", "Fraud", "Punishment", "Judicial Punishment"
          )),
        category = case_when(
          category == 1 ~ "None\nat all",
          category == 2 ~ "Not very\nmuch",
          category == 3 ~ "Quite\na Lot",
          category == 4 ~ "A Great\nDeal",
        ) %>%
          fct_inorder()
      ) %>%
      arrange(institution) %>%
      mutate(institution_facet_name = fct_inorder(institution_facet_name))

    } else {
    plotting %<>%
      mutate(
        significant = ifelse(lower < 0 & upper > 0, "no", "yes"),
        institution = case_when(
          institution == "pol_inst_pres" ~ "President",
          institution == "pol_inst_police" ~ "Police",
          institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
          institution == "pol_inst_gov" ~ "Government",
          institution == "pol_inst_part" ~ "Political Parties",
          institution == "pol_inst_parl" ~ "Parliament",
          institution == "pol_inst_courts" ~ "Courts",
          institution == "pol_inst_armed" ~ "Armed Forces",
          institution == "pol_election" ~ "Elections",

        ),
        institution = as_factor(institution) %>%
          fct_relevel(
            "Elections",
            "Central Electoral\nCommission",
            "Political Parties",
            "Parliament",
            "President",
            "Government",
            "Courts",
            "Police",
            "Armed Forces"
          ),
        institution_facet_name = paste0(institution, "\nN = ", n),
        Condition = condition %>%
          factor(.,
            levels = c(
              "Control",
              "Fraud",
              "Punishment",
              "Judicial Punishment"
            )
          ),
        category = case_when(
          category == 1 ~ "None\nat all",
          category == 2 ~ "Not very\nmuch",
          category == 3 ~ "Quite\na Lot",
          category == 4 ~ "A Great\nDeal",
        ) %>%
          fct_inorder()
      )  %>%
      arrange(institution) %>%
      mutate(institution_facet_name = fct_inorder(institution_facet_name))
  }

  if (str_detect(model, pattern = "opponent")){
    plotting %<>%
      mutate(opponent = ifelse(
        opponent == "Yes",   "Opponent",
        "Supporter"
      ))
  }
  return(list("plotting" = plotting, "sup_opp" = sup_opp))
  }

setup <- function() {
  p_needed <- c(
    "bayesplot",
    "BayesPostEst",
    "bayestestR",
    "brms",
    "cobalt",
    "fastDummies",
    "ggdist",
    "ggthemes",
    "here",
    "hrbrthemes",
    "janitor",
    "lme4",
    "magrittr",
    "MatchIt",
    "modelr",
    "ordinal",
    "parallel",
    "patchwork",
    "remotes",
    "rstudioapi",
    "showtext",
    "sjlabelled",
    "stargazer",
    "styler",
    "tidybayes",
    "tidyverse",
    "tikzDevice",
    "viridis",
    "ggh4x"
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
      # legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.major = element_line(size = 0.2),
      panel.grid = element_line(size = 0.2),

      panel.grid.minor = element_blank(),
      panel.border = element_rect(size = 0.2, color = "grey40"),
      panel.background = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.ticks = element_blank(),
      # axis.ticks = element_line(color = "grey40"),
      strip.text = element_text(
        # face = "bold",
        size = rel(0.95), hjust = 0),
      strip.background = element_rect(fill = NA, color = NA),
      legend.title = element_text(face = "bold")
    ))

  formals(plasma)$end <- 0.8
  formals(scale_color_viridis)$end <- 0.8
  formals(scale_alpha_manual)$values <- c(0.5, 1)
}
setup()
