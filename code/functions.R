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
                       iter = 6000,
                       cores = 4,
                       chains = 4,
                       warmup = 3500,
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
    )

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")

  for (DV in inst) {
    if (match(DV, inst) == 1) {
      if (model == "lm") {
        mods[[DV]] <-
          first <-
          brm(
            formula = paste(DV, IVs, sep = "~"),
            data = data,
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
    } else {
      if (model == "lm") {
        mods[[DV]] <-
          update(first,
            formula. = paste(DV, IVs, sep = "~"),
            newdata = data
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
                           iter = 10000,
                           cores = 4,
                           chains = 4,
                           warmup = 7500,
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
                          ci = 0.89) {
  models <- read_rds(path)
  insts <- names(models)
  plotting <- tibble()

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
    }
    if (output == "diffs") {
      if (model == "condition + questnnr") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$questnnr)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                plot_categories$questnnr == cntr &
                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == "Fraud" &
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
        plot_categories %<>% filter(condition != "Fraud")
      }
      if (model == "condition + opponent") {
        for (num in 1:4) {
          for (cntr in unique(plot_categories$opponent)) {
            for (cnd in unique(plot_categories$condition)) {
              plot_categories[plot_categories$category == num &
                                plot_categories$opponent == cntr &
                                plot_categories$condition == cnd, 1:3] <-
                median_hdci(esoph_plot$.epred[esoph_plot$condition == "Fraud" &
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
        plot_categories %<>% filter(condition != "Fraud")
      }
      if (model == "condition") {
        for (num in 1:4) {
          for (cnd in unique(plot_categories$condition)) {
            plot_categories[plot_categories$category == num &
              plot_categories$condition == cnd, 1:3] <-
              median_hdci(esoph_plot$.epred[esoph_plot$condition == "Fraud" &
                esoph_plot$Trust == num] -
                esoph_plot$.epred[esoph_plot$condition == cnd &
                  esoph_plot$Trust == num],
              .width = ci
              )[1:3]
          }
        }
      }
      plot_categories %<>% filter(condition != "Fraud")
    }

    plot_categories$institution <- inst
    plotting <- bind_rows(plot_categories, plotting)
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
        institution_facet_name = paste0(institution, "\n N = ", n),
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
        institution_facet_name = paste0(institution, "\n N = ", n),
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
  return(plotting)
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
    "viridis"
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

  formals(plasma)$end <- 0.8
  formals(scale_color_viridis)$end <- 0.8
  formals(scale_alpha_manual)$values <- c(0.5, 1)
}
setup()
