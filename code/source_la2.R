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
    "bayesplot",
    "brms",
    "bayestestR",
    "parallel",
    "tidybayes",
    "modelr"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  lapply(p_needed, require, character.only = TRUE)
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
}
setup()


# 1. Loading Data ####

data_rus <- read_csv("data/RU.csv") %>%
  full_join(read_rds("data/toloka.rds"), ., by = c("case" = "CASE")) %>%
  filter(
    questnnr == "russia",
    response != 3,
    finished == 1,
    age > 17,
    time_sum > 180, # 10th quantile
  ) %>%
  select(-pa12, -pa17) %>%
  mutate(opponent = as.character(opponent),
         fraud = as.character(fraud),
         punishment = as.character(punishment),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition, 
                                 "Fraud", 
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment"))

data_la <- read_csv("data/LA.csv") %>%
  full_join(read_rds("data/toloka.rds"), .,
            by = c("case" = "CASE")) %>%
  filter(
    questnnr != "russia",
    response != 3,
    finished == 1,
    age > 17,
    time_sum > 180,
    # time_sum > 210, # 10th quantile
  ) %>%
  select(-involvement, -pa14) %>%
  mutate(opponent = as.character(opponent),
         fraud = as.character(fraud),
         punishment = as.character(punishment),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition, 
                                 "Fraud", 
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment"))


pol <- data_rus %>%
  dplyr::select(starts_with("pol_inst")) %>%
  colnames()
npol <- data_rus %>%
  dplyr::select(starts_with("npol_inst")) %>%
  colnames()


## Models to run:
# 1. LM
# 2. OL
# 3. with/without controls
# 4.

# 2. Main Analysis ####
## 2.0. Calculation Function ####

model_calc <- function(data,
                       polinst,
                       npolinst,
                       IVs,
                       model,
                       name) {
  
  # empty objects for storing
  polinst_mods_lm <- npolinst_mods_lm <- list()
  polinst_mods_ol <- npolinst_mods_ol <- list()
  
  # transform variables to factors for OL model
  data1 <- data %>%
    mutate(
      across(starts_with("pol_inst_"), ~ as.numeric(.x)),
      across(starts_with("npol_inst"), ~ as.numeric(.x))
    )
  # time <- format(Sys.time(), "%b%d_%H_%M_%S")
  if (!is.null(polinst)) {
    for (DV in polinst) {
      if (model == "lm") {
        polinst_mods_lm[[DV]] <-
          brm(
            formula = paste(DV, IVs, sep = "~"),
            data = data,
            iter = 4000,
            warmup = 2000,
            chains = 5,
            cores = 5,
            seed = 1201
          )
        write_rds(
          polinst_mods_lm,
          paste0(
            "output/polinst_lm_",
            name, ".rds"
          )
        )
      }
      if (model == "ol") {
        polinst_mods_ol[[DV]] <-
          brm(
            paste(DV, IVs, sep = "~"),
            data = data1,
            family = cumulative("logit"),
            iter = 5000,
            warmup = 3000,
            chains = 5,
            cores = 5,
            seed = 1201
          )
        write_rds(
          polinst_mods_ol,
          paste0(
            "output/polinst_ol_",
            name, ".rds"
          )
        )
      }
    }
  }
  if (!is.null(npolinst)) {
    if (model == "lm") {
      for (DV in npolinst) {
        npolinst_mods_lm[[DV]] <-
          brm(
            formula = paste(DV, IVs, sep = "~"),
            data = data,
            iter = 4000,
            warmup = 2000,
            chains = 5,
            cores = 5,
            seed = 1201
          )
        write_rds(
          npolinst_mods_lm,
          paste0(
            "output/npolinst_lm_",
            name, ".rds"
          )
        )
      }
    }
    if (model == "ol") {
      for (DV in npolinst) {
        npolinst_mods_ol[[DV]] <-
          brm(
            paste(DV, IVs, sep = "~"),
            data = data1,
            family = cumulative("logit"),
            iter = 5000,
            warmup = 3000,
            chains = 5,
            cores = 5,
            seed = 1201
          )
        write_rds(
          npolinst_mods_ol,
          paste0(
            "output/npolinst_ol_",
            name, ".rds"
          )
        )
      }
    }
  }
}


## 2.1. Basic Specification ####

# basic <- "~ (fraud + punishment + judicial_punishment)"
# controls <- "+ polint + gentrust + log(age) + sex + edu_three +
# as.factor(emplstat) + as.factor(sector) + savings +
# rural + opponent"

# conditional means
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = "condition",
#   npolinst = npolinst,
#   model = "lm"
# )

# model_calc(
#   data = data_la,
#   polinst = pol,
#   IVs = "condition",
#   npolinst = npol,
#   model = "lm",
#   name = "condition_la"
# )

# model_calc(
#   data = data_la,
#   polinst = pol,
#   IVs = "condition",
#   npolinst = npol,
#   model = "ol",
#   name = "condition_la"
# )


model_calc(
  data = data_la,
  polinst = pol,
  IVs = "condition*opponent",
  npolinst = npol,
  model = "ol",
  name = "condition_opponent_la"
)