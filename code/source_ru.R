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
  # options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
}
setup()


# 1. Loading Data ####
# 
data_ru <- read_csv("data/RU.csv") %>%
  full_join(read_rds("data/toloka.rds"), ., 
            by = c("case" = "CASE")) %>%
  filter(
    questnnr == "russia",
    response != 3,
    response != 2,
    response != 4,
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
                                 "Judicial Punishment"),
         condition1 = fct_collapse(condition,
                                   Punishment = c("Punishment",
                                                  "Judicial Punishment")))


pol <- data_ru %>%
  dplyr::select(starts_with("pol_inst")) %>%
  colnames()
npol <- data_ru %>%
  dplyr::select(starts_with("npol_inst")) %>%
  colnames()

source("code/functions.R")

# 4 conditions ####
# model_calc(data = data_ru,
#            inst = c(pol, npol),
#            IVs = "condition",
#            model = "ol",
#            cores = 1,
#            name = "ru_condition_234")

# # 4 conditions_opponent ####
model_calc(data = data_ru,
           inst = c(pol, npol),
           IVs = "condition*opponent",
           model = "ol",
           cores = 1,
           name = "ru_condition_opponent_234")
# 
# # # 4 conditions controls ####
# IVs <- c("condition +
#          opponent +
#          polint + gentrust + log(age) + sex + edu_three +
#          emplstat + sector + savings")
# 
# model_calc(
#   data = data_ru,
#   inst = c(pol, npol),
#   IVs = IVs,
#   cores = 1,
#   model = "ol",
#   name = "ru_controls"
# )
mpol <- read_rds("output/ol_ru_condition_234.rds")
