# install.packages(c("StanHeaders", "rstan"))
source("code/functions.R")

# 1. Loading Data ####

data_rus <-
  read_csv(here("data/RU.csv")) %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE")) %>%
  filter(
    questnnr == "russia",
    finished == 1, # complete cases
    sd12 == 1, # citizens of the country
    age > 17, # above 18
    time_sum >= 180 # 3 minutes or more per interview
  ) %>%
  select(-pa12, -pa17) %>%
  mutate(opponent = as.character(opponent),
         UR = ifelse(pa14 == "UR", "UR", "Non-UR"),
         CPRF = ifelse(pa14 == "CPRF", "CPRF", "Non-CPRF"),
         fraud = as.character(fraud),
         punishment = as.character(punishment),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment"))

data_la <- read_csv(here("data/LA_1.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE")) %>%
  filter(
    questnnr != "russia",
    finished == 1,
    sd12 == 1,
    age > 17,
    time_sum >= 180
  ) %>%
  select(-pa14) %>%
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


data_comb <- data_la %>% bind_rows(., data_rus) %>%
  mutate(condition = fct_collapse(condition,
                                  Punishment = c("Punishment",
                                                 "Judicial Punishment")))

# # 2. Main Analysis ####
# ## 2.0. Calculation Function ####
#
# # source("code/functions.R")
#
# ## 2.1. Main Specification ####
#
# ### Russia: full sample ####
# data <- data_rus
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_ru_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data,
#   IVs = "condition",
#   inst = npol,
#   model = "ol",
#   name = paste0("main_ru_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# #
# # ### Russia: restriction on data quality #1
# # data <- data_rus %>% filter(attention_check != "Unacceptable")
# # model_calc(
# #   data = data,
# #   inst = pol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = paste0("main_ru_pol_", nrow(data)),
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
# # model_calc(
# #   data = data,
# #   inst = npol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = paste0("main_ru_npol_", nrow(data)),
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
#
# ### Russia: only correct summaries ####
# data <- data_rus %>% filter(attention_check == "Summary")
#
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_ru_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_ru_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Full sample (one punishment) ####
# model_calc(
#   data = data_rus,
#   inst = pol,
#   cores = 4,
#   IVs = "condition",
#   model = "ol",
#   name = "main_ru_pol_collapsed",
#   prior = prior(normal(0, 5),
#                 class = "b") +
#     prior(normal(0, 5),
#           class = "Intercept")
# )
#
#
# ### LA: full sample ####
# data <- data_la
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_la_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_la_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Full sample (one punishment) ####
# model_calc(
#   data = data_la,
#   inst = pol,
#   cores = 4,
#   IVs = "condition",
#   model = "ol",
#   name = "main_la_pol_collapsed",
#   prior = prior(normal(0, 5),
#                 class = "b") +
#     prior(normal(0, 5),
#           class = "Intercept")
# )
#
# # ### LA: restriction on data quality #1
# # data <- data_la %>% filter(attention_check != "Unacceptable")
# # model_calc(
# #   data = data,
# #   inst = pol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = paste0("main_la_pol_", nrow(data)),
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
# # model_calc(
# #   data = data,
# #   inst = npol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = paste0("main_la_npol_", nrow(data)),
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
#
# ### LA: only correct summaries ####
# data <- data_la %>% filter(attention_check == "Summary")
#
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_la_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_la_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### LA: fixed effects ####
# model_calc(
#   data = data_la,
#   inst = pol,
#   IVs = "condition + questnnr",
#   model = "ol",
#   name = "main_pol_la_fe",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### LA: country-varying effects ####
# model_calc(
#   data = data_la,
#   inst = pol,
#   IVs = "condition * questnnr",
#   model = "ol",
#   name = "main_pol_la_int",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
#
# ### Full sample ####
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = "main_pol",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Full sample (one punishment) ####
# model_calc(
#   data = data_comb,
#   inst = pol,
#   cores = 4,
#   IVs = "condition",
#   model = "ol",
#   name = "main_pol_collapsed",
#   prior = prior(normal(0, 5),
#                 class = "b") +
#     prior(normal(0, 5),
#           class = "Intercept")
# )
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = npol,
#   IVs = "condition",
#   model = "ol",
#   name = "main_npol",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
# #
# # ### Full sample with FE
# # model_calc(
# #   data = data_la %>% bind_rows(., data_rus),
# #   inst = pol,
# #   IVs = "condition + questnnr",
# #   model = "ol",
# #   name = "main_pol_fe",
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
# #
# # model_calc(
# #   data = data_la %>% bind_rows(., data_rus),
# #   inst = npol,
# #   IVs = "condition + questnnr",
# #   model = "ol",
# #   name = "main_npol_fe",
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
#
# ### Full sample with interaction ####
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = pol,
#   IVs = "condition * questnnr",
#   model = "ol",
#   name = "main_pol_int",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = npol,
#   IVs = "condition * questnnr",
#   model = "ol",
#   name = "main_npol_int",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
#
# ### Full sample  (correct only) ####
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus) %>%
#     filter(attention_check == "Summary"),
#   inst = pol,
#   cores = 4,
#   IVs = "condition",
#   model = "ol",
#   name = "main_pol_correct",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Full combined sample with interaction (correct only) ####
# model_calc(
#   data = data_la %>% bind_rows(., data_rus) %>%
#     filter(attention_check == "Summary"),
#   inst = pol,
#   IVs = "condition * questnnr",
#   model = "ol",
#   name = "main_pol_int_correct",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
# #
# # ### Mexico only ####
# # model_calc(
# #   data = data_la[data_la$questnnr == "mexico",],
# #   inst = pol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = "main_la_pol_mexico",
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )
# #
# # ### Colombia only ####
# # model_calc(
# #   data = data_la[data_la$questnnr == "colombia",],
# #   inst = pol,
# #   IVs = "condition",
# #   model = "ol",
# #   name = "main_la_pol_colombia",
# #   prior = prior(normal(0, 5), class = "b") +
# #     prior(normal(0, 5), class = "Intercept")
# # )

### Russia: hierarchical ####
data <- data_rus %>%
  mutate(condition = as.character(condition))

model_calc(
  data = data,
  inst = pol,
  IVs = "(1 | condition)",
  model = "ol",
  name = paste0("main_ru_pol_ml_", nrow(data)),
  prior =
    prior(normal(0, 5), class = "Intercept")

)

### LA: hierarchical ####
data <- data_la %>%
  mutate(condition = as.character(condition))

model_calc(
  data = data,
  inst = pol,
  IVs = "(1 | condition)",
  model = "ol",
  name = paste0("main_la_pol_ml_", nrow(data)),
  prior =
    prior(normal(0, 5), class = "Intercept")

)
