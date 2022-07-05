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

}
setup()


# 1. Loading Data ####

data_rus <-
  read_csv(here("data/RU.csv")) %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE")) %>%
# data_rus <- read_rds(here("data/toloka.rds")) %>% # manually coded quality
  # full_join(read_rds(here("data/toloka.rds")), .,
  #           by = c("case" = "CASE")) %>%
  filter(
    questnnr == "russia",
    # response != 3,
    # response != 2,
    finished == 1, # as per PAP, only work with finished cases
    age > 17,
    time_sum > 180, # as per PAP, exclude shorter than 3 minutes response times
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

# # summary stats
# data_rus %>%
#   mutate(rural = case_when(rural %in% 1:4 ~ "Urban",
#                            TRUE ~ "Rural") %>% as.character()) %>%
#   select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
#          "age":"involvement", "condition") %>%
# modelsummary::datasummary_balance(data = .,
#                                   formula = ~condition,
#                                   output = "latex"
#   )


data_la <- read_csv(here("data/LA.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE")) %>%
  filter(
    questnnr != "russia",
    # response != 3,
    # response != 2,
    finished == 1,
    age > 17,
    time_sum > 180,
    # time_sum > 210, # 10th quantile
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
# data_la %>%
#   mutate(rural = case_when(rural %in% 1:4 ~ "Urban",
#                            TRUE ~ "Rural") %>% as.character()) %>%
#   select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
#          "age":"involvement", "condition") %>%
#   modelsummary::datasummary_balance(data = .,
#                                     formula = ~condition,
#                                     output = "latex_tabular"
#   )

pol <- data_rus %>%
  dplyr::select(starts_with("pol_inst")) %>%
  colnames()
npol <- data_rus %>%
  dplyr::select(starts_with("npol_inst")) %>%
  colnames()

# 2. Main Analysis ####
## 2.0. Calculation Function ####

source("code/functions.R")

## 2.1. Main Specification ####

# Russia: full sample
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_pol_", nrow(data))
)

model_calc(
  data = data,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_npol_", nrow(data))
)


# Russia: restriction on data quality #1
data <- data_rus %>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_npol_", nrow(data))
)

# Russia: restriction on data quality #2
data <- data_rus %>% filter(response != 3,  response != 2)

model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_npol_", nrow(data))
)

# LA: full sample
data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_", nrow(data))
)

model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_npol_", nrow(data))
)


# LA: restriction on data quality #1
data <- data_la %>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_npol_", nrow(data))
)

# LA: restriction on data quality #2
data <- data_la %>% filter(response != 3,  response != 2)

model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_npol_", nrow(data))
)


## Full combined sample
model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = "condition + questnnr",
  model = "ol",
  name = "main_pol"
)

# LA: dummy for country
model_calc(
  data = data_la,
  inst = pol,
  IVs = "condition + questnnr",
  model = "ol",
  name = "main_pol_fe"
)

model_calc(
  data = data_la[data_la$questnnr == "mexico",],
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = "main_la_pol_mexico"
)

model_calc(
  data = data_la[data_la$questnnr == "colombia",],
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = "main_la_pol_colombia"
)


## 2.2. Conditional Effects with Opposition to Regime ####

IVs <- c("condition*opponent")

data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_ru_npol_", nrow(data))
)


data <- data_rus %>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_ru_npol_", nrow(data))
)


data <- data_rus %>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_ru_npol_", nrow(data))
)



data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_la_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_la_npol_", nrow(data))
)


data <- data_la %>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_la_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_la_npol_", nrow(data))
)


data <- data_la %>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_la_pol_", nrow(data))
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_la_npol_", nrow(data))
)

# 3. Additional Specifications ####
## 3.3. With involvement ####

IVs <- c("condition*involvement")
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_ru_pol_", nrow(data))
)
data <- data_rus%>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_ru_pol_", nrow(data))
)
data <- data_rus%>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_ru_pol_", nrow(data))
)


data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_la_pol_", nrow(data))
)
data <- data_la%>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_la_pol_", nrow(data))
)
data <- data_la%>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_la_pol_", nrow(data))
)


## 3.3. With Controls ####

IVs <- c("condition +
         opponent +
         polint + gentrust + log(age) + sex + edu_three +
         emplstat + sector + savings")

data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_ru_pol_", nrow(data))
)

data <- data_rus%>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_ru_pol_", nrow(data))
)

data <- data_rus%>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_ru_pol_", nrow(data))
)

data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_la_pol_", nrow(data))
)
data <- data_la %>% filter(response != 3)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_la_pol_", nrow(data))
)
data <- data_la %>% filter(response != 3, response != 2)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_la_pol_", nrow(data))
)


#
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "ol"
# )
#
# IVs <- c("(fraud + punishment + judicial_punishment) *
#          opponent +
#          polint + gentrust + log(age) + sex + edu_three +
#          emplstat + sector + savings")
#
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "lm"
# )
#
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "ol"
# )
#
# IVs <- c("(fraud + punishment + judicial_punishment)*
#          polint + opponent")
#
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "lm"
# )
#
# model_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "ol"
# )
#
# 4. Mediation Analysis #####

mediation_calc <- function(data,
                           inst,
                           IVs,
                           model,
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
    mutate(across(starts_with("pol_inst_"), ~ as.numeric(.x)),
           across(starts_with("npol_inst"), ~ as.numeric(.x))) #

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")
  for (DV in inst) {
      f1 <- bf(paste("pol_election", IVs, sep = "~"),
               family = "cumulative")
      f2 <- bf(paste(DV,
                     paste("pol_election", IVs, sep = "+"),
                     sep = "~"),
               family = "cumulative")
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
      write_rds(mods,
                paste0("output/",
                       model,
                       "_",
                       name,
                       ".rds"))
    }

}

#
# IVs <- c("fraud + punishment + judicial_punishment")

data <- data_rus

data$condition <- str_replace_all(string = data$condition, pattern = " ", replacement = "")
data <- cbind(model.matrix( ~ condition - 1, data), data %>% select(starts_with("pol_")))
IVs <- "conditionFraud + conditionJudicialPunishment + conditionPunishment"
mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionFraud + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_ru_pol_", nrow(data))
)


data <- data_la

data$condition <- str_replace_all(string = data$condition, pattern = " ", replacement = "")
data <- cbind(model.matrix( ~ condition - 1, data), data %>% select(starts_with("pol_")))
IVs <- "conditionFraud + conditionJudicialPunishment + conditionPunishment"
mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionFraud + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_la_pol_", nrow(data))
)

#
# mm <- read_rds("output/ol_mediation_ru_pol_1226.rds")
# bayestestR::mediation(mm$pol_inst_armed,
#                       treatment = c("conditionControl"),
#                       mediator = "pol_election",
#                       ci = .95) %>%
#   as.data.frame()


#
# mediation_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = "condition",
#   npolinst = npolinst,
#   model = "ol"
# )
#
# mediation_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = "condition*opponent",
#   npolinst = npolinst,
#   model = "lm"
# )
#
# mediation_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = "condition*opponent",
#   npolinst = npolinst,
#   model = "ol"
# )
#
#
# IVs <- c("fraud + punishment + judicial_punishment")
#
# mediation_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "lm"
# )
#
# mediation_calc(
#   data = data_rus,
#   polinst = polinst,
#   IVs = IVs,
#   npolinst = npolinst,
#   model = "ol"
# )
#
# bayestestR::mediation(m2, ci = .95) %>%
#   as.data.frame()
#
# # 4. Plotting #####
# ### 4.1 EVs of the Posterior Predictive Distribution ####
#
# # Simple Model: institution ~ condition
# mpol <- read_rds("output/ol_condition_ru.rds")
#
#
# # file.remove("output/npolinst_mods_ol_Jul16_13_40_57.rds")
# # file.rename("output/polinst_mods_ol_Jul15_19_09_11.rds",
# #             to = "output/polinst_ol_condition2_ru.rds")
#
# plotting <- plotting2 <- tibble()
#
# for (i in pol) {
#   temp <- mpol[[i]] %>%
#     conditional_effects(
#       prob = 0.9,
#       method = "posterior_epred",
#       plot = FALSE
#     )
#   plotting <- temp[[1]] %>%
#     mutate(institution = i) %>%
#     dplyr::select(-i) %>%
#     bind_rows(., plotting)
#
#   plotting2 <- data_rus %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[i]]) %>%
#     mutate(institution = i) %>%
#     bind_rows(plotting2, .)
# }
#
#
# plotting2 %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President"
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       )
#   ) %>%
#   ggplot(aes(y = institution,
#              x = .value,
#              group = condition)) +
#   stat_pointinterval(
#     aes(
#       shape = condition,
#       color = condition,
#       fill = condition
#     ),
#     position = position_dodge(0.8),
#     size = 0.8,
#     # color = "grey20",
#   ) +
#   facet_grid(institution ~ .,
#              scales = "free_y",
#              space = "free") +
#   theme_bw() +
#   theme(
#     strip.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = "top"
#   ) +
#   scale_x_continuous(
#     limits = c(1, 4),
#     labels = c("None\nat all",
#                "Not very\nmuch",
#                "Quite\na Lot",
#                "A Great\nDeal")
#   ) +
#   viridis::scale_color_viridis(discrete = T,
#                                option = "C",
#                                end = 0.8) +
#   viridis::scale_fill_viridis(
#     discrete = T,
#     option = "C",
#     begin = 0.1,
#     end = 0.9,
#     alpha = 0.5
#   ) +
#   scale_shape_manual(values = c(15, 16, 17, 8)) +
#   labs(
#     y = "",
#     x = "",
#     shape = "",
#     color = "",
#     fill = "",
#     subtitle = "Expected Values of the Posterior Predictive Distribution"
#   )
#
# #### Interaction with the opponent
#
# m_pol <- read_rds("output/ol_condition_ru.rds")
#
# plotting3 <- plotting4 <- tibble()
#
# for (i in names(pol)) {
#   temp <- mpol[[i]] %>%
#     conditional_effects(
#       prob = 0.9,
#       method = "posterior_epred",
#       plot = FALSE
#     )
#   plotting4 <- temp[[3]] %>%
#     mutate(institution = i) %>%
#     dplyr::select(-i) %>%
#     bind_rows(., plotting4)
#
#   plotting3 <- data_rus %>%
#     data_grid(condition, opponent) %>%
#     add_fitted_draws(mpol[[i]]) %>%
#     mutate(institution = i) %>%
#     bind_rows(plotting3, .)
# }
#
# plotting2 <- plotting2 %>%
#   mutate(opponent = "Full Sample") %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President"
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       )
#   )
# plotting3 %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President"
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       ),
#     opponent = ifelse(opponent == 1, "Opponents", "Supporters")
#   ) %>%
#   ggplot(aes(y = institution, x = .value,
#              group = condition)) +
#   stat_pointinterval(aes(shape = condition,
#                    color = condition,
#                    fill = condition),
#                position = position_dodge(0.8),
#                size = 0.8,
#                # color = "grey20",
#   ) +
#   facet_grid(institution ~ opponent,
#              scales = "free_y",
#              space = "free"
#   ) +
#   theme_bw() +
#   theme(
#     strip.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = "top"
#   ) +
#   scale_x_continuous(
#     limits = c(1, 4),
#     labels = c(
#       "None\nat all",
#       "Not very\nmuch",
#       "Quite\na Lot",
#       "A Great\nDeal"
#     )
#   ) +
#   viridis::scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   viridis::scale_fill_viridis(
#     discrete = T,
#     option = "C",
#     begin = 0.1,
#     end = 0.9,
#     alpha = 0.5
#   ) +
#   scale_shape_manual(values = c(15, 16, 17, 8)) +
#   labs(
#     y = "",
#     x = "",
#     shape = "",
#     color = "",
#     fill="",
#     subtitle = "Expected Values of the Posterior Predictive Distribution"
#   )
#
#
#
#
#
#
#
# ### 4.1.2 ATE ######
# plotting4 %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President"
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       )
#   ) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = estimate__,
#     xmin = lower__,
#     xmax = upper__,
#     y = institution,
#     shape = condition,
#     color = condition
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(institution ~ .,
#              scales = "free_y",
#              space = "free"
#   ) +
#   ggthemes::theme_base() +
#   theme(
#     strip.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = "top"
#   ) +
#   scale_x_continuous(
#     limits = c(1, 4),
#     labels = c(
#       "None\nat all",
#       "Not very\nmuch",
#       "Quite\na Lot",
#       "A Great\nDeal"
#     )
#   ) +
#   viridis::scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(15, 16, 17, 8)) +
#   labs(
#     y = "",
#     x = "",
#     shape = "",
#     color = "",
#     subtitle = "Expected Values of the Posterior Predictive Distribution"
#   )
#
# #### OL ####
# mpol <- read_rds("output/ol_condition_ru.rds")
# m_pol_ol$pol_inst_armed$formula
#
# plotting4 <-plotting3 <- tibble()
# for (i in pol) {
#   temp <- mpol[[i]] %>%
#     conditional_effects(
#       prob = 0.9,
#       method = "posterior_epred",
#       plot = FALSE,
#       categorical = T
#     )
#   plotting4 <- temp[[1]] %>%
#     mutate(institution = i) %>%
#     dplyr::select(-i) %>%
#     bind_rows(., plotting4)
#
#   plotting3 <- data_rus %>%
#     data_grid(condition) %>%
#     add_fitted_draws(mpol[[i]]) %>%
#     mutate(institution = i) %>%
#     bind_rows(plotting3, .)
# }
#
#
#
#
# plotting <- tibble()
# for (i in pol) {
#   temp <- m_pol[[i]] %>%
#     conditional_effects(
#       prob = 0.9,
#       method = "posterior_epred",
#       plot = FALSE
#     )
#   plotting <- temp[[1]] %>%
#     mutate(institution = i) %>%
#     dplyr::select(-i) %>%
#     bind_rows(., plotting)
# }
#
# plotting4 %>%
#   mutate(
#     institution =
#       case_when(
#         institution == "pol_inst_armed" ~ "Armed Forces",
#         institution == "pol_inst_police" ~ "Police",
#         institution == "pol_inst_CEC" ~ "Central Electoral\nCommission",
#         institution == "pol_inst_gov" ~ "Government",
#         institution == "pol_inst_part" ~ "Parties",
#         institution == "pol_inst_parl" ~ "Parliament",
#         institution == "pol_inst_courts" ~ "Courts",
#         institution == "pol_inst_pres" ~ "President"
#       ),
#     institution = as_factor(institution) %>%
#       fct_relevel(
#         "Central Electoral\nCommission",
#         "Parties",
#         "Parliament",
#         "Courts",
#         "President",
#         "Government",
#         "Police",
#         "Armed Forces"
#       ),
#     # opponent = ifelse(opponent == 1, "Opponents", "Supporters")
#   ) %>%
#   ggplot() +
#   geom_pointrange(aes(
#     x = estimate__,
#     xmin = lower__,
#     xmax = upper__,
#     y = institution,
#     shape = condition,
#     color = condition
#   ),
#   position = position_dodge(1),
#   size = 0.4
#   ) +
#   facet_grid(institution ~ cats__,
#              scales = "free_y",
#              space = "free"
#   ) +
#   ggthemes::theme_base() +
#   theme(
#     strip.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = "top"
#   ) +
#   # scale_x_continuous(
#   #   limits = c(1, 4),
#   #   labels = c(
#   #     "None\nat all",
#   #     "Not very\nmuch",
#   #     "Quite\na Lot",
#   #     "A Great\nDeal"
#   #   )
#   # ) +
#   viridis::scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   scale_shape_manual(values = c(15, 16, 17, 8)) +
#   labs(
#     y = "",
#     x = "",
#     shape = "",
#     color = "",
#     subtitle = "Expected Values of the Posterior Predictive Distribution"
#   )
#
#
#
#
# # title = "Trust in Political Institutions in Russia",
# # caption = "Bayesian Posterior Densities,
# # Median and 95% Credible Intervals ")
#
#
#
# m_pol <- read_rds("output/polinst_mods_lm_Jul15_19_02_28.rds")
#
# plot <- m_pol$pol_inst_armed %>%
#   conditional_effects(prob = 0.9)
#
# m1$pol_inst_armed$data %>%
#   modelr::data_grid(condition) %>%
#   add_fitted_draws(m1$pol_inst_armed) %>%
#   ggplot(aes(x = .value, y = condition)) +
#   stat_gradientinterval(
#     thickness = 0.25,
#     position = "dodge"
#   )
#
# m_pol$pol_inst_armed$data %>%
#   modelr::data_grid(condition) %>%
#   add_predicted_draws(m_pol$pol_inst_armed) %>%
#   ggplot(aes(x = .prediction, y = condition)) +
#   stat_slab()
#
# m1$pol_inst_CEC$data %>%
#   modelr::data_grid(condition) %>%
#   add_fitted_draws(m1$pol_inst_CEC,
#     dpar = c("mu", "sigma")
#   ) %>%
#   sample_draws(50) %>%
#   ggplot(aes(y = condition)) +
#   stat_dist_slab(
#     aes(dist = "norm", arg1 = mu, arg2 = sigma),
#     slab_color = "gray65",
#     alpha = 1 / 10,
#     fill = NA
#   ) +
#   geom_point(
#     aes(x = response),
#     data = data_rus,
#     shape = 21,
#     fill = "#9ECAE1",
#     size = 2
#   )
#
# library(tidybayes)
#
#
# # m1$pol_inst_armed %>%
# #   mcmc_intervals_data(regex_pars = "b_",
# #                  prob = 0.9, # 80% intervals
# #                  prob_outer = 0.99, # 99%
# #                  point_est = "mean") %>%
# #   ggplot(mapping = aes(y = parameter, x = point_est)) +
# #   stat_gradientinterval()
#
#
# estimates <- tibble()
# for (i in polinst) {
#   estimates <- bind_rows(
#     m1[[i]] %>%
#       mcmc_intervals_data(
#         prob_outer = 0.95,
#         pars = c(
#           "fraud1",
#           "punishment1",
#           "punishment_judicial"
#         )
#       ) %>%
#       mutate(model = j, institution = i),
#     estimates
#   )
# }
#
# for (i in npolinst) {
#   estimates <- bind_rows(
#     FP_rus$NONpol_institutions[[j]][[i]] %>%
#       mcmc_intervals_data(
#         prob_outer = 0.9,
#         pars = c(
#           "fraud1",
#           "punishment1",
#           "punishment_judicial"
#         )
#       ) %>%
#       mutate(model = j, institution = i),
#     estimates
#   )
# }
#
# estimates$model <- ifelse(estimates$model == 1, "Linear", "Ordered")
#
#
# ggplot(estimates) +
#   geom_pointrange(aes(
#     x = m,
#     xmin = ll,
#     xmax = hh,
#     y = institution,
#     shape = parameter,
#     color = parameter
#   ),
#   position = position_dodge(1)
#   ) +
#   facet_grid(institution ~ model, scales = "free_y") +
#   theme(strip.text.y = element_blank()) +
#   geom_vline(aes(xintercept = 0), lty = 2) +
#   # scale_x_continuous(
#   #   limits = c(1, 4),
#   #   labels =  c("None\nat all",
#   #               "Not very\nmuch",
#   #               "Quite\na Lot",
#   #               "A Great\nDeal")
#   # ) +
#   viridis::scale_color_viridis(
#     discrete = T,
#     option = "C",
#     end = 0.8
#   ) +
#   labs(
#     y = "",
#     x = "",
#     title = "Trust in Institutions in Russia",
#     subtitle = FP_rus$pol_institutions[[1]]$pol_inst_army$formula %>%
#       str_remove(".*~"),
#     caption = "Bayesian Posterior Densities,
#        Median and 90% Credible Intervals"
#   ) +
#   theme(legend.position = "top")
#
# summary(m_pol_ol$pol_inst_armed)
#
#
# # plotting3 %>%
# #   dplyr::filter(institution == "pol_inst_armed",
# #                 fraud == 0,
# #                 punishment == 0,
# #                 judicial_punishment == 0)
#
#
# mpol$pol_inst_pres
#
# # esoph_plot = data_rus %>%
# #   data_grid(condition) %>%
# #   add_fitted_draws(mpol$pol_inst_courts,
# #                    category = "Trust") %>%
# #   ggplot(aes(x = .value, y = Trust)) +
# #   coord_cartesian(expand = FALSE)
# #
# # esoph_plot +
# #   facet_grid(. ~ condition,
# #              switch = "x") +
# #   theme_classic() +
# #   theme(strip.background = element_blank(), strip.placement = "outside")  +
# #   stat_ccdfinterval()
# #   stat_summary(fun = median,
# #                geom = "bar",
# #                fill = "gray65", width = 1, color = "white") +
# #   stat_pointinterval()
# #
# #   mpol
