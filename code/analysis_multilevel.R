# m = brm(
#   pol_inst_armed ~ (1|condition), 
#   data = data_rus, 
#   prior = c(
#     prior(normal(0, 1), class = Intercept),
#     prior(student_t(3, 0, 1), class = sd),
#     prior(student_t(3, 0, 1), class = sigma)
#   ),
#   iter = 12000,
#   warmup = 10000,
#   cores = 5,
#   control = list(adapt_delta = .99),
# )
# 
# m1 <- data_rus %>%
#   mutate(condition_opponent = str_c(condition, opponent, sep = "_")) %>% 
#   brm(
#     pol_inst_armed ~ (1| condition_opponent), 
#     data = ., 
#     prior = c(
#       prior(normal(0, 1), class = Intercept),
#       prior(student_t(3, 0, 1), class = sd),
#       prior(student_t(3, 0, 1), class = sigma)
#     ),
#     iter = 5000,
#     warmup = 3000,
#     cores = 5,
#     control = list(adapt_delta = .99),
#   )
# 
# 
# 
# grid = data_rus %>%
#   data_grid(condition) 
# 
# fits = grid %>%
#   add_fitted_draws(m1$pol_inst_CEC)
# 
# preds = grid %>%
#   add_predicted_draws(m1$pol_inst_CEC)
# 
# data_rus %>%
#   ggplot(aes(y = condition, x = response)) +
#   # stat_interval(aes(x = .prediction), data = preds) +
#   stat_pointinterval(aes(x = .value), data = fits, 
#                      .width = c(.66, .95)) +
#   # geom_point() +
#   coord_flip()+
#   scale_color_brewer() +
#   xlim(1,4)

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

# 
# # 1. Loading Data ####
# 
# data_rus <- read_csv("data/RU.csv") %>%
#   full_join(read_rds("data/toloka.rds"), ., by = c("case" = "CASE")) %>%
#   filter(
#     questnnr == "russia",
#     response != 3,
#     finished == 1,
#     age > 17,
#     time_sum > 180, # 10th quantile
#   ) %>%
#   select(-pa12, -pa17) %>%
#   mutate(opponent = as.character(opponent),
#          fraud = as.character(fraud),
#          punishment = as.character(punishment),
#          judicial_punishment = as.character(judicial_punishment),
#          condition = fct_relevel(condition, 
#                                  "Fraud", 
#                                  "Control",
#                                  "Punishment",
#                                  "Judicial Punishment"))
# 
# pol <- data_rus %>%
#   dplyr::select(starts_with("pol_inst")) %>%
#   colnames()
# 
# npol <- data_rus %>%
#   dplyr::select(starts_with("npol_inst")) %>%
#   colnames()
# 
# model_calc2 <- function(data,
#                        polinst,
#                        npolinst,
#                        IVs,
#                        model,
#                        iter,
#                        cores,
#                        chains,
#                        warmup,
#                        name) {
#   
#   # empty objects for storing
#   polinst_mods_lm <- npolinst_mods_lm <- list()
#   polinst_mods_ol <- npolinst_mods_ol <- list()
#   
#   # transform variables to factors for OL model
#   data1 <- data %>%
#     mutate(
#       across(starts_with("pol_inst_"), ~ as.numeric(.x)),
#       across(starts_with("npol_inst"), ~ as.numeric(.x))
#     )
#   # time <- format(Sys.time(), "%b%d_%H_%M_%S")
#   if (!is.null(polinst)) {
#     for (DV in polinst) {
#       if (model == "lm") {
#         polinst_mods_lm[[DV]] <-
#           brm(
#             formula = paste0(DV, "~ (1|", IVs, ")"), # (1|condition)
#             data = data,
#             iter = iter,
#             warmup = warmup,
#             chains = chains,
#             cores = cores,
#             seed = 1201,
#             control = list(adapt_delta = .99)
#           )
#         write_rds(
#           polinst_mods_lm,
#           paste0(
#             "output/polinst_lm_",
#             name, ".rds"
#           )
#         )
#       }
#       if (model == "ol") {
#         polinst_mods_ol[[DV]] <-
#           brm(
#             paste0(DV, "~ (1|", IVs, ")"),
#             data = data1,
#             family = cumulative("logit"),
#             iter = iter,
#             warmup = warmup,
#             chains = chains,
#             cores = cores,
#             seed = 1201,
#             control = list(adapt_delta = .99)
#           )
#         write_rds(
#           polinst_mods_ol,
#           paste0(
#             "output/polinst_ol_",
#             name, ".rds"
#           )
#         )
#       }
#     }
#   }
#   if (!is.null(npolinst)) {
#     if (model == "lm") {
#       for (DV in npolinst) {
#         npolinst_mods_lm[[DV]] <-
#           brm(
#             formula = paste0(DV, "~ (1|", IVs, ")"),
#             data = data,
#             iter = iter,
#             warmup = warmup,
#             chains = chains,
#             cores = cores,
#             control = list(adapt_delta = .99),
#             seed = 1201
#           )
#         write_rds(
#           npolinst_mods_lm,
#           paste0(
#             "output/npolinst_lm_",
#             name, ".rds"
#           )
#         )
#       }
#     }
#     if (model == "ol") {
#       for (DV in npolinst) {
#         npolinst_mods_ol[[DV]] <-
#           brm(
#             paste0(DV, "~ (1|", IVs, ")"),
#             data = data1,
#             family = cumulative("logit"),
#             iter = iter,
#             warmup = warmup,
#             chains = chains,
#             cores = cores,
#             control = list(adapt_delta = .99),
#             seed = 1201
#           )
#         write_rds(
#           npolinst_mods_ol,
#           paste0(
#             "output/npolinst_ol_",
#             name, ".rds"
#           )
#         )
#       }
#     }
#   }
# }
# 
# 
# 
# model_calc2(
#   data = data_rus,
#   polinst = pol,
#   IVs = "condition",
#   npolinst = npol,
#   model = "ol",
#   name = "multilevel",
#   iter = 20000,
#   cores = 5,
#   chains = 5,
#   warmup = 18000
# )
# 
# 
# 
# mpol <- read_rds("output/polinst_ol_condition_la.rds")
# mpol <- read_rds("output/polinst_ol_condition_new_ru.rds")

# 
# esoph_plot = data_rus %>%
#   data_grid(condition) %>%
#   add_fitted_draws(mpol$pol_inst_pres, 
#                    category = "Trust")
# 
# 
# 
# esoph_plot = data_rus %>%
#   data_grid(condition) %>%
#   add_fitted_draws(mpol$pol_inst_pres, 
#                    category = "Trust")
# 
# 
# plot_categories <- tibble(lower = rep(NA, 12),
#                           median = rep(NA, 12),
#                           upper = rep(NA, 12),
#                           
#                           category = rep(1:4, 3),
#                           condition = rep(c("Control",
#                                             "Punishment",
#                                             "Judicial Punishment"), 
#                                           each = 4)) %>% 
#   as.data.frame()

plotting <- tibble()
for (inst in pol){
  esoph_plot = data_rus %>%
    data_grid(condition) %>%
    add_fitted_draws(mpol[[inst]], 
                     category = "Trust") 
  plot_categories <- tibble(lower = rep(NA, 12),
                            median = rep(NA, 12),
                            upper = rep(NA, 12),
                            category = rep(1:4, 3),
                            condition = rep(c("Control",
                                              "Punishment",
                                              "Judicial Punishment"), 
                                            each = 4)) %>% 
    as.data.frame()
  
  for (cond in levels(data_rus$condition)[2:4]){
    for (num in 1:4){
        plot_categories[plot_categories$category == num &
                          plot_categories$condition == cond, 1:3] <- 
          quantile(esoph_plot$.value[esoph_plot$condition == "Fraud" &
                                       esoph_plot$Trust == num 
                                     ] -
                     esoph_plot$.value[esoph_plot$condition == cond &
                                         esoph_plot$Trust == num 
                                       ], 
                   c(0.05, 0.5, 0.95))
        plot_categories$institution <- inst
      }
  }
  plotting <- bind_rows(plot_categories, plotting)
}

plotting <- plotting %>%
mutate(
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
    ))

ggplot(plotting, aes(y = category, 
                            x = median,
                            group = condition)) +
  geom_pointrange(aes(xmin = lower,
                      xmax = upper,
                      color = condition),
                  position = position_dodge(0.3)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Change in Probabilities for Trust Ranking Categories",
       subtitle = "Sample: Latin America",
       x = "Pr(Category|Fraud) - Pr(Category|Condition)",
       y = "") +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("None\nat all",
               "Not very\nmuch",
               "Quite\na Lot",
               "A Great\nDeal")
  ) +
  geom_vline(aes(xintercept = 0), alpha = 0.3) +
  facet_wrap(~ institution, 
             nrow = 2) +
  viridis::scale_color_viridis(discrete = T) -> plot

ggsave(plot, 
       filename = "plot_la.png",
       height = 6,
       width = 10)

# changes in probabilities for each category 


