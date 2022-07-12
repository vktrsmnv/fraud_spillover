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


mediation_calc <- function(data,
                           inst,
                           IVs,
                           model,
                           iter = 10000,
                           cores = 4,
                           chains = 4,
                           warmup = 5500,
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
IVs <- "conditionControl + conditionJudicialPunishment + conditionPunishment"
mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_ru_pol_", nrow(data))
)


data <- data_la

data$condition <- str_replace_all(string = data$condition, pattern = " ", replacement = "")
data <- cbind(model.matrix( ~ condition - 1, data), data %>% select(starts_with("pol_")))
IVs <- "conditionControl + conditionJudicialPunishment + conditionPunishment"
mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_la_pol_", nrow(data))
)




plotting_mediation <- tibble()
condition <- c("conditionControl", "conditionJudicialPunishment",  "conditionPunishment")
for (md in pol){
  for (cn in condition){
    tt <- bayestestR::mediation(ol_mediation_la_pol_881[[md]],
                                treatment = cn,
                                mediator = "pol_election",
                                ci = .89)

    plotting_mediation <- tt %>%
      as_tibble() %>%
      add_column(institution = md,
                 treatment = cn,
                 n = nrow(ol_mediation_la_pol_881[[md]]$data)) %>%
      janitor::clean_names() %>%
      slice(1:4) %>%
      bind_rows(plotting_mediation)
  }

}



plotting_mediation %>%
  mutate(significant = ifelse(ci_low < 0 & ci_high > 0, "no", "yes"),
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
         treatment = treatment %>%
           str_remove("condition"),
         treatment = case_when(treatment == "JudicialPunishment" ~ "Judicial Punishment",
                               TRUE ~ treatment),
         treatment = treatment %>%
           factor(., levels = c(
             "Control", "Punishment", "Judicial Punishment"
           )
           )) %>%
  ggplot(aes(x = estimate, xmin = ci_low, xmax = ci_high,
             y = effect,
             shape = treatment,
             color = treatment,
             alpha = significant
  )) +
  geom_pointrange(
    position = position_dodge(0.5),
    size = 0.4
  ) +
  facet_wrap(
    . ~ institution,
    ncol = 4,
    labeller = label_glue(
      "{institution}, N = {plotting_mediation %>% select(institution, n) %>% distinct() %>% arrange(institution) %>% pull(n)}"
    )
  ) +
  scale_color_viridis(
    discrete = T,
    option = "C",
    end = 0.8
  ) +
  geom_vline(xintercept = 0,
             linetype = 2,
             color = "black",
             alpha = 0.5
  ) +
  labs(y = "",
       x = "Estimate",
       alpha = "",
       shape = "")+
  guides(alpha = "none", color = "none")
