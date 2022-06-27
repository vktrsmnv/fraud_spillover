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
   "stargazer",
   "estimatr"
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

data_rus <- read_csv(here("data/RU.csv")) %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE"))  %>%
  filter(questnnr == "russia")

data_la <- read_csv(here("data/LA.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
            by = c("case" = "CASE")) %>%
  filter(questnnr != "russia")

# completion rate
round(prop.table(table(data_rus$finished)), 3)
round(prop.table(table(data_la$finished)), 3)

# dropout across the conditions
data_rus %>%
  select(-pa12, -pa17) %>%
  mutate(opponent = as.character(opponent),
         fraud = as.character(fraud),
         punishment = as.character(punishment),
         drop_after_treatment = ifelse(!is.na(time004) & is.na(time005), 1, 0),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment")) %>%
  mutate(rural = case_when(rural %in% 1:4 ~ "Urban",
                           TRUE ~ "Rural") %>% as.character()) %>%
  select(drop_after_treatment, finished, condition) %>%
  # select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
  #        "age":"involvement", "condition") %>%
  modelsummary::datasummary_crosstab(data = .,
                                    formula = condition~drop_after_treatment,
                                    fmt = 2,
                                    output = "latex"
  )

# dropout across the conditions
data_la %>%
  mutate(condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment"),
         drop_after_treatment = ifelse(!is.na(time004) & is.na(time005), 1, 0),
  ) %>%
  select(finished,  drop_after_treatment, condition) %>%
  modelsummary::datasummary_crosstab(data = .,
                                     formula = condition~drop_after_treatment,
                                     fmt = 2,
                                     output = "latex"
  )

# time required
data_rus %>%
  filter(finished == 1,
         questnnr == "russia") %>%
  select(-pa12, -pa17) %>%
  mutate(opponent = as.character(opponent),
         fraud = as.character(fraud),
         punishment = as.character(punishment),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment")) %>%
  select(response) %>%
  modelsummary::datasummary_skim(data = .,
                                     # formula = ~ Percent(),
                                     fmt = 2,
                                     # output = "latex"
  )

data_rus %>%
  filter(finished == 1) %>%
  mutate(condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment")) %>%
  select(condition, time_sum, time004, time005) %>%
  modelsummary::datasummary_balance(data = .,
                                    formula = ~ condition,
                                    fmt = 2,
                                    output = "latex"
  )

data_la %>%
  filter(finished == 1) %>%
  mutate(condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment")) %>%
  select(condition, time_sum, time004, time005) %>%
  modelsummary::datasummary_balance(data = .,
                                 formula = ~ condition,
                                 fmt = 2,
                                 output = "latex"
  )

# completion rate
data_rus %>%
  filter(finished == 1,
         # age > 17,
         time_sum > 180,
         questnnr == "russia") %>%
  pull(response) %>%
  table()

data_rus %>%
  filter(finished == 1,
         time_sum > 180,
         questnnr == "russia") %>%
  pull(response) %>%
  table() %>%
  prop.table() %>%
  round(., 2)

# completion rate
data_la %>%
  filter(finished == 1,
         # age > 17,
         time_sum > 180) %>%
  pull(response) %>%
  table()

data_la %>%
  filter(finished == 1,
         time_sum > 180) %>%
  pull(response) %>%
  table() %>%
  prop.table() %>%
  round(., 2)

# summary stats
data_rus %>%
  filter(
    response != 3,
    finished == 1,
    age > 17,
    time_sum > 180
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
                                 "Judicial Punishment")) %>%
  mutate(rural = case_when(rural %in% 1:4 ~ "Urban",
                           TRUE ~ "Rural") %>% as.character()) %>%
  select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
         "age":"involvement", "condition") %>%
  modelsummary::datasummary_balance(data = .,
                                    formula = ~condition,
                                    output = "latex"
  )


data_la %>%
  filter(
    response != 3,
    finished == 1,
    age > 17,
    time_sum > 180
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
                                 "Judicial Punishment")) %>%
  mutate(rural = case_when(rural %in% 1:4 ~ "Urban",
                           TRUE ~ "Rural") %>% as.character()) %>%
  select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
         "age":"involvement", "condition") %>%
  modelsummary::datasummary_balance(data = .,
                                    formula = ~condition,
                                    output = "latex"
  )
