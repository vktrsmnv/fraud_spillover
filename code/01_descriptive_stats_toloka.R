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
    "estimatr",
    "modelsummary"
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

# load all the functions
source("code/functions.R")

# load the datasets
data_rus <- read_delim(here("data/RU.csv"),
                       delim = ";",
                       escape_double = FALSE,
                       trim_ws = TRUE)  %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
  filter(questnnr == "russia")

data_la <- read_csv(here("data/LA.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
  filter(questnnr != "russia")

# completion rate: overall
round(prop.table(table(data_rus$finished)), 3)
round(prop.table(table(data_la$finished)), 3)

# dropout across the conditions: final dataset
# options("modelsummary_format_numeric_latex" = "plain")
data_rus %>%
  select(-pa12, -pa17) %>%
  mutate(
    opponent = as.character(opponent),
    fraud = as.character(fraud),
    punishment = as.character(punishment),
    drop_after_treatment = ifelse(!is.na(time004) & is.na(time005), 1, 0),
    judicial_punishment = as.character(judicial_punishment),
    condition = fct_relevel(
      condition,
      "Fraud",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ) %>%
  mutate(rural = case_when(
    rural %in% 1:4 ~ "Urban",
    TRUE ~ "Rural"
  ) %>%
    as.character()
  ) %>%
  filter(
    # response != 3,
    # finished == 1,
    age > 17,
    time_sum > 180
  ) %>%
  select(drop_after_treatment, finished, condition) %>%
  # select(starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
  #        "age":"involvement", "condition") %>%
  modelsummary::datasummary_crosstab(
    data = .,
    title = "Dropout Rates right after Reading the Treatment across Experimental Conditions,
Survey Data in Russia.",
    formula = condition ~ drop_after_treatment,
    fmt = 2,
    # output = "tables/dropout_ru.tex"
  )



# dropout across the conditions
data_la %>%
  mutate(
    condition = fct_relevel(
      condition,
      "Fraud",
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    drop_after_treatment = ifelse(!is.na(time004) & is.na(time005), 1, 0),
  ) %>%
  select(finished, drop_after_treatment, condition) %>%
  modelsummary::datasummary_crosstab(
    data = .,
    formula = condition ~ drop_after_treatment,
    fmt = 2,
    title = "Dropout Rates right after Reading the Treatment across Experimental Conditions,
Survey Data in Latin America",
    output = "tables/dropout_la.tex"
  )

# time required
# data_rus %>%
#   filter(finished == 1,
#          questnnr == "russia") %>%
#   select(-pa12, -pa17) %>%
#   mutate(opponent = as.character(opponent),
#          fraud = as.character(fraud),
#          punishment = as.character(punishment),
#          judicial_punishment = as.character(judicial_punishment),
#          condition = fct_relevel(condition,
#                                  "Fraud",
#                                  "Control",
#                                  "Punishment",
#                                  "Judicial Punishment")) %>%
#   select(response) %>%
#   modelsummary::datasummary_skim(data = .,
#                                      # formula = ~ Percent(),
#                                      fmt = 2,
#                                      # output = "latex"
#   )

data_rus %>%
  filter(finished == 1) %>%
  mutate(condition = fct_relevel(
    condition,
    "Fraud",
    "Control",
    "Punishment",
    "Judicial Punishment"
  )) %>%
  select(condition, time_sum, time004, time005) %>%
  modelsummary::datasummary_balance(
    data = .,
    formula = ~condition,
    fmt = 2,
    dinm=FALSE,
    output = "tables/time_required_ru.tex",
    title = "Time required for completion in seconds (finished interviews), Survey Data for Russia."
  )

data_la %>%
  filter(finished == 1) %>%
  mutate(condition = fct_relevel(
    condition,
    "Fraud",
    "Control",
    "Punishment",
    "Judicial Punishment"
  )) %>%
  select(condition, time_sum, time004, time005) %>%
  modelsummary::datasummary_balance(
    data = .,
    formula = ~condition,
    fmt = 2,
    dinm=FALSE,
    output = "tables/time_required_la.tex",
    title = "Time required for completion in seconds (finished interviews), Survey Data for Latin America"
  )

# completion rate
data_rus %>%
  filter(
    finished == 1,
    age > 17,
    time_sum > 180,
    questnnr == "russia"
  ) %>%
  pull(response) %>%
  table()

data_rus %>%
  filter(
    finished == 1,
    time_sum > 180,
    questnnr == "russia"
  ) %>%
  pull(response) %>%
  table() %>%
  prop.table() %>%
  round(., 2)

# completion rate
data_la %>%
  filter(
    finished == 1,
    # age > 17,
    time_sum > 180
  ) %>%
  pull(response) %>%
  table()

data_la %>%
  filter(
    finished == 1,
    time_sum > 180
  ) %>%
  pull(response) %>%
  table() %>%
  prop.table() %>%
  round(., 2)

# summary stats
data_rus %>%
  filter(
    # response != 3,
    finished == 1,
    age > 17,
    time_sum > 180
  ) %>%
  select(-pa14) %>%
  mutate(
    opponent = as.character(opponent),
    fraud = as.character(fraud),
    punishment = as.character(punishment),
    judicial_punishment = as.character(judicial_punishment),
    condition = fct_relevel(
      condition,
      "Fraud",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ) %>%
  mutate(rural = case_when(
    rural %in% 1:4 ~ "Urban",
    TRUE ~ "Rural"
  ) %>% as.character()) %>%
  select(
    starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
    "age":"involvement", "condition"
  ) %>%
  modelsummary::datasummary_balance(
    data = .,
    formula = ~condition,
    output = "tables/descr_ru.tex"
  )


data_la %>%
  filter(
    # response != 3,
    finished == 1,
    age > 17,
    time_sum > 180
  ) %>%
  select(-pa14) %>%
  mutate(
    opponent = as.character(opponent),
    fraud = as.character(fraud),
    punishment = as.character(punishment),
    judicial_punishment = as.character(judicial_punishment),
    condition = fct_relevel(
      condition,
      "Fraud",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )
  ) %>%
  mutate(rural = case_when(
    rural %in% 1:4 ~ "Urban",
    TRUE ~ "Rural"
  ) %>% as.character()) %>%
  select(
    starts_with("pol_"), starts_with("npol"), "polint", "gentrust",
    "age":"involvement", "condition"
  ) %>%
  modelsummary::datasummary_balance(
    data = .,
    formula = ~condition,
    output = "tables/descr_la.tex"
  )
