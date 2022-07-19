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

# load the datasets
data_rus <- read_csv(here("data/RU.csv")) %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
  filter(questnnr == "russia")

data_la <- read_csv(here("data/LA_1.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
  filter(questnnr != "russia")

# completion rate: overall (appendix) ####
round(prop.table(table(data_rus$finished)), 3)
round(prop.table(table(data_la$finished)), 3)

# dropout across the conditions ####
# options("modelsummary_format_numeric_latex" = "plain")
data_rus %>%
  select(-pa12, -pa17) %>%
  mutate(
    opponent = as.character(opponent),
    fraud = as.character(fraud),
    punishment = as.character(punishment),
    drop_after_treatment = ifelse(maxpage == 4, 1, 0),
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
    as.character(),
  drop_after_treatment = as.character(drop_after_treatment),
  drop_after_treatment =
    case_when(
      drop_after_treatment == "1" ~ "Yes",
      drop_after_treatment == "0" ~ "No"
  )
  ) %>%
  filter(
    # response != 3,
    # finished == 1,
    # age > 17,
    # time_sum > 180
  ) %>%
  select(drop_after_treatment, finished, condition) %>%
  modelsummary::datasummary_crosstab(
    data = .,
    title = "Dropout Rates right after Reading the Treatment across Experimental Conditions,
Survey Data in Russia.",
    formula = condition ~ drop_after_treatment,
    fmt = 2,
    output = "tables/dropout_ru.tex"
  )

data_la %>%
  mutate(
    condition = fct_relevel(
      condition,
      "Fraud",
      "Control",
      "Punishment",
      "Judicial Punishment"
    ),
    drop_after_treatment = ifelse(maxpage == 4, 1, 0),
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

# time required ####
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
    dinm = FALSE,
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
    dinm = FALSE,
    output = "tables/time_required_la.tex",
    title = "Time required for completion in seconds (finished interviews), Survey Data for Latin America"
  )

# quality (manual coding) ####

data_rus %>%
  filter(
    finished == 1,
    age > 17,
    sd12 == 1,
    time_sum >= 180,
    questnnr == "russia"
  ) %>%
  mutate(attention_check = fct_infreq(attention_check)) %>%
  select(attention_check) %>%
  janitor::clean_names("sentence") %>%
  modelsummary::datasummary_skim(
    data = .,
    formula = attention_check,
    type = "categorical",
    fmt = 2,
    dinm = FALSE,
    title = "Data quality categories based on manual coding for Russian sample",
    output = "tables/manual_quality_ru.tex",
  )

data_la %>%
  filter(
    finished == 1,
    sd12 == 1,
    age > 17,
    time_sum >= 180,
  ) %>%
  mutate(attention_check = fct_infreq(attention_check)) %>%
  select(attention_check) %>%
  janitor::clean_names("sentence") %>%
  modelsummary::datasummary_skim(
    data = .,
    formula = attention_check,
    type = "categorical",
    fmt = 2,
    dinm = FALSE,
    output = "tables/manual_quality_la.tex",
    title = "Data quality categories based on manual coding for Latin American sample",
  )

# summary stats #####
data_rus %>%
  filter(
    # response != 3,
    finished == 1,
    sd12 == 1,
    age > 17,
    time_sum >= 180,
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
    "age":"involvement", "condition", -"sd04_three", -"sd14_three",
    -"edu_three", -"edu"
  ) -> table_ru

colnames(table_ru) %<>%
  str_replace("n?pol_i?n?s?t?_?", "Confidence in ") %>%
  str_replace("armed", "Armed Forced") %>%
  str_replace("gov", "Government") %>%
  str_replace("part", "Parties") %>%
  str_replace("parl", "Pariament") %>%
  str_replace("pres", "President") %>%
  str_replace("election", "Elections") %>%
  str_replace("comp", "Companies") %>%
  str_replace("env", "Environmental Organizations") %>%
  str_replace("polint", "Political Interest") %>%
  str_replace("gentrust", "Generalized Trust") %>%
  janitor::make_clean_names("title") %>%
  str_replace("Un", "UN") %>%
  str_replace("Wto", "WTO") %>%
  str_replace("Ub", "World Bank") %>%
  str_replace("Edu Three f", "Education") %>%
  str_replace("Emplstat", "Empl. Status") %>%
  str_replace("Sector", "Empl. Sector") %>%
  str_replace("Savings f", "Last year") %>%
  str_replace("Rural", "Set. Type ") %>%
  str_replace("Polcorup", "Pol. Corruption")


  modelsummary::datasummary_balance(
    data = table_ru,
    formula = ~ Condition,
    dinm = FALSE,
    output = "tables/descr_ru.tex",
    title = "Summary Statistics of Key Variables, Survey Data for Russia",
    label = "descr:russia"
  )

# data_rus %>%
#   dplyr::select(age,
#                 sex,
#                 education = edu_three_f,
#                 employment = emplstat,
#                 sector,
#                 savings = savings_f,
#                 rural,
#                 trust = gentrust_f,
#                 political_interest = polint_f,
#                 corruption = polcorup,
#                 # party = pa14,
#                 opponent,
#                 elections = pol_election,
#                 armed_forces = pol_inst_armed,
#                 police = pol_inst_police,
#                 central_electoral_commission = pol_inst_CEC,
#                 government = pol_inst_gov,
#                 parties = pol_inst_part,
#                 parliament = pol_inst_parl,
#                 courts = pol_inst_courts,
#                 president = pol_inst_pres,
#                 companies = npol_inst_comp,
#                 banks = npol_inst_banks,
#                 environmental_organizations = npol_inst_env,
#                 un = npol_inst_UN,
#                 world_bank = npol_inst_WB,
#                 WTO = npol_inst_WTO,
#                 condition
#   ) %>%
#   dummy_columns(
#     select_columns =c(
#       "education",
#       "sex",
#       "employment",
#       "sector",
#       "savings",
#       "trust",
#       "political_interest"
#       # "party"
#     ),
#     ignore_na = T,
#     remove_selected_columns = T
#   ) %>%
#   clean_names(case = "title") %>%
#   datasummary_balance(~Condition,
#                       data = .,
#                       dinm = F,
#                       output = "tables/descr_rus_table.tex",
#                       title = "Sample Balance: Russia"
#   )



data_la %>%
  filter(
    # response != 3,
    finished == 1,
    age > 17,
    sd12 == 1,
    time_sum >= 180,
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
    "age":"involvement", "condition", -"sd04_three", -"sd14_three",
    -"edu_three", -"edu"
  ) -> table_la

colnames(table_la) %<>%
  str_replace("n?pol_i?n?s?t?_?", "Confidence in ") %>%
  str_replace("armed", "Armed Forced") %>%
  str_replace("gov", "Government") %>%
  str_replace("part", "Parties") %>%
  str_replace("parl", "Pariament") %>%
  str_replace("pres", "President") %>%
  str_replace("election", "Elections") %>%
  str_replace("comp", "Companies") %>%
  str_replace("env", "Environmental Organizations") %>%
  str_replace("polint", "Political Interest") %>%
  str_replace("gentrust", "Generalized Trust") %>%
  janitor::make_clean_names("title") %>%
  str_replace("Un", "UN") %>%
  str_replace("Wto", "WTO") %>%
  str_replace("Ub", "World Bank") %>%
  str_replace("Edu Three f", "Education") %>%
  str_replace("Emplstat", "Empl. Status") %>%
  str_replace("Sector", "Empl. Sector") %>%
  str_replace("Savings f", "Last year") %>%
  str_replace("Rural", "Set. Type ") %>%
  str_replace("Polcorup", "Pol. Corruption")

  modelsummary::datasummary_balance(
    data = table_la,
    formula = ~Condition,
    dinm = FALSE,
    title = "Summary Statistics of Key Variables, Survey Data for Latin America",
    label = "descr:la",
    output = "tables/descr_la.tex"
  )
