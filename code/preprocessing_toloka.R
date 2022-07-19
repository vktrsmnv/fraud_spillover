setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "magrittr",
    "sjlabelled",
    "styler",
    "here",
    "modelsummary",
    "fastDummies"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  lapply(p_needed, require, character.only = TRUE)
}
setup()

# 1. Cleaning Dataset ####

data_files <- file.info(list.files("data", full.names = T)) %>%
  rownames_to_column(var = "rowname") %>%
  filter(str_detect(rowname, "data_lctnfrdn"))

data <-
  read_delim(
    here(data_files$rowname[which.max(data_files$mtime)]),
    ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    na = c("-9", "-1")
  ) %>%
  clean_names() %>%
  remove_empty() %>%
  filter(case > 500,
         !case %in% c(1429, 793, 1406, 3525, 1637, 4450, 1372, 1465)) %>%
  # remove Mexico MTurk cases
  transmute(
    case = as.numeric(case),
    questnnr,
    time_sum,
    time004,
    time005,
    finished,
    pol_inst_armed = coalesce(tr04_01, tr11_01),
    pol_inst_police = coalesce(tr04_02, tr11_02),
    pol_inst_CEC = coalesce(tr04_03, tr11_03),
    pol_inst_gov = coalesce(tr04_04, tr11_04),
    pol_inst_part = coalesce(tr04_05, tr11_05),
    pol_inst_parl = coalesce(tr04_06, tr11_06),
    pol_inst_courts = coalesce(tr04_07, tr11_07),
    pol_inst_pres = coalesce(tr04_08, tr11_08),
    npol_inst_comp = tr05_02,
    npol_inst_banks = tr05_03,
    npol_inst_env = tr05_04,
    npol_inst_UN = tr05_05,
    npol_inst_WB = tr05_06,
    npol_inst_WTO = tr05_07,
    pol_election = coalesce(tr03, tr16) %>%
      as_factor() %>%
      fct_rev(),
    # pol_election = case_when(
    #   pol_election == 1 ~ 4, # None at all
    #   pol_election == 2 ~ 3,
    #   pol_election == 3 ~ 2,
    #   pol_election == 4 ~ 1 # A great deal
    # ),
    condition = coalesce(rg02_01, rg12_01, rg11_01),
    # rg02_01,
    # rg12_01,
    # rg11_01,
    fraud = case_when(
      condition %in% 2:4 ~ 1,
      TRUE ~ 0
    ),
    punishment = case_when(
      condition %in% 3:4 ~ 1,
      TRUE ~ 0
    ),
    judicial_punishment = case_when(
      condition == 4 ~ 1,
      TRUE ~ 0
    ),
    condition = as_factor(condition),
    condition = fct_recode(
      condition,
      "Control" = "1",
      "Fraud" = "2",
      "Punishment" = "3",
      "Judicial Punishment" = "4"
    ),
    polint = coalesce(pa01, pa18) %>%
      as_factor() %>%
      fct_rev() %>%
    # polint = case_when(
    #   polint == 1 ~ 4, # Not at all interested
    #   polint == 2 ~ 3,
    #   polint == 3 ~ 2,
    #   polint == 4 ~ 1 # Very interested
    # ),
    # polint_f = as_factor(polint) %>%
      fct_recode(
        "Very interested" = "4",
        "Somewhat interested" = "3",
        "Not very interested" = "2",
        "Not at all interested" = "1"
      ),
    news_newspaper = coalesce(pa05_01, pa20_01),
    news_tv = coalesce(pa05_02, pa20_02),
    news_radio = coalesce(pa05_03, pa20_03),
    news_internet = coalesce(pa05_04, pa20_04),
    news_social = coalesce(pa05_05, pa20_05),
    news_talk = coalesce(pa05_06, pa20_06),
    gentrust = coalesce(gt01, gt02),
    gentrust_f = as_factor(gentrust) %>%
      fct_recode(
        "Most people can be trusted" = "1",
        "Need to be very careful" = "2"
      ),
    age = sd03_01,
    sex = case_when(
      sd01 == 1 ~ "Female",
      TRUE ~ "Male"
    ),
    edu = coalesce(sd04, sd14),
    sd04_three = case_when(
      sd04 %in% 1:3 ~ 1,
      sd04 %in% 4:7 ~ 2,
      sd04 %in% 8:11 ~ 3,
      # TRUE ~ sd04
    ),
    sd14_three = case_when(
      sd14 %in% 1:3 ~ 1,
      sd14 %in% 4:6 ~ 2,
      sd14 %in% 7:9 ~ 3,
      # TRUE ~ sd14
    ),
    edu_three = coalesce(sd04_three, sd14_three),
    edu_three_f = as_factor(edu_three),
    edu_three_f = fct_recode(
      edu_three_f,
      "Lower" = "1",
      "Middle" = "2",
      "Higher" = "3"
    ),
    emplstat = as_factor(sd15),
    emplstat = fct_recode(
      emplstat,
      "Paid employment" = "1",
      "Retired/pensioned" = "2",
      "Housewife" = "3",
      "Student" = "4",
      "Unemployed" = "5",
      "Other" = "6"
    ),
    sector = as_factor(sd16),
    sector = fct_recode(
      sector,
      "Government or public institution" = "1",
      "Private business or industry" = "2",
      "Private non-profit organization" = "3",
    ),
    savings = sd07,
    savings_f = as_factor(sd07) %>%
      fct_recode(
        "Saved money" = "1",
        "Just got by" = "2",
        "Spent some savings" = "3",
        "Spent savings and\nborrowed money" = "4"
      ),
    rural = coalesce(sd09, sd17),
    polcorup = coalesce(pa08_01, pa23_01),
    opponent = case_when(
      pa14 %in% c(2, 15, 16) ~ "Yes",
      pa12 %in% c(1, 2, 3, 4, 6, 9, 10) ~ "Yes",
      pa17 %in% c(1, 4, 6, 7, 10, 12, 13, 14, 15) ~ "Yes",
      TRUE ~ "No"
    ),
    involvement = coalesce(tr06, tr17),
    involvement = case_when(
      involvement == 1 ~ 4, # None of them
      involvement == 2 ~ 3,
      involvement == 3 ~ 2,
      involvement == 4 ~ 1 # All of them
    ),
    pa14 = as_factor(pa14),
    pa14 = fct_recode(pa14,
      "UR" = "1",
      "CPRF" = "2",
      "LDPR" = "3",
      "Just Russia" = "4",
      "Other" = "15",
      "None" = "16"
    ),
    pa12,
    pa17,
    sd12,
    started = strptime(started, format = "%d-%m-%Y %H:%M:%S"),
    date = as.Date(started),
    maxpage,
    lastpage
  ) %>%
  select(-"sd04_three", -"sd14_three")

write_rds(data, "data/toloka.rds")

# 2. Descriptive Stats Tables ####
## 2.1. Russia ####
### 2.1.1 Dropout #####

data %>%
  filter(questnnr == "russia") %>%
  count(finished, condition) %>%
  mutate(prop = n / sum(n))

### 2.1.2 Balance #####

data_rus <- read_csv("data/RU.csv") %>%
  full_join(data, ., by = c("case" = "CASE")) %>%
  filter(
    questnnr == "russia",
    response != 3,
    finished == 1,
    age > 17
    # time_sum > 210, # 10th quantile
  ) %>%
  select(-involvement, -pa12, -pa17)

data_rus %>%
  dplyr::select(age,
    sex,
    education = edu_three_f,
    employment = emplstat,
    sector,
    savings = savings_f,
    rural,
    trust = gentrust_f,
    political_interest = polint_f,
    corruption = polcorup,
    # party = pa14,
    opponent,
    elections = pol_election,
    armed_forces = pol_inst_armed,
    police = pol_inst_police,
    central_electoral_commission = pol_inst_CEC,
    government = pol_inst_gov,
    parties = pol_inst_part,
    parliament = pol_inst_parl,
    courts = pol_inst_courts,
    president = pol_inst_pres,
    companies = npol_inst_comp,
    banks = npol_inst_banks,
    environmental_organizations = npol_inst_env,
    un = npol_inst_UN,
    world_bank = npol_inst_WB,
    WTO = npol_inst_WTO,
    condition
  ) %>%
  dummy_columns(
    c(
      "education",
      "sex",
      "employment",
      "sector",
      "savings",
      "trust",
      "political_interest",
      # "party"
    ),
    ignore_na = T,
    remove_selected_columns = T
  ) %>%
  clean_names(case = "title") %>%
  datasummary_balance(~Condition,
    data = .,
    dinm = F,
    output = "output/descr_rus_table.tex",
    title = "Sample Balance: Russia"
  )

## 2.2. Latin America ####
### 2.2.1 Dropout #####

data %>%
  filter(questnnr != "russia") %>%
  count(finished, condition) %>%
  mutate(prop = n / sum(n))

data %>%
  filter(questnnr != "russia") %>%
  count(finished, condition, questnnr) %>%
  mutate(prop = n / sum(n))


### 2.2.2 Balance #####

data_la <- read_csv("data/LA.csv") %>%
  full_join(data, ., by = c("case" = "CASE")) %>%
  filter(
    questnnr != "russia",
    response != 3,
    finished == 1,
    age > 17
    # time_sum > 210, # 10th quantile
  ) %>%
  select(-involvement, -pa14)

data_la %>%
  dplyr::select(age,
    sex,
    education = edu_three_f,
    employment = emplstat,
    sector,
    savings = savings_f,
    rural,
    trust = gentrust_f,
    political_interest = polint_f,
    corruption = polcorup,
    # party = pa14,
    opponent,
    elections = pol_election,
    armed_forces = pol_inst_armed,
    police = pol_inst_police,
    central_electoral_commission = pol_inst_CEC,
    government = pol_inst_gov,
    parties = pol_inst_part,
    parliament = pol_inst_parl,
    courts = pol_inst_courts,
    president = pol_inst_pres,
    companies = npol_inst_comp,
    banks = npol_inst_banks,
    environmental_organizations = npol_inst_env,
    un = npol_inst_UN,
    world_bank = npol_inst_WB,
    WTO = npol_inst_WTO,
    condition
  ) %>%
  dummy_columns(
    c(
      "education",
      "sex",
      "employment",
      "sector",
      "savings",
      "trust",
      "political_interest",
      # "party"
    ),
    ignore_na = T,
    remove_selected_columns = T
  ) %>%
  clean_names(case = "title") %>%
  datasummary_balance(~ Condition,
    data = .,
    dinm = F,
    output = "output/descr_la_table.tex",
    title = "Sample Balance: Latin America"
  )
