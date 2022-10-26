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


IVs <- c("condition*opponent")

### Russia: full sample ####
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


### Russia: restriction on data quality #2 ####
data <- data_rus %>% filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


### LA: full sample ####
data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### LA: restriction on data quality #2 ####
data <- data_la %>% filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  name = paste0("cond_la_npol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### Full Sample: collapsed ####
model_calc(
  data = data_comb,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_pol_collapsed"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### Full sample ####
model_calc(
  data = data_la %>%
    bind_rows(., data_rus),
  inst = pol,
  IVs = "condition * opponent",
  model = "ol",
  name = "cond_pol",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### Correct cases only sample ####

model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    filter(attention_check == "Summary"),
  inst = pol,
  IVs = "condition * opponent",
  model = "ol",
  name = "cond_pol_correct",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


### Full combined sample with triple interaction ####

model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = "condition * opponent * questnnr",
  model = "ol",
  name = paste0("cond_pol_ints_threeway"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    filter(attention_check == "Summary"),
  inst = pol,
  IVs = "condition * opponent * questnnr",
  model = "ol",
  name = paste0("cond_pol_ints_threeway_correct"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
