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
         pol_election = case_when(
           pol_election == 1 ~ 4, # None at all
           pol_election == 2 ~ 3,
           pol_election == 3 ~ 2,
           pol_election == 4 ~ 1 # A great deal
         ),
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
         pol_election = as.numeric(pol_election),
         judicial_punishment = as.character(judicial_punishment),
         condition = fct_relevel(condition,
                                 "Fraud",
                                 "Control",
                                 "Punishment",
                                 "Judicial Punishment"))
pol <- data_rus %>%
  dplyr::select(starts_with("pol_")) %>%
  colnames()
npol <- data_rus %>%
  dplyr::select(starts_with("npol_inst")) %>%
  colnames()


data_comb <- data_la %>% bind_rows(., data_rus) %>%
  mutate(condition = fct_collapse(condition,
                                  Punishment = c("Punishment",
                                                 "Judicial Punishment")))

# 2. Main Analysis ####
## 2.0. Calculation Function ####

# source("code/functions.R")

## 2.1. Main Specification ####

### Russia: full sample ####
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  seed = 191022,
  name = paste0("main_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data,
  IVs = "condition",
  inst = npol,
  model = "ol",
  seed = 191022,
  name = paste0("main_ru_npol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

#
# ### Russia: restriction on data quality #1
# data <- data_rus %>% filter(attention_check != "Unacceptable")
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_ru_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = "condition",
#   model = "ol",
#   name = paste0("main_ru_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

### Russia: only correct summaries ####
data <- data_rus %>% filter(attention_check == "Summary")

model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_npol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### Russia: full sample (one punishment) ####
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


### LA: full sample ####
data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_npol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### LA: full sample (one punishment) ####
model_calc(
  data = data_la,
  inst = pol,
  cores = 4,
  IVs = "condition",
  model = "ol",
  name = "main_la_pol_collapsed",
  prior = prior(normal(0, 5),
                class = "b") +
    prior(normal(0, 5),
          class = "Intercept")
)

# ### LA: restriction on data quality #1
# data <- data_la %>% filter(attention_check != "Unacceptable")
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

### LA: only correct summaries ####
data <- data_la %>% filter(attention_check == "Summary")

model_calc(
  data = data,
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
model_calc(
  data = data,
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_npol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### LA: fixed effects ####
model_calc(
  data = data_la,
  inst = pol,
  IVs = "condition + questnnr",
  model = "ol",
  name = "main_pol_la_fe",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### LA: country-varying effects ####
model_calc(
  data = data_la,
  inst = pol,
  IVs = "condition * questnnr",
  model = "ol",
  name = "main_pol_la_int",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


### Full sample ####
model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = "condition",
  model = "ol",
  name = "main_pol",
  seed = 191022,
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = npol,
  IVs = "condition",
  model = "ol",
  name = "main_npol",
  seed = 191022,
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    filter(attention_check != "Unacceptable"),
  inst = pol,
  IVs = "condition",
  model = "ol",
  seed = 191022,
  name = "main_pol_cut",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    filter(attention_check != "Unacceptable"),
  inst = npol,
  IVs = "condition",
  model = "ol",
  seed = 191022,
  name = "main_npol_cut",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


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
#
# ### Full sample with FE
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = pol,
#   IVs = "condition + questnnr",
#   model = "ol",
#   name = "main_pol_fe",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = npol,
#   IVs = "condition + questnnr",
#   model = "ol",
#   name = "main_npol_fe",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

### Full sample with interaction ####
model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = "condition * questnnr",
  model = "ol",
  seed = 191022,
  name = "main_pol_int",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = npol,
  IVs = "condition * questnnr",
  model = "ol",
  seed = 191022,
  name = "main_npol_int",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


### Full sample  (correct only) ####

model_calc(
  data = data_la %>% bind_rows(., data_rus) %>%
    filter(attention_check == "Summary"),
  inst = pol,
  cores = 4,
  seed = 191022,
  IVs = "condition",
  model = "ol",
  name = "main_pol_correct",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

### Full combined sample with interaction (correct only) ####
model_calc(
  data = data_la %>% bind_rows(., data_rus) %>%
    filter(attention_check == "Summary"),
  inst = pol,
  IVs = "condition * questnnr",
  model = "ol",
  seed = 191022,
  name = "main_pol_int_correct",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
#
# ### Mexico only
# model_calc(
#   data = data_la[data_la$questnnr == "mexico",],
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = "main_la_pol_mexico",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Colombia only
# model_calc(
#   data = data_la[data_la$questnnr == "colombia",],
#   inst = pol,
#   IVs = "condition",
#   model = "ol",
#   name = "main_la_pol_colombia",
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

### Russia: hierarchical ####
data <- data_rus %>%
  mutate(condition = as.character(condition))

model_calc(
  data = data,
  inst = pol,
  IVs = "(1 | condition)",
  model = "ol",
  name = paste0("main_ru_pol_ml_", nrow(data))
)

### LA: hierarchical ####
data <- data_la %>%
  mutate(condition = as.character(condition))

model_calc(
  data = data,
  inst = pol,
  IVs = "(1 | condition)",
  model = "ol",
  name = paste0("main_la_pol_ml_", nrow(data))
)


## 2.2. Conditional Effects with Opposition to Regime ####

IVs <- c("condition*opponent")

### Russia: full sample ####
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  seed = 191022,
  name =  paste0("cond_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = IVs,
#   model = "ol",
#   name = paste0("cond_ru_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Russia: restriction on data quality #1
# data <- data_rus %>% filter(attention_check != "Unacceptable")
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = IVs,
#   model = "ol",
#   name =  paste0("cond_ru_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
model_calc(
  data = data,
  inst = npol,
  IVs = IVs,
  model = "ol",
  seed = 191022,
  name = paste0("cond_ru_npol_", nrow(data)),
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
  seed = 191022,
  name =  paste0("cond_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = IVs,
#   model = "ol",
#   name = paste0("cond_ru_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

### LA: full sample ####
data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  seed = 191022,
  name =  paste0("cond_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = IVs,
#   model = "ol",
#   name = paste0("cond_la_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

# ### LA: restriction on data quality #1
# data <- data_la %>% filter(attention_check != "Unacceptable")
# model_calc(
#   data = data,
#   inst = pol,
#   IVs = IVs,
#   model = "ol",
#   name =  paste0("cond_la_pol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
# model_calc(
#   data = data,
#   inst = npol,
#   IVs = IVs,
#   model = "ol",
#   name = paste0("cond_la_npol_", nrow(data)),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

### LA: restriction on data quality #2 ####
data <- data_la %>% filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  seed = 191022,
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
  seed = 191022,
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
  seed = 191022,
  name = "cond_pol_correct",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

#
# ### Full combined sample with interaction ####
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = pol,
#   IVs = "condition * opponent + questnnr",
#   model = "ol",
#   name = paste0("cond_pol_int_fe"),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus) %>%
#     filter(attention_check == "Summary"),
#   inst = pol,
#   IVs = "condition * opponent + questnnr",
#   model = "ol",
#   name = paste0("cond_pol_int_fe_correct"),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# ### Full combined sample with interaction ####
#
# model_calc(
#   data = data_la %>% bind_rows(., data_rus),
#   inst = pol,
#   IVs = "condition * opponent + condition * questnnr",
#   model = "ol",
#   iter = 15000,
#   warmup = 12500,
#   name = paste0("cond_pol_ints"),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )
#
# model_calc(
#   data = data_la %>%
#     bind_rows(., data_rus) %>%
#     filter(attention_check == "Summary"),
#   inst = pol,
#   IVs = "condition * opponent + condition * questnnr",
#   model = "ol",
#   iter = 15000,
#   warmup = 12500,
#   name = paste0("cond_pol_ints_correct"),
#   prior = prior(normal(0, 5), class = "b") +
#     prior(normal(0, 5), class = "Intercept")
# )

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
  iter = 15000,
  warmup = 12500,
  name = paste0("cond_pol_ints_threeway_correct"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

# 3. Additional Specifications ####
## 3.1. With involvement ####

IVs <- c("condition*involvement")
data <- data_rus
model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = IVs,
  model = "ol",
  seed = 191022,
  name = paste0("involvement_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_rus %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_la %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("involvement_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


## 3.2. With Controls ####

IVs <- c("condition +
         opponent +
         polint + gentrust_f + log(age) + sex + edu_three_f +
         emplstat + sector + savings_f + rural + polcorup")

data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  seed = 191022,
  model = "ol",
  name = paste0("controls_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

IVs <- c("condition +
         opponent +
         polint + gentrust_f + log(age) + sex + edu_three_f +
         emplstat + sector + savings_f + rural + polcorup + questnnr")

data <- data_la

model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("controls_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = IVs,
  seed = 191022,
  model = "ol",
  name = paste0("controls_pooled"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


## 3.3. Opponent Specification ######
IVs <- c("condition*opponent")

data <- data_rus %>%
  mutate(opponent = UR)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  seed = 191022,
  model = "ol",
  name =  paste0("cond_ru_pol_UR_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_rus %>%
  filter(attention_check == "Summary")
model_calc(
  data = data %>%
    filter(attention_check == "Summary"),
  inst = pol,
  IVs = IVs,
  seed = 191022,
  model = "ol",
  name =  paste0("cond_ru_pol_UR_corect_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

IVs <- c("condition*pa14")

data <- data_rus %>%
  mutate(pa14 = as.character(pa14),
         opponent = pa14)
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  seed = 191022,
  model = "ol",
  name =  paste0("cond_ru_pol_pa14_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_rus %>%
  mutate(pa14 = as.character(pa14)) %>%
  filter(attention_check == "Summary")

model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name =  paste0("cond_ru_pol_pa14_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

## 3.4. With Political Interest ####

IVs <- c("condition*polint")
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("polint_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_rus %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("polint_ru_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("polint_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_la %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  iter = 15000,
  warmup = 12500,
  name = paste0("polint_la_pol_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>% bind_rows(., data_rus),
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("polint_pooled"),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)


## 3.5. With TV News Consumption ####

IVs <- c("condition*news_tv")
data <- data_rus
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("tv_ru_pol_", nrow(data))
)

data <- data_rus %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("tv_ru_pol_", nrow(data))
)

data <- data_la
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("tv_la_pol_", nrow(data))
)

data <- data_la %>%
  filter(attention_check == "Summary")
model_calc(
  data = data,
  inst = pol,
  IVs = IVs,
  model = "ol",
  name = paste0("tv_la_pol_", nrow(data))
)



# # 4. Plotting

# 4. Mediation Analysis ####

### Russia ####
data <- data_rus %>%
  mutate(
    pol_election = as.numeric(as.character(pol_election)),
    condition = str_replace_all(
      string = data$condition,
      pattern = " ",
      replacement = ""
    )
  )

data <-
  cbind(model.matrix( ~ condition - 1, data),
        data %>% select(starts_with("pol_")))

mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_ru_pol_", nrow(data))
)

data <- data_rus %>%
  filter(attention_check == "Summary") %>%
  mutate(
    pol_election = as.numeric(as.character(pol_election)),
    condition = str_replace_all(
      string = condition,
      pattern = " ",
      replacement = ""
    )
  )

data <-
  cbind(model.matrix( ~ condition - 1, data),
        data %>% select(starts_with("pol_")))

mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_ru_pol_", nrow(data))
)


### Latin America ####
data <- data_la %>%
  mutate(pol_election = as.numeric(as.character(pol_election)))
data$condition <-
  str_replace_all(
    string = data$condition,
    pattern = " ",
    replacement = ""
  )
data <-
  cbind(model.matrix( ~ condition - 1, data),
        data %>% select(starts_with("pol_")))

mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_la_pol_", nrow(data))
)

data <- data_la %>% filter(attention_check == "Summary") %>%
  mutate(pol_election = as.numeric(as.character(pol_election)))
data$condition <-
  str_replace_all(
    string = data$condition,
    pattern = " ",
    replacement = ""
  )
data <-
  cbind(model.matrix( ~ condition - 1, data),
        data %>% select(starts_with("pol_")))

mediation_calc(
  data = data,
  inst = pol,
  IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
  model = "ol",
  name = paste0("mediation_la_pol_", nrow(data))
)


# 5. Elections ####

model_calc(
  data = data_la %>% bind_rows(., data_rus) %>%
    mutate(pol_election = as.numeric(as.character(pol_election))),
  inst = pol[9],
  IVs = "condition",
  model = "ol",
  name = "main_pol_election",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>% bind_rows(., data_rus) %>%
    mutate(pol_election = as.numeric(as.character(pol_election))) %>%
    filter(attention_check == "Summary"),
  inst = pol[9],
  IVs = "condition",
  model = "ol",
  name = "main_pol_correct_election",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_rus %>%
  mutate(pol_election = as.numeric(as.character(pol_election)))
model_calc(
  data = data,
  inst = pol[9],
  IVs = "condition",
  model = "ol",
  name = paste0("main_ru_pol_election_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

data <- data_la %>%
  mutate(pol_election = as.numeric(as.character(pol_election)))
model_calc(
  data = data,
  inst = pol[9],
  IVs = "condition",
  model = "ol",
  name = paste0("main_la_pol_election_", nrow(data)),
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    mutate(pol_election = as.numeric(as.character(pol_election))),
  inst = pol[9],
  IVs = "condition * opponent",
  model = "ol",
  name = "cond_pol_election",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)

model_calc(
  data = data_la %>%
    bind_rows(., data_rus) %>%
    mutate(pol_election = as.numeric(as.character(pol_election))) %>%
    filter(attention_check == "Summary"),
  inst = pol[9],
  IVs = "condition * opponent",
  model = "ol",
  name = "cond_pol_correct_election",
  prior = prior(normal(0, 5), class = "b") +
    prior(normal(0, 5), class = "Intercept")
)
