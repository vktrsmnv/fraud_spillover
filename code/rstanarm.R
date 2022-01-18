# install.packages("brms")
# install.packages("rstanarm")


library(rstanarm)
library(tidyverse)
library(here)
# library(showtext)
conflict_prefer("as_factor", "forcats")
library(bayesplot)
library(parallel)
# 
# # font_add_google("EB Garamond")
# # showtext_auto()
# 
# # setting a global theme for the entire document
# # theme_set(theme_minimal() +
# #             # ggthemes::theme_base(base_family = "EB Garamond") +
# #             theme(
# #               plot.title.position = "plot",
# #               legend.title = element_blank(),
# #               legend.position = "top",
# #               text = element_text(color = mannheim_colors[1])
# #             )
# # )
# 
# # Load the most recent file in the data older
# data_files <- file.info(list.files("data", full.names = T)) %>%
#   rownames_to_column(var = "rowname") %>%
#   filter(str_detect(rowname, "data_lctnfrdn"))
# # 
# data_raw <-
#   read_delim(
#     here(data_files$rowname[which.max(data_files$mtime)]),
#     ";",
#     escape_double = FALSE,
#     trim_ws = TRUE,
#     na = c("-9", "-1")
#   ) 
#   mutate(
#     PTR_army = case_when(QUESTNNR == "russia" |
#                            QUESTNNR == "mexico" ~ TR04_01,
#                          TRUE ~ TR11_01),
#     PTR_police = case_when(QUESTNNR == "russia" |
#                              QUESTNNR == "mexico" ~ TR04_02,
#                            TRUE ~ TR11_02),
#     PTR_CEC = case_when(QUESTNNR == "russia" |
#                           QUESTNNR == "mexico" ~ TR04_03,
#                         TRUE ~ TR11_03),
#     PTR_gov = case_when(QUESTNNR == "russia" |
#                           QUESTNNR == "mexico" ~ TR04_04,
#                         TRUE ~ TR11_04),
#     PTR_part = case_when(QUESTNNR == "russia" |
#                            QUESTNNR == "mexico" ~ TR04_05,
#                          TRUE ~ TR11_05),
#     PTR_parl = case_when(QUESTNNR == "russia" |
#                            QUESTNNR == "mexico" ~ TR04_06,
#                          TRUE ~ TR11_06),
#     PTR_courts = case_when(QUESTNNR == "russia" |
#                              QUESTNNR == "mexico" ~ TR04_07,
#                            TRUE ~ TR11_07),
#     PTR_pres = case_when(QUESTNNR == "russia" |
#                            QUESTNNR == "mexico" ~ TR04_08,
#                          TRUE ~ TR11_08),
#     NTR_comp = TR05_02,
#     NTR_banks = TR05_03,
#     NTR_env = TR05_04,
#     NTR_UN = TR05_05,
#     NTR_WB = TR05_06,
#     NTR_WTO = TR05_07,
#     TR_election = case_when(QUESTNNR == "russia" |
#                               QUESTNNR == "mexico" ~ TR03,
#                             TRUE ~ TR16),
#     TR_election = case_when(TR_election == 1 ~ 4,
#                             TR_election == 2 ~ 3,
#                             TR_election == 3 ~ 2,
#                             TR_election == 4 ~ 1),
#     # TR_election = as.numeric((TR_election)),
#     RG02_01 = case_when(QUESTNNR == "russia" ~ RG02_01,
#                           QUESTNNR == "mexico" ~ RG12_01,
#                         TRUE ~ RG11_01),
#     fraud = case_when(
#       RG02_01 == 2 ~ 1,
#       RG02_01 == 3 ~ 1,
#       RG02_01 == 4 ~ 1,
#       RG11_01 == 2 ~ 1,
#       RG11_01 == 3 ~ 1,
#       RG11_01 == 4 ~ 1,
#       RG12_01 == 2 ~ 1,
#       RG12_01 == 3 ~ 1,
#       RG12_01 == 4 ~ 1,
#       TRUE ~ 0
#     ),
#     punishment = case_when(RG02_01 == 3 ~ 1,
#                            RG02_01 == 4 ~ 1,
#                            RG11_01 == 3 ~ 1,
#                            RG11_01 == 4 ~ 1,
#                            RG12_01 == 3 ~ 1,
#                            RG12_01 == 4 ~ 1,
#                            TRUE ~ 0),
#     punishment_judicial = case_when(RG02_01 == 4 ~ 1,
#                                     RG11_01 == 4 ~ 1,
#                                     RG12_01 == 4 ~ 1,
#                                     TRUE ~ 0),
#     PA_interest = case_when(QUESTNNR == "russia" |
#                               QUESTNNR == "mexico" ~ PA01,
#                             TRUE ~ PA18),
#     PA_corruption = case_when(QUESTNNR == "russia" |
#                                 QUESTNNR == "mexico" ~ PA08_01,
#                               TRUE ~ PA23_01),
#     opponent = case_when(
#       PA14 == 2 ~ 1,
#       # KPRF
#       PA14 == 15 ~ 1,
#       # Other
#       PA14 == 16 ~ 1,
#       # None
#       PA12 == 1 ~ 1,
#       # PAN
#       PA12 == 2 ~ 1,
#       # PRI
#       PA12 == 3 ~ 1,
#       # PRD
#       PA12 == 4 ~ 1,
#       # PVEM
#       PA12 == 6 ~ 1,
#       # MC
#       PA12 == 9 ~ 1,
#       # None
#       PA12 == 10 ~ 1,
#       # Other
#       PA17 == 1 ~ 1,
#       # AV
#       PA17 == 4 ~ 1,
#       # Colombia humana
#       PA17 == 6 ~ 1,
#       # FARC
#       PA17 == 7 ~ 1,
#       # MAIS
#       PA17 == 10 ~ 1,
#       # POC
#       PA17 == 12 ~ 1,
#       # PDA/POLO
#       PA17 == 13 ~ 1,
#       # PLC
#       PA17 == 14 ~ 1,
#       # Other
#       PA17 == 15 ~ 1,
#       # None
#       TRUE ~ 0
#     )
#   )


data_rus <- data %>%
  filter(CASE != 416, # that was me testing 
         FINISHED == 1,
         # TIME_SUM > 299,
         TIME_SUM > 180,
         QUESTNNR == "russia")

data_LA <- data %>%
  filter(FINISHED == 1,
         # TIME_SUM > 180,
         TIME_SUM > 299,
         QUESTNNR != "russia")

data_LA %>% 
  dplyr::select(starts_with("SD")) %>%
  mutate(SD09 = ifelse(is.na(SD09), SD17,SD09 )) %>%
  filter(SD03_01 > 17) %>%
  dplyr::select(-SD17) %>%
  as.data.frame() %>% 
  stargazer()
# # library(rstanarm)
fit1 <- rstanarm::stan_glm(PTR_CEC ~ (fraud + punishment + punishment_judicial)*opponent,
            data = data_rus,
            seed = 1201)

data_rus1 <- data_rus %>% 
  mutate(across(starts_with("PTR_"), ~ as.factor(.x))) 

options(mc.cores = parallel::detectCores())

fit2 <- stan_polr(PTR_CEC ~ (fraud + punishment + punishment_judicial),
                  data = data_rus1,
                  cores = 4,
                  prior = R2(0.25), 
                  prior_counts = dirichlet(1),
                  seed = 1201)

fit3 <- stan_polr(PTR_parl ~ fraud + punishment + punishment_judicial,
                  data = data_rus1,
                  cores = 4,
                  prior = R2(0.25), 
                  prior_counts = dirichlet(1),
                  seed = 1201)

bayesplot::mcmc_areas_ridges(fit2)


# library(bayesplot)
# posterior_interval(fit1, prob = 0.95)
# any(summary(fit1)[, "Rhat"] > 1.1)
# 
fit1 <- stan_glm(PTR_pres ~ (fraud + punishment + punishment_judicial) +PA01 +
                             PA01 + SD01 + log(SD03_01) + SD04 + SD07 + SD16 ,
                           data = data_rus,
                           seed = 1201)
fit2 <- stan_polr(PTR_pres ~ (fraud + punishment + punishment_judicial) +PA01 +
                   PA01 + SD01 + log(SD03_01) + SD04 + SD07 + SD16 ,
                 data = data_rus1,
                 prior = R2(0.25), 
                 prior_counts = dirichlet(1),
                 seed = 1201)
mcmc_areas_ridges(fit2)
# fit2 <- rstanarm::stan_glm(PTR_army ~ fraud + punishment, 
#                            data = data_rus,
#                            seed = 1201)
# fit2 <- rstanarm::stan_glm(PTR_CEC ~ fraud*opponent + punishment*opponent + punishment_judicial*opponent, 
#                            data = data_rus,
#                            seed = 1201)
# 
# ppc_intervals(y = fit1$model$PTR_CEC,
#                       yrep = posterior_predict(fit1),
#                       x = fit1$model$RG02_01, 
#                       prob = 0.9)
# 
# 
# mcmc_intervals(fit1, 
#            regex_pars = "RG02",
#            prob = 0.8, prob_outer = 0.95) +
#   scale_y_discrete(labels = c("Control", "Fraud", 
#                               "Punishment", "Judicial\nPunishment")) +
#   labs(x = "Trust in Institution") +
#   xlim(1,4) 
# 
# dat <- ppc_intervals_data(
#   y = fit1$model$PTR_CEC,
#   yrep = posterior_predict(fit1),
#   x = fit1$model$punishment,
#   prob = 0.5
# )
# 
# 
# library(brms)

data_rus$fraud <- as.character(data_rus$fraud)
data_rus$punishment <- as.character(data_rus$punishment)
data_rus$RG02_01 <- as.character(data_rus$RG02_01)
data_rus$RG02_010 <- ifelse(data_rus$RG02_01 == "4", "3",  data_rus$RG02_01)
# fit1 <- stan_glm(PTR_CEC ~ -1 + RG02_010, 
#                            data = data_rus,
#                            seed = 1201)
# 
# fit2 <- stan_glm(PTR_CEC ~  -1 + RG02_01*opponent, 
#                  data = data_rus,
#                  seed = 1201)
# 
# mcmc_intervals(fit1)
# 
# refgrid <- data_rus %>% 
#   dplyr::select(punishment) %>% 
#   psycho::refdata(length.out=10)
# 
# plot_model(fit2, 
#             type = "int", 
#             terms = c("RG02_01", "opponent"),
#            mdrt.values = "meansd",
#            ci.lvl = 0.9) 
# pl
data_rus1 <- data_rus %>% mutate(across(starts_with("PTR_"), ~as.factor(.x))) 

options(mc.cores = parallel::detectCores())

# fit2 <- stan_polr(PTR_CEC ~ (fraud + punishment + punishment_judicial),
#                   data = data_rus1,
#                   cores = 4,
#                   prior = R2(0.25), 
#                   prior_counts = dirichlet(1),
#                   seed = 1201)

#### Simple Model ####

model_calc <- function(data,
                       polinst,
                       npolinst,
                       contr) {
  
  polinst_mods_lm <- npolinst_mods_lm <- list()
  polinst_mods_ol <- npolinst_mods_ol <- list()
  plotting_data <- tibble()
  data1 <- data %>% 
    mutate(across(starts_with("PTR_"), ~as.factor(.x)),
           across(starts_with("NTR_"), ~as.factor(.x))) 
 
   for (DV in polinst) {
    polinst_mods_lm[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        cores = 4,
        seed = 1201
      )
    
    # plotting_data <- mcmc_intervals_data(polinst_mods_lm[[DV]]) %>%
    #   mutate(institution = DV) %>%
    #   bind_rows(., plotting_data)
    
    polinst_mods_ol[[DV]] <-
      stan_polr(
        paste(DV, IVs, sep = "~"),
        data = data1,
        cores = 4,
        prior = R2(0.25), 
        prior_counts = dirichlet(1),
        seed = 1201
      )
  }
  for (DV in npolinst) {
    npolinst_mods_lm[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    # plotting_data <- mcmc_intervals_data(npolinst_mods_lm[[DV]], prob_outer = 0.95) %>%
    #   mutate(institution = DV) %>%
    #   bind_rows(., plotting_data)
    
    npolinst_mods_ol[[DV]] <-
      stan_polr(
        paste(DV, IVs, sep = "~"),
        data = data1,
        cores = 4,
        prior = R2(0.25), 
        prior_counts = dirichlet(1),
        seed = 1201
      )
  }
  output <-
    list("pol_institutions" = list(polinst_mods_lm, polinst_mods_ol),
         "NONpol_institutions" = list(npolinst_mods_lm, npolinst_mods_ol),
         "plotting_data" = plotting_data)
}

polinst <- data %>% 
  dplyr::select(starts_with("PTR")) %>% colnames()
npolinst <- data %>% 
  dplyr::select(starts_with("NTR")) %>% colnames()

IVs <- c("fraud + punishment + punishment_judicial + opponent")

FP_rus <-
  model_calc(data = data_rus,
             polinst = polinst,
             contr = IVs,
             npolinst = npolinst)
write_rds(FP_rus, "output/FP_rus.rds")

FP_rus <- read_rds("output/FP_rus.rds")

estimates <- tibble()
for (i in polinst){
  for (j in 1:2){
    estimates <- bind_rows(
      FP_rus$pol_institutions[[j]][[i]] %>% 
        mcmc_intervals_data(prob_outer = 0.9, 
                            pars = c("fraud1", 
                                     "punishment1", 
                                     "punishment_judicial")) %>%
        mutate(model = j, institution = i),
      estimates) 
  }
}

for (i in npolinst){
  for (j in 1:2){
    estimates <- bind_rows(
      FP_rus$NONpol_institutions[[j]][[i]] %>% 
        mcmc_intervals_data(prob_outer = 0.9, 
                            pars = c("fraud1", 
                                     "punishment1", 
                                     "punishment_judicial")) %>%
        mutate(model = j, institution = i),
      estimates) 
  }
}

estimates$model <- ifelse(estimates$model == 1, "Linear", "Ordered")


ggplot(estimates) +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter,
    color = parameter
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ model, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  # scale_x_continuous(
  #   limits = c(1, 4),
  #   labels =  c("None\nat all", 
  #               "Not very\nmuch", 
  #               "Quite\na Lot", 
  #               "A Great\nDeal")
  # ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Institutions in Russia",
       subtitle = FP_rus$pol_institutions[[1]]$PTR_army$formula %>% 
         str_remove(".*~"),
       caption = "Bayesian Posterior Densities, 
       Median and 90% Credible Intervals") +
  theme(legend.position = "top")

ggsave(filename = "figs/overview_rus.pdf", 
       width = 10, height = 10)


FP_LA <-
  model_calc(data = data_LA,
             polinst = polinst,
             contr = IVs,
             npolinst = npolinst)

write_rds(FP_LA, "output/FP_LA.rds")


# FP_LA <- read_rds("output/FP_LA.rds")

estimates <- tibble()
for (i in polinst){
  for (j in 1:2){
    estimates <- bind_rows(
      FP_LA$pol_institutions[[j]][[i]] %>% 
        mcmc_intervals_data(prob_outer = 0.9, 
                            pars = c("fraud", 
                                     "punishment", 
                                     "punishment_judicial")) %>%
        mutate(model = j, institution = i),
      estimates) 
  }
}

for (i in npolinst){
  for (j in 1:2){
    estimates <- bind_rows(
      FP_LA$NONpol_institutions[[j]][[i]] %>% 
        mcmc_intervals_data(prob_outer = 0.9, 
                            pars = c("fraud", 
                                     "punishment", 
                                     "punishment_judicial")) %>%
        mutate(model = j, institution = i),
      estimates) 
  }
}

estimates$model <- ifelse(estimates$model == 1, "Linear", "Ordered")


ggplot(estimates) +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter,
    color = parameter
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ model, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  # scale_x_continuous(
  #   limits = c(1, 4),
  #   labels =  c("None\nat all", 
  #               "Not very\nmuch", 
  #               "Quite\na Lot", 
  #               "A Great\nDeal")
  # ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Institutions in LA",
       subtitle = FP_LA$pol_institutions[[1]]$PTR_army$formula %>% 
         str_remove(".*~"),
       caption = "Bayesian Posterior Densities, 
       Median and 90% Credible Intervals") +
  theme(legend.position = "top")

ggsave(filename = "figs/overview_la.pdf", 
       width = 10, height = 10)

##### Plotting RUS ##### 

FP_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                        parameter1 == "RG02_0102" ~ "Fraud",
                        parameter1 == "RG02_0103" ~ "Punishment",
                        TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President"
                          )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y",space = "free") +
  theme(strip.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  # scale_color_mannheim(discrete = TRUE) +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9")) +
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  labs(y = "",
       x = "",
       # title = "Trust in Political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") ->n

ggsave(n, filename = "political_rus.pdf", width = 6, height = 6)

FP_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +   viridis::scale_color_viridis(discrete = T, 
                                     option = "C", 
                                     end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "nonpolitical_rus.pdf", width = 6, height = 6)


##### Plotting LA ##### 
# data_LA$RG11_01 <- as.character(data_LA$RG11_01)
# data_LA$RG12_01 <- as.character(data_LA$RG12_01)
# data_LA$RG02_010 <- case_when(data_LA$RG11_01 == "4" ~ "3",
#                               data_LA$RG12_01 == "4" ~ "3",
#                               data_LA$RG11_01 == "3" ~ "3",
#                               data_LA$RG12_01 == "3" ~ "3",
#                               data_LA$RG11_01 == "2" ~ "2",
#                               data_LA$RG12_01 == "2" ~ "2",
#                               data_LA$RG11_01 == "1" ~ "1",
#                               data_LA$RG12_01 == "1" ~ "1")



FP_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +    scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9")) +

  # viridis::scale_color_viridis(discrete = T, 
                                     # option = "C", 
                                     # end = 0.8) +
  labs(y = "",
       x = "",
       # title = "Trust in Political Institutions in Latin America",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "political_LA.pdf", width = 6, height = 6)


FP_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) + 
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Latin America",
       caption = "Bayesian Posterior Densities,
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "nonpolitical_LA.pdf", width = 6, height = 6)


#### Interaction Model ####

model_calc_2 <- function(data,
                       polinst,
                       npolinst,
                       contr) {
  
  polinst_mods_lm <- npolinst_mods_lm <- list()
  polinst_mods_ol <- npolinst_mods_ol <- list()
  plotting_data <- tibble()
  data1 <- data %>% 
    mutate(across(starts_with("PTR_"), ~as.factor(.x)),
           across(starts_with("NTR_"), ~as.factor(.x))) 
  
  for (DV in polinst) {
    polinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    post <- as_tibble(polinst_mods[[DV]]) %>%
      janitor::clean_names()
    
    supporters <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$punishment1)
    
    opponents <- cbind(
      post$intercept + 
        post$fraud1 %*% t(0:1) +
        post$opponent + 
        post$fraud1_opponent %*% t(0:1),
      post$intercept + 
        post$fraud1 + 
        post$opponent + 
        post$fraud1_opponent +
        post$punishment1 + 
        post$opponent_punishment1 )
    
    colnames(supporters) <- c("Control", "Fraud", "Punishment")
    colnames(opponents) <- c("Control", "Fraud", "Punishment")
    
    plotting_data <- mcmc_intervals_data(supporters,
                                         prob_outer = 0.95) %>%
      bind_rows(., mcmc_intervals_data(opponents,
                                       prob_outer = 0.95)) %>%
      mutate(group = rep(c("Supporters", "Opponents"), each = 3),
             institution = DV) %>%
      bind_rows(., plotting_data)
  }
  for (DV in npolinst) {
    npolinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    post <- as_tibble(npolinst_mods[[DV]]) %>%
      janitor::clean_names()
    
    supporters <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$punishment1 )
    
    opponents <- cbind(
      post$intercept + 
        post$fraud1 %*% t(0:1) +
        post$opponent + 
        post$fraud1_opponent %*% t(0:1),
      post$intercept + 
        post$fraud1 + 
        post$opponent + 
        post$fraud1_opponent +
        post$punishment1 + 
        post$opponent_punishment1 )
    
    colnames(supporters) <- c("Control", "Fraud", "Punishment")
    colnames(opponents) <- c("Control", "Fraud", "Punishment")
    
    plotting_data <- mcmc_intervals_data(supporters,
                                         prob_outer = 0.95) %>%
      bind_rows(., mcmc_intervals_data(opponents,
                                       prob_outer = 0.95)) %>%
      mutate(group = rep(c("Supporters", "Opponents"), each = 3),
             institution = DV) %>%
      bind_rows(., plotting_data)
  }
  output <-
    list("pol_institutions" = polinst_mods,
         "NONpol_institutions" = npolinst_mods,
         "plotting_data" = plotting_data)
}

IVs <- c("fraud + opponent + punishment + opponent")

FPiO_rus <-
  model_calc(data = data_rus,
             polinst = polinst,
             contr = IVs,
             npolinst = npolinst)
write_rds(FPiO_rus, "output/FPiO_rus.rds")

# data_LA$fraud <- as.character(data_LA$fraud)
# data_LA$punishment <- as.character(data_LA$punishment)
FPiO_LA <-
  model_calc(data = data_LA,
               polinst = polinst,
               contr = IVs,
               npolinst = npolinst)

write_rds(FPiO_LA, "output/FPiO_LA.rds")

##### Plotting RUS ####

FPiO_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals")-> n

ggsave(n, filename = "political_rus_i.pdf", width = 10, height = 6) 




FPiO_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "nonpolitical_rus_i.pdf", width = 10, height = 6)

##### Plotting LA ####

FPiO_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Political Institutions in Latin America",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "political_LA_i.pdf", width = 10, height = 6)




FPiO_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Latin America",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals")-> n

ggsave(n, filename = "nonpolitical_LA_i.pdf", width = 10, height = 6) 


#### Full Model ####

model_calc_3 <- function(data,
                         polinst,
                         npolinst,
                         contr) {
  
  polinst_mods <- npolinst_mods <- list()
  plotting_data <- tibble()
  
  for (DV in polinst) {
    polinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    post <- as_tibble(polinst_mods[[DV]]) %>%
      janitor::clean_names()
    
    supporters <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$punishment1 +
              post$punishment_judicial %*% t(0:1))
    
    opponents <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1) +
              post$opponent + 
              post$fraud1_opponent %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$fraud1_opponent  +
              post$punishment1 +
              post$punishment_judicial %*% t(0:1) +
              post$opponent + 
              post$punishment_judicial_opponent %*% t(0:1))
    
    
    colnames(supporters) <- c("Control", "Fraud", "Punishment", "Judicial Punishment")
    colnames(opponents) <- c("Control", "Fraud", "Punishment",  "Judicial Punishment")
    
    plotting_data <- mcmc_intervals_data(supporters,
                                         prob_outer = 0.95) %>%
      bind_rows(., mcmc_intervals_data(opponents,
                                       prob_outer = 0.95)) %>%
      mutate(group = rep(c("Supporters", "Opponents"), each = 4),
             institution = DV) %>%
      bind_rows(., plotting_data)
  }
  for (DV in npolinst) {
    npolinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    post <- as_tibble(npolinst_mods[[DV]]) %>%
      janitor::clean_names()
    
    supporters <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$punishment1 +
              post$punishment_judicial %*% t(0:1))
    
    opponents <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1) +
              post$opponent + 
              post$fraud1_opponent %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$fraud1_opponent  +
              post$punishment1 +
              post$punishment_judicial %*% t(0:1) +
              post$opponent + 
              post$punishment_judicial_opponent %*% t(0:1))
    
    
    colnames(supporters) <- c("Control", "Fraud", "Punishment", "Judicial Punishment")
    colnames(opponents) <- c("Control", "Fraud", "Punishment",  "Judicial Punishment")
    
    plotting_data <- mcmc_intervals_data(supporters,
                                         prob_outer = 0.95) %>%
      bind_rows(., mcmc_intervals_data(opponents,
                                       prob_outer = 0.95)) %>%
      mutate(group = rep(c("Supporters", "Opponents"), each = 4),
             institution = DV) %>%
      bind_rows(., plotting_data)
  }
  output <-
    list("pol_institutions" = polinst_mods,
         "NONpol_institutions" = npolinst_mods,
         "plotting_data" = plotting_data)
}


IVs <- c("(fraud + punishment + punishment_judicial)*opponent")
FPJiO_rus <-
  model_calc(data = data_rus,
               polinst = polinst,
               contr = IVs,
               npolinst = npolinst)
write_rds(FPJiO_rus, "output/FPJiO_rus.rds")

FPJiO_LA <-
  model_calc(data = data_LA,
               polinst = polinst,
               contr = IVs,
               npolinst = npolinst)
write_rds(FPJiO_LA, "output/FPJiO_LA.rds")

##### Plotting RUS ####

FPJiO_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter,
    color = parameter
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9","#94b166")) +
  
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  labs(y = "",
       x = "",
       # title = "Trust in Political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals") -> n

ggsave(n, filename = "political_rus_ij.pdf", width = 11, height = 6)




FPJiO_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals ") -> n

ggsave(n, filename = "nonpolitical_rus_ij.pdf", width = 10, height = 6)

##### Plotting LA ####


FPJiO_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter,
    color = parameter
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9","#94b166")) +
  
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  labs(y = "",
       x = "",
       # title = "Trust in Political Institutions in Latin America",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals") -> n

ggsave(n, filename = "political_LA_ij.pdf", width = 11, height = 6)




FPJiO_LA$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Non-political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  viridis::scale_color_viridis(discrete = T, 
                               option = "C", 
                               end = 0.8) +
  labs(y = "",
       x = "",
       title = "Trust in Non-political Institutions in Latin America",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals") -> n

ggsave(n, filename = "nonpolitical_LA_ij.pdf", width = 10, height = 6) 

############################

model_calc_4 <- function(data,
                         polinst,
                         npolinst,
                         contr) {
  
  polinst_mods <- npolinst_mods <- list()
  plotting_data <- tibble()
  
  for (DV in polinst) {
    polinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    post <- as_tibble(polinst_mods[[DV]]) %>%
      janitor::clean_names()
    
    supporters <- 
      cbind(post$intercept +
              post$fraud1 %*% t(0:1),
            post$intercept + 
              post$fraud1 + 
              post$punishment1 +
              post$punishment_judicial %*% t(0:1))
    
    # opponents <- 
    #   cbind(post$intercept +
    #           post$fraud1 %*% t(0:1) +
    #           post$opponent,
    #         post$intercept + 
    #           post$fraud1 + 
    #           post$punishment1 +
    #           post$punishment_judicial %*% t(0:1) +
    #           post$opponent )
    # 
    
    colnames(supporters) <- c("Control", "Fraud", "Punishment", "Judicial Punishment")
    # colnames(opponents) <- c("Control", "Fraud", "Punishment",  "Judicial Punishment")
    
    plotting_data <- mcmc_intervals_data(supporters,
                                         prob_outer = 0.95) %>%
      bind_rows(., mcmc_intervals_data(opponents,
                                       prob_outer = 0.95)) %>%
      mutate(group = rep(c("Supporters", "Opponents"), each = 4),
             institution = DV) %>%
      bind_rows(., plotting_data)
  }
  # for (DV in npolinst) {
  #   npolinst_mods[[DV]] <-
  #     stan_glm(
  #       paste(DV, IVs, sep = "~"),
  #       data = data,
  #       seed = 1201
  #     )
  #   post <- as_tibble(npolinst_mods[[DV]]) %>%
  #     janitor::clean_names()
  #   
  #   supporters <- 
  #     cbind(post$intercept +
  #             post$fraud1 %*% t(0:1),
  #           post$intercept + 
  #             post$fraud1 + 
  #             post$punishment1 +
  #             post$punishment_judicial %*% t(0:1))
  #   
  #   opponents <- 
  #     cbind(post$intercept +
  #             post$fraud1 %*% t(0:1) +
  #             post$opponent + 
  #             post$fraud1_opponent %*% t(0:1),
  #           post$intercept + 
  #             post$fraud1 + 
  #             post$fraud1_opponent  +
  #             post$punishment1 +
  #             post$punishment_judicial %*% t(0:1) +
  #             post$opponent + 
  #             post$punishment_judicial_opponent %*% t(0:1))
  #   
  #   
  #   colnames(supporters) <- c("Control", "Fraud", "Punishment", "Judicial Punishment")
  #   colnames(opponents) <- c("Control", "Fraud", "Punishment",  "Judicial Punishment")
  #   
  #   plotting_data <- mcmc_intervals_data(supporters,
  #                                        prob_outer = 0.95) %>%
  #     bind_rows(., mcmc_intervals_data(opponents,
  #                                      prob_outer = 0.95)) %>%
  #     mutate(group = rep(c("Supporters", "Opponents"), each = 4),
  #            institution = DV) %>%
  #     bind_rows(., plotting_data)
  # }
  output <-
    list("pol_institutions" = polinst_mods,
         "NONpol_institutions" = npolinst_mods,
         "plotting_data" = plotting_data)
}


IVs <- c("(fraud + punishment + punishment_judicial) + opponent")


FPiO_rus <-
  model_calc_4(data = data_rus,
               polinst = polinst,
               contr = IVs,
               npolinst = npolinst)
FPiO_rus$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_0101" ~ "Control",
                         parameter1 == "RG02_0102" ~ "Fraud",
                         parameter1 == "RG02_0103" ~ "Punishment",
                         TRUE ~ parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President",
                          
                          institution == "NTR_comp" ~ "Companies",
                          institution == "NTR_banks" ~ "Banks",
                          institution == "NTR_env" ~ "Environmental\norganizations",
                          institution == "NTR_UN" ~ "UN",
                          institution == "NTR_WB" ~ "World Bank",
                          institution == "NTR_WTO" ~ "WTO"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter,
    color = parameter
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ group, scales = "free_y") +
  theme(strip.text.y = element_blank()) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9","#94b166")) +
  
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  labs(y = "",
       x = "",
       # title = "Trust in Political Institutions in Russia",
       caption = "Bayesian Posterior Densities, 
       Median and 95% Credible Intervals") -> n


####################


model_calc <- function(data,
                       polinst,
                       npolinst,
                       contr) {
  
  polinst_mods <- npolinst_mods <- list()
  plotting_data <- tibble()
  
  for (DV in polinst) {
    polinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    plotting_data <- mcmc_intervals_data(polinst_mods[[DV]]) %>%
      mutate(institution = DV) %>%
      bind_rows(., plotting_data)
  }
  for (DV in npolinst) {
    npolinst_mods[[DV]] <-
      stan_glm(
        paste(DV, IVs, sep = "~"),
        data = data,
        seed = 1201
      )
    plotting_data <- mcmc_intervals_data(npolinst_mods[[DV]], prob_outer = 0.95) %>%
      mutate(institution = DV) %>%
      bind_rows(., plotting_data)
  }
  output <-
    list("pol_institutions" = polinst_mods,
         "NONpol_institutions" = npolinst_mods,
         "plotting_data" = plotting_data)
}

polinst <- data %>% dplyr::select(starts_with("PTR")) %>% colnames()
npolinst <- data %>% dplyr::select(starts_with("NTR")) %>% colnames()

IVs <- c("-1 + RG02_01 + opponent")

FP_rus1 <-
  model_calc(data = data_rus,
             polinst = polinst,
             contr = IVs,
             npolinst = npolinst)

FP_rus1$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_011" ~ "Control",
                         parameter1 == "RG02_012" ~ "Fraud",
                         parameter1 == "RG02_013" ~ "Punishment",
                         parameter1 == "RG02_014" ~ "Judicial Punishment",
                         TRUE ~ parameter1),
  parameter1 = as_factor(parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y",space = "free") +
  theme(strip.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "#003056"),
        rect = element_rect(color = "#003056"),
        line = element_line(color = "#003056")) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  ) +
  # scale_color_mannheim(discrete = TRUE) +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9","#94b166")) +
  
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  labs(y = "",
       x = "")
       # title = "Trust in Political Institutions in Russia",
       # caption = "Bayesian Posterior Densities, 
       # Median and 95% Credible Intervals ") 

ggsave(filename = "political_rus_1.pdf", width = 7, height = 6)


##### Plotting LA ##### 
data_LA$RG11_01 <- as.character(data_LA$RG11_01)
data_LA$RG12_01 <- as.character(data_LA$RG12_01)
data_LA$RG02_01 <- case_when(data_LA$RG11_01 == "4" ~ "4",
                              data_LA$RG12_01 == "4" ~ "4",
                              data_LA$RG11_01 == "3" ~ "3",
                              data_LA$RG12_01 == "3" ~ "3",
                              data_LA$RG11_01 == "2" ~ "2",
                              data_LA$RG12_01 == "2" ~ "2",
                              data_LA$RG11_01 == "1" ~ "1",
                              data_LA$RG12_01 == "1" ~ "1")

FP_LA1 <-
  model_calc(data = data_LA,
             polinst = polinst,
             contr = IVs,
             npolinst = npolinst)

FP_LA1$plotting_data %>%
  mutate(political = if_else(
    str_detect(institution, "PTR"),
    "Political Institutions",
    "Non-political Institutions"
  ),
  parameter1 = as.character(parameter),
  parameter1 = case_when(parameter1 == "RG02_011" ~ "Control",
                         parameter1 == "RG02_012" ~ "Fraud",
                         parameter1 == "RG02_013" ~ "Punishment",
                         parameter1 == "RG02_014" ~ "Judicial Punishment",
                         TRUE ~ parameter1),
  parameter1 = as_factor(parameter1),
  institution = case_when(institution == "PTR_army" ~ "Army",
                          institution == "PTR_police" ~ "Police",
                          institution == "PTR_CEC" ~ "Electoral\nCommission",
                          institution == "PTR_gov" ~ "Government",
                          institution == "PTR_part" ~ "Parties",
                          institution == "PTR_parl" ~ "Parliament",
                          institution == "PTR_courts" ~ "Courts",
                          institution == "PTR_pres" ~ "President"
  )
  ) %>%
  filter(parameter != "sigma",
         political == "Political Institutions") %>%
  ggplot() +
  geom_pointrange(aes(
    x = m,
    xmin = ll,
    xmax = hh,
    y = institution,
    shape = parameter1,
    color = parameter1
  ),
  position = position_dodge(1)) +
  facet_grid(institution ~ ., scales = "free_y") +
  theme(strip.text.y = element_blank(),
        axis.line = element_line(color = "#003056"),
        rect = element_rect(color = "#003056"),
        line = element_line(color = "#003056")) +
  scale_x_continuous(
    limits = c(1, 4),
    labels =  c("None\nat all", 
                "Not very\nmuch", 
                "Quite\na Lot", 
                "A Great\nDeal")
  )  +
  scale_color_manual(values = c("#003056", "#df7e50", "#b3b7b9","#94b166")) +
  
  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  scale_shape_manual(values=c(15, 16, 17, 8)) +  # viridis::scale_color_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8) +
  
  # viridis::scale_color_viridis(discrete = T, 
  # option = "C", 
  # end = 0.8) +
  labs(y = "",
       x = ""
       # title = "Trust in Political Institutions in Latin America",
       # caption = "Bayesian Posterior Densities, 
       # Median and 95% Credible Intervals "
       ) 

ggsave(filename = "political_LA_1.pdf", width = 7, height = 6)
