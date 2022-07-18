# Perceptions of Election Fraud, Judicial Rulings and Diffuse Political Support
# Lion Behrens and Viktoriia Semenova
# Reproduction File

setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "cobalt",
    "tikzDevice",
    "stargazer",
    "brms",
    "MatchIt",
    "fastDummies",
    "lme4",
    "magrittr",
    "sjlabelled",
    "ordinal",
    "here",
    "hrbrthemes",
    "viridis",
    "ggdist"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  lapply(p_needed, require, character.only = TRUE)

  # setwd(dirname(getActiveDocumentContext()$path)) # set directory to the document
}
setup()

# load functions for matching and plotting
source(here("code/functions.R"))

# 1. Data Preparation #########################################################

wvs_vdem <- readRDS(here("data/wvs7.rds")) %>%
  as.data.frame()
wvs_vdem$cntry <- as.factor(wvs_vdem$cow_alpha)
wvs_vdem[, c("edu_three", "emplstat", "poldiscu", "year")] <-
  lapply(wvs_vdem[, c("edu_three", "emplstat", "poldiscu", "year")], factor)

# 2. Bayesian Ordinal logistic Mixed Effects Models ############################

set.seed(12345)

# resource for model code: https://kevinstadler.github.io/notes/bayesian-ordinal-regression-with-random-effects-using-brms/
options(mc.cores = parallel::detectCores()) # distribute chains across CPU cores

x <- "fraud1d"
contr <- c(
  "polint", "gentrust", "sex", "log(age)", "edu_three", "emplstat",
  "savings", "rural", "polcorup", "vdem_elections", "year", "(1|cntry)"
)

# summary statistics
wvs_vdem <- wvs_vdem[, c(
  "inst_armed", "inst_police", "inst_courts",
  "inst_parl", "inst_gov", "inst_parties",
  "inst_comp", "inst_UN", "inst_banks", "inst_wto", "inst_wb",
  "fraud1d", "fraud1d1", "fraud1d2", "polint", "gentrust", "sex", "age", "edu_three", "emplstat",
  "savings", "rural", "polcorup", "vdem_elections", "cntry", "year"
)]
wvs_vdem <- wvs_vdem[complete.cases(wvs_vdem), ]


stargazer(wvs_vdem,
          summary = T
)

## 2.1. Institutions:  spillover ###############################################

y_spill <- c("inst_armed", "inst_police", "inst_courts",
                 "inst_parl", "inst_gov", "inst_parties")

model_calc(data = wvs_vdem,
          inst = y_spill,
          model = "ol",
          IVs = paste0(contr, collapse = " + "),
          name = "basic_wvs")


## 2.3 Institutions: no spillover ##############################################

# y_nospill <- c("inst_comp", "inst_UN", "inst_banks", "inst_wto", "inst_wb")
# m_nospill <- list()
#
# for (depvar in y_nospill) {
#   m_nospill[depvar] <- list(summary(brm(paste(depvar, paste(c(x, contr), collapse = "+"), sep = "~"),
#                                         family = gaussian(link = "identity"), data = wvs_vdem
#   )))
# }

# 3. Matching ##################################################################

## 3.1 Construct empty object for results #####

# ATT average treatment effect on the treated
n_algorithms <- 3
n_institutions <- 11
n_post_samples <- 12000
warmup <- 2000
att_data <- as.data.frame(matrix(NA, nrow = n_institutions * n_algorithms * 10000, ncol = 5))
colnames(att_data) <- c("y_var", "algorithm", "att_lin", "att_ord", "type")
att_data$y_var <- c(
  rep("inst_armed", n_algorithms * 10000),
  rep("inst_police", n_algorithms * 10000),
  rep("inst_courts", n_algorithms * 10000),
  rep("inst_parl", n_algorithms * 10000),
  rep("inst_gov", n_algorithms * 10000),
  rep("inst_parties", n_algorithms * 10000),
  rep("inst_comp", n_algorithms * 10000),
  rep("inst_UN", n_algorithms * 10000),
  rep("inst_banks", n_algorithms * 10000),
  rep("inst_wto", n_algorithms * 10000),
  rep("inst_wb", n_algorithms * 10000)
)
att_data$type <- c(
  rep("real spillover", 90000),
  rep("endogenous spillover", 90000),
  rep("no effect expected", 150000)
)
att_data$algorithm <- rep(c("nearest", "exact", "cem"), 110000)


## 3.2 Propensity score-based nearest neighbor matching without replacement ####

set.seed(54321)

# make sure to include all controls
print(contr)

# calculate PS model
ps_model <- brm(
  formula = as.formula("fraud1d ~ polint + gentrust + sex + log(age) + edu_three +
              emplstat + savings + rural + polcorup + vdem_elections + year"),
  data = wvs_vdem,
  family = bernoulli(link = "logit"),
  warmup = 2000,
  iter = 2250,
  chains = 4,
  inits = "0",
  cores = 4,
  seed = 12345
)

# get all posterior samples from first chain
post_samples <- as.data.frame(as.mcmc(ps_model, combine_chains = T))

# construct propensity scores from each posterior sample
ps_data <- dummy_cols(ps_model$data)
ps_data[, 1] <- 1 # create column of 1s for intercept
colnames(ps_data)[1] <- "constant"
ps_data <- ps_data %>%
  dplyr::select(-c(edu_three, emplstat, year, age, edu_three_1, emplstat_1, year_2017)) %>%
  relocate(
    constant, polint, gentrust, sex, `log(age)`, edu_three_2, edu_three_3, emplstat_2, emplstat_3,
    emplstat_4, emplstat_5, emplstat_6, emplstat_7, emplstat_8, savings, rural, polcorup, vdem_elections,
    year_2018, year_2019, year_2020
  )

post_samples <- post_samples %>%
  dplyr::select(-lp__)

### iterate over rows in ps_data, construct ps for each case based on first posterior sample
### construct a matched data set using matching-function of arm package (below)
### do this for all posterior samples, I get a matched dataset for each posterior sample

# object to save case selection for matching for reproduction
cnts <- list()


for (sample in 1:nrow(post_samples)) {
  if (sample > 1) {
    ps_data <- ps_data[, -ncol(ps_data)]
  }

  # construct PS score
  used_lists <- list(
    as.matrix(ps_data),
    t(as.matrix(post_samples[sample, ]))
  )
  ps_data$prop_score <- c(Reduce("%*%", used_lists))


  # construct matched dataset for each posterior sample
  matched_cases <- matching(
    z = ps_model$data$fraud1d,
    score = ps_data$prop_score,
    shuffle = T,
    random_ties = T
  )
  data_nearest <- as.data.frame(cbind(
    ps_model$data$fraud1d[matched_cases$cnts == 1],
    wvs_vdem[matched_cases$cnts == 1, ]
  ))
  cnts[[sample]] <- which(matched_cases$cnts == 1)

  # calculate ATE
  y_vars <- c(
    "inst_armed", "inst_police", "inst_courts", "inst_parl",
    "inst_gov", "inst_parties", "inst_comp", "inst_UN", "inst_banks",
    "inst_wto", "inst_wb"
  )

  for (y_var in y_vars) {

    # linear regression
    model_lin <- lmer(as.formula(paste(y_var, "fraud1d + (1|cntry)", sep = "~")), data = data_nearest)
    att_data[att_data$y_var == y_var & att_data$algorithm == "nearest", ]$att_lin[sample] <-
      mean(coef(model_lin)$cntry[, 2])

    # ordinal regression
    model_ord <- clmm(as.formula(paste(str_c("as.factor(", y_var, ")"), "fraud1d + (1|cntry)", sep = "~")), data = data_nearest)
    att_data[att_data$y_var == y_var & att_data$algorithm == "nearest", ]$att_ord[sample] <-
      coef(model_ord)[4]
  }

  # store att_data to folder
  saveRDS(att_data, file = "att_data.rds")
  saveRDS(cnts, file = "cnts.rds")

  print(str_c("NN matching, sample ", sample, " of ", nrow(post_samples), " done."))
}


## 3.3 CEM and exact matching (not propensity score-based) #####################

set.seed(33333)

# define models to calculate propensity scores
print(contr)
ps_model <- as.formula("fraud1d ~ polint + gentrust + sex + log(age) + edu_three +
              emplstat + savings + rural + polcorup + vdem_elections + year")

# apply matching algorithms
m_exact <- matchit(ps_model, data = wvs_vdem, method = "exact") # stratifying over controls
m_cem <- matchit(ps_model, data = wvs_vdem, method = "cem") # stratifying over controls

# store matched data sets
data_exact <- match.data(m_exact)
data_cem <- match.data(m_cem)

# run models
y_vars <- c(
  "inst_armed", "inst_police", "inst_courts", "inst_parl",
  "inst_gov", "inst_parties", "inst_comp", "inst_UN", "inst_banks",
  "inst_wto", "inst_wb"
)

iter <- 0
for (depvar in y_vars) {
  iter <- iter + 1

  # based on exact matching, linear model
  model_lin <- brm(
    formula = as.formula(paste(depvar, "fraud1d + (1|cntry)", sep = "~")),
    data = data_exact,
    family = gaussian(link = "identity"),
    warmup = warmup,
    iter = n_post_samples,
    chains = 1,
    inits = "0",
    cores = 2,
    seed = 12345
  )

  samples_exact <- as.mcmc(model_lin)[[1]]
  att_data[att_data$y_var == depvar & att_data$algorithm == "exact", "att_lin"] <-
    samples_exact[, 4]

  # based on exact matching, ordinal model
  model_ord <- brm(
    formula = as.formula(paste(depvar, "fraud1d + (1|cntry)", sep = "~")),
    data = data_exact,
    family = cumulative("logit"),
    warmup = warmup,
    iter = n_post_samples,
    chains = 1,
    inits = "0",
    cores = 2,
    seed = 12345
  )

  samples_exact <- as.mcmc(model_ord)[[1]]
  att_data[att_data$y_var == depvar & att_data$algorithm == "exact", "att_ord"] <-
    samples_exact[, 4]

  # based on CEM, linear model
  model_lin <- brm(
    formula = as.formula(paste(depvar, "fraud1d + (1|cntry)", sep = "~")),
    data = data_cem,
    family = gaussian(link = "identity"),
    warmup = warmup,
    iter = n_post_samples,
    chains = 1,
    inits = "0",
    cores = 2,
    seed = 12345
  )

  samples_cem <- as.mcmc(model_lin)[[1]]
  att_data[att_data$y_var == depvar & att_data$algorithm == "cem", "att_lin"] <-
    samples_cem[, 4]

  # based on CEM, ordinal model
  model_ord <- brm(
    formula = as.formula(paste(depvar, "fraud1d + (1|cntry)", sep = "~")),
    data = data_cem,
    family = cumulative("logit"),
    warmup = warmup,
    iter = n_post_samples,
    chains = 1,
    inits = "0",
    cores = 2,
    seed = 12345
  )

  samples_cem <- as.mcmc(model_ord)[[1]]
  att_data[att_data$y_var == depvar & att_data$algorithm == "cem", "att_ord"] <-
    samples_cem[, 4]

  # store att_data to folder
  saveRDS(att_data, file = "output/att_data.rds")

  print(str_c("exact matching/CEM, ", iter, " of 11 y-variables done."))
} # end for depvar in y_vars






# 4. Plotting Results ####

att_data <- readRDS(here("output/att_data.RDS"))

# # linear models, exact matching
# ggplot(att_data[which(att_data$algorithm == "exact"), ], aes(x = att_lin, color = y_var, fill = y_var)) +
#   geom_histogram(alpha = 0.6, binwidth = 0.01) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
#   facet_wrap(~y_var) +
#   scale_fill_viridis(discrete = TRUE) +
#   scale_color_viridis(discrete = TRUE) +
#   theme_ipsum() +
#   xlab("Posterior distribution of ATE") +
#   ggtitle("Linear models, exact matching")
library(tidybayes)
library(stickylabeller)

# ordingal models, exact matching

att_data %>%
  mutate(algorithm =
           case_when(algorithm == "exact" ~ "Exact",
                     algorithm == "cem" ~ "CEM",
                     algorithm == "nearest" ~ "PS Nearest Neighbor"
           ),
         algorithm = as_factor(algorithm) %>%
           fct_relevel(
             "Exact",
             "CEM",
             "PS Nearest Neighbor"
           )) %>%
  mutate(
    institution =
      case_when(
        y_var == "inst_armed" ~ "Armed Forces",
        y_var == "inst_police" ~ "Police",
        y_var == "inst_gov" ~ "Government",
        y_var == "inst_parties" ~ "Parties",
        y_var == "inst_parl" ~ "Parliament",
        y_var == "inst_courts" ~ "Courts",
        y_var == "inst_comp" ~ "Companies",
        y_var == "inst_banks" ~ "Banks",
        y_var == "inst_UN" ~ "United Nations",
        y_var == "inst_wb" ~ "World Bank",
        y_var == "inst_wto" ~ "WTO"
      ),
    institution = as_factor(institution) %>%
      fct_relevel(
        "Parties",
        "Parliament",
        "Courts",
        "Government",
        "Police",
        "Armed Forces",
        "Companies",
        "Banks",
        "World Bank",
        "WTO"
      ) %>% fct_rev(),
    political = case_when(
      institution %in% c( "Parties",
                          "Parliament",
                          "Courts",
                          "Government",
                          "Police",
                          "Armed Forces") ~ "Political",
      TRUE ~ "Non-political"
    )
  ) %>%
  na.omit() %>%
  ggplot(.,
         aes(x = att_ord,
             y = institution,
             color = political,
             fill = political)) +
  stat_gradientinterval(
    # alpha = 0.6,
    aes(fill_ramp = stat(cut_cdf_qi(
      cdf,
      .width = c(.89, .95),
      labels = scales::percent_format()
    ))),
  ) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "black",
             size = 1) +
  labs(y = "",
       color = "",
       fill = "",
       fill_ramp = "") +
  facet_wrap( ~ algorithm,
              ncol = 3,
              labeller = label_glue('{algorithm}, N = {"tba"}')) +
  # theme_minimal() +
  # scale_fill_viridis(discrete = T,
  #                    direction = -1) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = TRUE) +
  ggdist::scale_fill_ramp_discrete(range = c(1, 0.4),
                                   na.translate = FALSE) +
  xlab("Posterior distribution of ATE") +
  theme_bw() +
  theme(legend.position = "bottom") -> p2
p2
ggsave(p2,
       filename = "figs/matching.png",
       height = 6,
       width = 10)
# why NAs in att_ord for nearest??

# # linear models, CEM
# ggplot(att_data[which(att_data$algorithm == "cem"), ], aes(x = att_lin, color = y_var, fill = y_var)) +
#   geom_histogram(alpha = 0.6, binwidth = 0.01) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
#   facet_wrap(~y_var) +
#   scale_fill_viridis(discrete = TRUE) +
#   scale_color_viridis(discrete = TRUE) +
#   theme_ipsum() +
#   xlab("Posterior distribution of ATE") +
#   ggtitle("Linear models, CEM") -> p3

# ordinal models, CEM
ggplot(att_data[which(att_data$algorithm == "cem"), ], aes(x = att_ord, color = y_var, fill = y_var)) +
  geom_histogram(alpha = 0.6, binwidth = 0.01) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  facet_wrap(~y_var) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  xlab("Posterior distribution of ATE") +
  ggtitle("Ordinal models, CEM") -> p4


#### 4.1. Visual Assessment of balance ####


par(mfrow = c(1, 1))

# for love.plot: rename variables
new.names <- c(
  "Sex", "Age", "Rural", "Employment: Full time", "Employment: Part time", "Employment: Self employed",
  "Employment: Retired/pensioned", "Employment: Housewife", "Employment: Student", "Employment: Unemployed",
  "Employment: Other", "Education", "Household Income", "Vote in national elections: Always", "Vote in national elections: Usually",
  "Vote in national elections: Never", "Vote in national elections: Not allowed to vote",
  "Generalized trust", "Political Interest", "Political Discussion: Frequently", "Political Discussion: Occasionally", "Political Discussion: Never",
  "Perceived Political Corruption"
)

# for love.plot: pair old variables with new variable names
names(new.names) <- c(
  "Sex_2", "Age", "rural_2", "emplstat_1", "emplstat_2", "emplstat_3",
  "emplstat_4", "emplstat_5", "emplstat_6", "emplstat_7",
  "emplstat_8", "edu_recoded", "hhinc", "votenat_1", "votenat_2", "votenat_3",
  "votenat_4", "gentrust_2", "polint", "poldiscu_1", "poldiscu_2", "poldiscu_3",
  "polcorup"
)

tikz("covbalance_cem.tex", standAlone = TRUE, width = 7, height = 7)

cobalt::love.plot(m_cem,
                  binary = "std", var.names = new.names,
                  colors = c("#003056", "#b3b7b9")
) +
  theme(
    legend.position = "top",
    legend.title = element_text()
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "#003056"),
    rect = element_rect(color = "#003056"),
    line = element_line(color = "#003056"),
    text = element_text(color = "#003056"),
    axis.text.x = element_text(color = "#003056"),
    axis.text.y = element_text(color = "#003056"),
    legend.title = element_blank()
  ) +
  labs(title = "Coarsened Exact Matching") -> CEM
# ggsave("CEM_epsa.pdf", width = 6, height = 6)

cobalt::love.plot(m_exact,
                  binary = "std", var.names = new.names, drop.distance = F,
                  colors = c("#003056", "#b3b7b9")
) +
  theme(
    legend.position = "top",
    legend.title = element_text()
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "#003056"),
    rect = element_rect(color = "#003056"),
    line = element_line(color = "#003056"),
    text = element_text(color = "#003056"),
    axis.ticks = element_line(color = "#003056"),
    axis.text.x = element_text(color = "#003056"),
    axis.text.y = element_blank(),
    legend.title = element_blank()
  ) +
  labs(title = "Exact Matching") -> exact
# library(patchwork)
CEM + exact + plot_layout(guides = "collect")

ggsave("matching_epsa.pdf", width = 10, height = 6)
dev.off()
tools::texi2dvi("covbalance_cem.tex", pdf = T)
system(paste(getOption("pdfviewer"), "covbalance_cem.pdf"))


# for love.plot: rename variables
new.names <- c(
  "Sex", "Age", "Rural", "Employment: Full time", "Employment: Part time", "Employment: Self employed",
  "Employment: Retired/pensioned", "Employment: Housewife", "Employment: Student", "Employment: Unemployed",
  "Employment: Other", "Education", "Household Income", "Vote in national elections: Usually",
  "Vote in national elections: Never", "Vote in national elections: Not allowed to vote",
  "Generalized trust", "Political Interest", "Political Discussion: Occasionally", "Political Discussion: Never",
  "Perceived Political Corruption"
)

# for love.plot: pair old variables with new variable names
names(new.names) <- c(
  "Sex_2", "Age", "rural_2", "emplstat1", "emplstat2", "emplstat3",
  "emplstat4", "emplstat5", "emplstat6", "emplstat7",
  "emplstat8", "edu_recoded", "hhinc", "votenat2", "votenat3",
  "votenat4", "gentrust_2", "polint", "poldiscu2", "poldiscu3",
  "polcorup"
)


tikz("covbalance_exact.tex", standAlone = TRUE, width = 7, height = 7)

love.plot(m_exact,
          binary = "std", var.names = new.names,
          colors = c("black", "grey")
)

dev.off()
tools::texi2dvi("covbalance_exact.tex", pdf = T)
system(paste(getOption("pdfviewer"), "covbalance_exact.pdf"))



## 4.2. Matching Estimator  ####


# ATT average treatment effect on the treated
n_algorithms <- 5
att_data <- as.data.frame(matrix(NA, nrow = 10 * n_algorithms, ncol = 5))
colnames(att_data) <- c("y_var", "algorithm", "att", "se", "type")
att_data$y_var <- c(
  rep("inst_armed", n_algorithms),
  # rep("inst_press", n_algorithms),
  rep("inst_police", n_algorithms),
  rep("inst_courts", n_algorithms),
  rep("inst_parl", n_algorithms),
  rep("inst_gov", n_algorithms),
  rep("inst_parties", n_algorithms),
  rep("inst_comp", n_algorithms),
  rep("inst_UN", n_algorithms),
  rep("inst_banks", n_algorithms),
  rep("inst_uni", n_algorithms)
)
att_data$type <- c(
  rep("real spillover", 15),
  rep("endogenous spillover", 15),
  rep("no effect expected", 20)
)
att_data$algorithm <- rep(c("exact", "nearest", "subclass", "optimal", "cem"), 10)

iter <- 0
for (depvar in att_data$y_var) {
  iter <- iter + 1

  # based on exact matching
  data_exact$inst_armed <- as.factor(data_exact$inst_armed)
  data_exact$inst_press <- as.factor(data_exact$inst_press)
  data_exact$inst_police <- as.factor(data_exact$inst_police)
  data_exact$inst_courts <- as.factor(data_exact$inst_courts)
  data_exact$inst_parl <- as.factor(data_exact$inst_parl)
  data_exact$inst_gov <- as.factor(data_exact$inst_gov)
  data_exact$inst_parties <- as.factor(data_exact$inst_parties)
  data_exact$inst_comp <- as.factor(data_exact$inst_comp)
  data_exact$inst_UN <- as.factor(data_exact$inst_UN)
  data_exact$inst_banks <- as.factor(data_exact$inst_banks)
  data_exact$inst_uni <- as.factor(data_exact$inst_uni)

  reg <- clm(paste(depvar, "fraud1d", sep = "~"), data = data_exact, weights = weights)
  att_data[att_data$y_var == depvar & att_data$algorithm == "exact", "att"] <- reg$beta
  att_data[att_data$y_var == depvar & att_data$algorithm == "exact", "se"] <- sqrt(reg$vcov[4, 4])

  # based on nearest neighbor matching
  # reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_near, weights=weights)
  # att_data[att_data$y_var==depvar & att_data$algorithm=="nearest", "att"] <- coef(reg)[2]
  # att_data[att_data$y_var==depvar & att_data$algorithm=="nearest", "se"] <- se.coef(reg)[2]

  # based on subclassification matching
  # reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_sub, weights=weights)
  # att_data[att_data$y_var==depvar & att_data$algorithm=="subclass", "att"] <- coef(reg)[2]
  # att_data[att_data$y_var==depvar & att_data$algorithm=="subclass", "se"] <- se.coef(reg)[2]

  # based on optimal matching
  # reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_opti, weights=weights)
  # att_data[att_data$y_var==depvar & att_data$algorithm=="optimal", "att"] <- coef(reg)[2]
  # att_data[att_data$y_var==depvar & att_data$algorithm=="optimal", "se"] <- se.coef(reg)[2]

  # based on exact matching
  data_cem$inst_armed <- as.factor(data_cem$inst_armed)
  data_cem$inst_press <- as.factor(data_cem$inst_press)
  data_cem$inst_police <- as.factor(data_cem$inst_police)
  data_cem$inst_courts <- as.factor(data_cem$inst_courts)
  data_cem$inst_parl <- as.factor(data_cem$inst_parl)
  data_cem$inst_gov <- as.factor(data_cem$inst_gov)
  data_cem$inst_parties <- as.factor(data_cem$inst_parties)
  data_cem$inst_comp <- as.factor(data_cem$inst_comp)
  data_cem$inst_UN <- as.factor(data_cem$inst_UN)
  data_cem$inst_banks <- as.factor(data_cem$inst_banks)
  data_cem$inst_uni <- as.factor(data_cem$inst_uni)

  reg <- clm(paste(depvar, "fraud1d", sep = "~"), data = data_cem, weights = weights)
  att_data[att_data$y_var == depvar & att_data$algorithm == "cem", "att"] <- reg$beta
  att_data[att_data$y_var == depvar & att_data$algorithm == "cem", "se"] <- sqrt(reg$vcov[4, 4])
}

