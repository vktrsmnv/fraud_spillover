# install.packages(c("StanHeaders", "rstan"))
setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "magrittr",
    "sjlabelled",
    "patchwork",
    "styler",
    "here",
    "BayesPostEst",
    "bayesplot",
    "brms",
    "bayestestR",
    "parallel",
    "tidybayes",
    "modelr",
    "remotes",
    "viridis",
    "showtext",
    "ggthemes"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }

  remotes::install_github("rensa/stickylabeller")
  library(stickylabeller)
  lapply(p_needed, require, character.only = TRUE)
  options(mc.cores = parallel::detectCores())

  # Let's Add a Different Font for Plots!
  font_add_google("Source Sans Pro")
  showtext_auto()

  # setting a global theme for the entire document
  theme_set(theme_bw(base_family = "Source Sans Pro") +
    # theme_bw(base_family = "Source Sans Pro") +
    theme(
      # plot.background = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.title = element_blank(),
      legend.position = "bottom"
    ))
  formals(plasma)$end <- 0.8
  formals(scale_color_viridis)$end <- 0.8
  formals(scale_alpha_manual)$values <- c(0.5, 1)
}
setup()

# re-estimate the models?
estimate <- FALSE

# 1. Loading Data ####

data_rus <-
  read_csv(here("data/RU.csv")) %>% # manually coded quality
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
  filter(
    questnnr == "russia",
    finished == 1, # as per PAP, only work with finished cases
    age > 17,
    time_sum > 180, # as per PAP, exclude shorter than 3 minutes response times
  ) %>%
  select(-pa12, -pa17) %>%
  mutate(
    opponent = as.character(opponent),
    UR = ifelse(pa14 == "UR", "UR", "Non-UR"),
    CPRF = ifelse(pa14 == "CPRF", "CPRF", "Non-CPRF"),
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
  )


data_la <- read_csv(here("data/LA.csv")) %>%
  full_join(read_rds(here("data/toloka.rds")), .,
    by = c("case" = "CASE")
  ) %>%
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
  )

pol <- data_rus %>%
  dplyr::select(starts_with("pol_inst")) %>%
  colnames()
npol <- data_rus %>%
  dplyr::select(starts_with("npol_inst")) %>%
  colnames()

# 2. Main Analysis ####
## 2.0. Calculation Function ####

source("code/functions.R")

## 2.1. Estimation ####

if (estimate) {
  data <- data_rus

  data$condition <- str_replace_all(string = data$condition, pattern = " ", replacement = "")
  data <- cbind(model.matrix(~ condition - 1, data), data %>% select(starts_with("pol_")))
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
  data <- cbind(model.matrix(~ condition - 1, data), data %>% select(starts_with("pol_")))
  IVs <- "conditionControl + conditionJudicialPunishment + conditionPunishment"
  mediation_calc(
    data = data,
    inst = pol,
    IVs = "conditionControl + conditionJudicialPunishment + conditionPunishment",
    model = "ol",
    name = paste0("mediation_la_pol_", nrow(data))
  )
}

# 3. Plotting ####

model_files <- list.files("output", pattern = "ol_mediation_[a-z]+_pol_[0-9]")


for (i in model_files) {
  mm <- read_rds(paste0("output/", i))

  if (str_detect(string = i, pattern = "la")) {
    title <- "Latin America"
  } else {
    title <- "Russia"
  }

  plotting_mediation <- tibble()
  condition <- c("conditionControl", "conditionJudicialPunishment", "conditionPunishment")
  for (md in pol) {
    for (cn in condition) {
      tt <- bayestestR::mediation(mm[[md]],
        treatment = cn,
        mediator = "pol_election",
        ci = .89
      )

      plotting_mediation <- tt %>%
        as_tibble() %>%
        add_column(
          institution = md,
          treatment = cn,
          n = nrow(mm[[md]]$data)
        ) %>%
        janitor::clean_names() %>%
        slice(1:4) %>%
        bind_rows(plotting_mediation)
    }
  }

  temp <- plotting_mediation %>%
    mutate(
      significant = ifelse(ci_low < 0 & ci_high > 0, "no", "yes"),
      institution = as_factor(institution) %>%
        fct_relevel(
          "pol_inst_CEC",
          "pol_inst_part",
          "pol_inst_parl",
          "pol_inst_courts",
          "pol_inst_pres",
          "pol_inst_gov",
          "pol_inst_police",
          "pol_inst_armed"
        )
    ) %>%
    arrange(institution) %>%
    mutate(
      institution =
        case_when(
          institution == "pol_inst_armed" ~ paste0("Armed Forces,\nN = ", n),
          institution == "pol_inst_police" ~ paste0("Police,\nN = ", n),
          institution == "pol_inst_CEC" ~ paste0("Central Electoral\nCommission,\nN = ", n),
          institution == "pol_inst_gov" ~ paste0("Government,\nN = ", n),
          institution == "pol_inst_part" ~ paste0("Parties,\nN = ", n),
          institution == "pol_inst_parl" ~ paste0("Parliament,\nN = ", n),
          institution == "pol_inst_courts" ~ paste0("Courts,\nN = ", n),
          institution == "pol_inst_pres" ~ paste0("President,\nN = ", n)
        ),
      institution = fct_inorder(institution),
      treatment = treatment %>%
        str_remove("condition"),
      treatment = case_when(
        treatment == "JudicialPunishment" ~ "Judicial Punishment",
        TRUE ~ treatment
      ),
      treatment = treatment %>%
        factor(., levels = c(
          "Control", "Punishment", "Judicial Punishment"
        ))
    ) %>%
    ggplot(aes(
      x = estimate, xmin = ci_low, xmax = ci_high,
      y = effect,
      shape = treatment,
      color = treatment,
      alpha = significant
    )) +
    geom_pointrange(
      position = position_dodge(0.5),
      size = 0.4
    ) +
    facet_grid(
      rows = vars(treatment),
      cols = vars(institution),
    ) +
    scale_color_viridis(
      discrete = T,
      option = "C",
      end = 0.8
    ) +
    geom_vline(
      xintercept = 0,
      linetype = 2,
      color = "black",
      alpha = 0.5
    ) +
    labs(
      y = "",
      x = "Estimate",
      alpha = "",
      title = title,
      shape = ""
    ) +
    guides(alpha = "none", color = "none") +
    scale_alpha_manual(values = c(0.3, 1)) +
    guides(shape = guide_legend(override.aes = list(
      shape = c(16, 17, 15),
      color = viridis(3, end = 0.8, option = "C")
    )))
  assign(paste0("plot_", str_remove(i, ".rds")), temp)
}


pp <- eval(parse(text = paste0("plot_", str_remove(model_files, ".rds"), collapse = "/"))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
pp
ggsave(pp,
  filename = paste0(
    "figs/",
    str_remove(model_files[1], "ol_") %>% str_remove("_[a-z]+_pol") %>% str_remove(".rds"),
    ".png"
  ),
  height = 10,
  width = 10
)
