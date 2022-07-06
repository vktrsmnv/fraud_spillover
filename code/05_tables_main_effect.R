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
    "ggplot2",
    "viridis",
    "stringr",
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
}

setup()

formals(plasma)$end <- 0.8
formals(scale_color_viridis)$end <- 0.8
formals(scale_alpha_manual)$values <- c(0.5, 1)

## Political Institutions ####
### All Cases #####

model_files <- list.files("output", pattern = "ol_main_[a-z]+_pol_[0-9]")


for (i in model_files){
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)){
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(names(mpol) == "pol_inst_pres" ~ "President",
                           names(mpol) == "pol_inst_police" ~ "Police",
                           names(mpol) == "pol_inst_CEC" ~ "Central EC",
                           names(mpol) == "pol_inst_gov" ~ "Government",
                           names(mpol) == "pol_inst_part" ~ "Political Parties",
                           names(mpol) == "pol_inst_parl" ~ "Parliament",
                           names(mpol) == "pol_inst_courts" ~ "Courts",
                           names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")){
    tt <- "Latin American pooled sample."
  } else{
    tt <- "Russian sample."

  }
  BayesPostEst::mcmcReg(mpol[1:4], ci = 0.89,
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = rep(list(c("Intercept$_1$",
                                               "Intercept$_2$",
                                               "Intercept$_3$",
                                               "Control",
                                               "Punishment",
                                               "Judicial Punishment"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars  \\\\\nFraud treatment group serves as the baseline.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = rep(list(c("Intercept$_1$",
                                               "Intercept$_2$",
                                               "Intercept$_3$",
                                               "Control",
                                               "Punishment",
                                               "Judicial Punishment"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))[-c(1:2, 25:33)],
         readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))[-c(1:7)])
  b_caption <- b[str_detect(b,  pattern = "caption")]
  b <- b[!str_detect(b,  pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
    write(.,file=paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}



## non-political institutions
model_files <- list.files("output", pattern = "ol_main_[a-z]+_npol_[0-9]")


for (i in model_files){
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)){
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(names(mpol) == "npol_inst_comp" ~ "Companies",
                           names(mpol) == "npol_inst_banks" ~ "Banks",
                           names(mpol) == "npol_inst_env" ~ "Environmental\nOrganizations",
                           names(mpol) == "npol_inst_UN" ~ "United Nations",
                           names(mpol) == "npol_inst_WB" ~ "World Bank",
                           names(mpol) == "npol_inst_WTO" ~ "WTO",
  )

  if (str_detect(string = i, pattern = "la")){
    tt <- "Latin American pooled sample."
  } else{
    tt <- "Russian sample."

  }
  BayesPostEst::mcmcReg(mpol[1:3],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[1:3]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:3],
                        coefnames = rep(list(c("Intercept$_1$",
                                               "Intercept$_2$",
                                               "Intercept$_3$",
                                               "Control",
                                               "Punishment",
                                               "Judicial Punishment"
                        )), 3),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))

  BayesPostEst::mcmcReg(mpol[4:6],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[4:6]),
                        custom.note = "%stars  \\\\\nFraud treatment group serves as the baseline.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[4:6],
                        coefnames = rep(list(c("Intercept$_1$",
                                               "Intercept$_2$",
                                               "Intercept$_3$",
                                               "Control",
                                               "Punishment",
                                               "Judicial Punishment"
                        )), 3),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))[-c(1:2, 25:34)],
         readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))[-c(1:6)])
  b_caption <- b[str_detect(b,  pattern = "caption")]
  b <- b[!str_detect(b,  pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
  write(., file=paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


