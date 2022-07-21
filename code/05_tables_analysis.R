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

# Main Results #####
## Political Institutions ####

model_files <- list.files("output", pattern = "ol_main_[a-z]+_pol_[0-9]")


for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
    ci = 0.89,
    pars = "b",
    pointest = "median",
    regex = T,
    custom.gof.rows = list("Observations" = n_cases[1:4]),
    # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
    # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
    # caption = "Ordinal logistic regression results for Latin American sample",
    custom.model.names = names(mpol)[1:4],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )), 4),
    float.pos = "h",
    threeparttable = TRUE,
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
    ci = 0.89,
    pointest = "median",
    pars = "b",
    regex = T,
    custom.gof.rows = list("Observations" = n_cases[5:8]),
    custom.note = "%stars Reported are medians and 89\\% credible intervals.
    \\\\\nFraud treatment group serves as the baseline.",
    caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
    custom.model.names = names(mpol)[5:8],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )), 4),
    float.pos = "h",
    threeparttable = TRUE,
    label = paste0("table:", str_remove(i, pattern = ".rds")),
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}



## Non-political Institutions ####
model_files <- list.files("output", pattern = "ol_main_[a-z]+_npol_[0-9]")


for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "npol_inst_comp" ~ "Companies",
    names(mpol) == "npol_inst_banks" ~ "Banks",
    names(mpol) == "npol_inst_env" ~ "Environmental\nOrganizations",
    names(mpol) == "npol_inst_UN" ~ "United Nations",
    names(mpol) == "npol_inst_WB" ~ "World Bank",
    names(mpol) == "npol_inst_WTO" ~ "WTO",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:3],
    ci = 0.89,
    pars = "b",
    pointest = "median",
    regex = T,
    custom.gof.rows = list("Observations" = n_cases[1:3]),
    # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
    # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
    # caption = "Ordinal logistic regression results for Latin American sample",
    custom.model.names = names(mpol)[1:3],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )), 3),
    float.pos = "h",
    threeparttable = TRUE,
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[4:6],
    ci = 0.89,
    pars = "b",
    pointest = "median",
    regex = T,
    custom.gof.rows = list("Observations" = n_cases[4:6]),
    custom.note = "%stars  \\\\\nFraud treatment group serves as the baseline.",
    caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
    custom.model.names = names(mpol)[4:6],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment"
    )), 3),
    float.pos = "h",
    threeparttable = TRUE,
    label = paste0("table:", str_remove(i, pattern = ".rds")),
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b <- c(
    readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))[-c(1:2, 25:34)],
    readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))[-c(1:6)]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}



## Other Specifications ####
### Pooled and FE models ####
model_files <- list.files("output", pattern = "ol_main_pol_fe")
model_files

for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
                        ci = 0.89,
                        pars = "b",
                        pointest = "median",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Mexico"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pointest = "median",
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars Reported are medians and 89\\% credible intervals.
    \\\\\nFraud treatment group serves as the baseline.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Mexico"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = paste0("table:", str_remove(i, pattern = ".rds")),
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


model_files <- list.files("output", pattern = "ol_main_pol\\.")
model_files

for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
                        ci = 0.89,
                        pars = "b",
                        pointest = "median",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Mexico",
                          "Russia"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pointest = "median",
                        pars = "b",
                        regex = T,
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars Reported are medians and 89\\% credible intervals.
    \\\\\nFraud treatment group serves as the baseline.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Mexico",
                          "Russia"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = paste0("table:", str_remove(i, pattern = ".rds")),
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


# Results with Controls #####
## Political Institutions ####

model_files <- list.files("output", pattern = "ol_controls_[a-z]+_pol_[0-9]")


for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
    var_names <- rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Opponent",
      "Political Interest: Somewhat Interested",
      "Political Interest: Not Very Interested",
      "Political Interest: Not at All Interested",
      "Low Generalized Trust",
      "Age (log)",
      "Male",
      "Education: Middle",
      "Education: Higher",
      "Employment: Retired",
      "Employment: Housewife",
      "Employment: Student",
      "Employment: Unemployed",
      "Employment: Other",
      "Sector: Private Business or Industry",
      "Sector: Private Non-profit",
      "Savings: Just Got By",
      "Savings: Spent Some Savings",
      "Savings: Spent Savings and Borrowed",
      "Rural",
      "Political Corruption",
      "Mexico"
    )), 4)
  } else {
    tt <- "Russian sample."
    var_names <- rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Opponent",
      "Political Interest: Somewhat Interested",
      "Political Interest: Not Very Interested",
      "Political Interest: Not at All Interested",
      "Low Generalized Trust",
      "Age (log)",
      "Male",
      "Education: Middle",
      "Education: Higher",
      "Employment: Retired",
      "Employment: Housewife",
      "Employment: Student",
      "Employment: Unemployed",
      "Employment: Other",
      "Sector: Private Business or Industry",
      "Sector: Private Non-profit",
      "Savings: Just Got By",
      "Savings: Spent Some Savings",
      "Savings: Spent Savings and Borrowed",
      "Rural",
      "Political Corruption"
    )), 4)
  }


  BayesPostEst::mcmcReg(mpol[1:4],
    pointest = "median",
    ci = 0.89,
    pars = "b",
    regex = T,
    custom.gof.rows = list("Observations" = n_cases[1:4]),
    custom.note = "%stars. Repoted are medians and 89\\% credible intervals.
                        \\\\\nRespective baseline categories are Fraud, Supporter, Employment: paid employment, Sector: Government or public institution",
    caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
    custom.model.names = names(mpol)[1:4],
    coefnames = var_names,
    float.pos = "h",
    threeparttable = TRUE,
    label = paste0("table:", str_remove(str_replace_all(i, replacement = "-", pattern = "_"), pattern = ".rds")),
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
    ci = 0.89,
    pars = "b",
    regex = T,
    pointest = "median",
    custom.gof.rows = list("Observations" = n_cases[5:8]),
    custom.note = "%stars Repoted are medians and 89\\% credible intervals.
                        \\\\\nRespective baseline categories are Fraud, Supporter, Employment: paid employment, Sector: Government or public institution",
    # custom.note = "%stars. Repoted are medians and 89% credible intervals. \\\\\nFraud treatment group serves as the baseline.",
    caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt, " (cont.)"),
    custom.model.names = names(mpol)[5:8],
    coefnames = var_names,
    float.pos = "h",
    threeparttable = TRUE,
    label = "",
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  # change the caption position in first file
  b <- c(readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))[-c(1:2)])
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], "\\small", b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))

  # change the caption position in second file
  b <- c(readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))[-c(1:2)])
  b_caption <- b[str_detect(b, pattern = "caption")]

  b <- b[!str_detect(b, pattern = "caption")]
  c(
    b[1:2],"\\small", paste0(
      str_trunc(b_caption, width = 8, ellipsis = ""), "*",
      str_trunc(b_caption, width = str_length(b_caption) - 8, ellipsis = "", side = "left")
    ),
    b[3:length(b)]
  ) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  # put tables into a single tex file
  c(
    readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")),
    readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  ) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))

  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


# Conditional Effect Results #####
## Political Institutions ####

model_files <- list.files("output", pattern = "ol_cond_[a-z]+_pol_[0-9]")


for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
    ci = 0.89,
    pars = "b",
    regex = T,
    pointest = "median",
    custom.gof.rows = list("Observations" = n_cases[1:4]),
    # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
    # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
    # caption = "Ordinal logistic regression results for Latin American sample",
    custom.model.names = names(mpol)[1:4],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Opponent",
      "Control $\\times$ Opponent",
      "Punishment $\\times$ Opponent",
      "Judicial Punishment $\\times$ Opponent"
    )), 4),
    float.pos = "h",
    threeparttable = TRUE,
    label = "",
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
    ci = 0.89,
    pars = "b",
    regex = T,
    pointest = "median",
    custom.gof.rows = list("Observations" = n_cases[5:8]),
    custom.note = "%stars. Repoted are medians and 89\\% credible intervals.",
    caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
    custom.model.names = names(mpol)[5:8],
    coefnames = rep(list(c(
      "Intercept$_1$",
      "Intercept$_2$",
      "Intercept$_3$",
      "Control",
      "Punishment",
      "Judicial Punishment",
      "Opponent",
      "Control $\\times$ Opponent",
      "Punishment $\\times$ Opponent",
      "Judicial Punishment $\\times$ Opponent"
    )), 4),
    float.pos = "h",
    threeparttable = TRUE,
    label = paste0("table:", str_remove(str_replace_all(i, replacement = "-", pattern = "_"), pattern = ".rds")),
    file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], "\\small", b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


### Other Specifications ####

#### Political Interest Interaction ####
model_files <- list.files("output",
                          pattern = "ol_polint_[a-z]+_pol_[0-9]")
model_files

for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Pol. Interest: Somewhat",
                          "Pol. Interest: Not Very",
                          "Pol. Interest: Not at All",
                          "Control $\\times$ Pol. Interest: Somewhat",
                          "Punishment $\\times$ Pol. Interest: Somewhat",
                          "Judicial Punishment $\\times$ Pol. Interest: Somewhat",

                          "Control $\\times$ Pol. Interest: Not Very",
                          "Punishment $\\times$ Pol. Interest: Not Very",
                          "Judicial Punishment $\\times$ Pol. Interest: Not Very",

                          "Control $\\times$ Pol. Interest: Not at All",
                          "Punishment $\\times$ Pol. Interest: Not at All",
                          "Judicial Punishment $\\times$ Pol. Interest: Not at All"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = "",
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars. Repoted are medians and 89\\% credible intervals.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "Pol. Interest: Somewhat",
                          "Pol. Interest: Not Very",
                          "Pol. Interest: Not at All",
                          "Control $\\times$ Pol. Interest: Somewhat",
                          "Punishment $\\times$ Pol. Interest: Somewhat",
                          "Judicial Punishment $\\times$ Pol. Interest: Somewhat",

                          "Control $\\times$ Pol. Interest: Not Very",
                          "Punishment $\\times$ Pol. Interest: Not Very",
                          "Judicial Punishment $\\times$ Pol. Interest: Not Very",

                          "Control $\\times$ Pol. Interest: Not at All",
                          "Punishment $\\times$ Pol. Interest: Not at All",
                          "Judicial Punishment $\\times$ Pol. Interest: Not at All"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = paste0("table:", str_remove(str_replace_all(i, replacement = "-", pattern = "_"), pattern = ".rds")),
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )

  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], "\\small", b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}


#### Party ID Interaction ####
model_files <- list.files("output",
                          pattern = "ol_cond_ru+_pol_pa14_[0-9]")
model_files

for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  var_names <- rep(list(c(
    "Intercept$_1$",
    "Intercept$_2$",
    "Intercept$_3$",
    "Control",
    "Punishment",
    "Judicial Punishment",

    "Just Russia",
    "LDPR",
    "None",
    "Other",
    "United Russia",

    "Control $\\times$ Just Russia",
    "Punishment $\\times$ Just Russia",
    "Judicial Punishment $\\times$ Just Russia",

    "Control $\\times$ LDPR",
    "Punishment $\\times$ LDPR",
    "Judicial Punishment $\\times$ LDPR",

    "Control $\\times$ None",
    "Punishment $\\times$ None",
    "Judicial Punishment $\\times$ None",


    "Control $\\times$ Other",
    "Punishment $\\times$ Other",
    "Judicial Punishment $\\times$ Other",

    "Control $\\times$ United Russia",
    "Punishment $\\times$ United Russia",
    "Judicial Punishment $\\times$ United Russia"
  )), 4)
  BayesPostEst::mcmcReg(mpol[1:4],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = var_names,
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = "",
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars. Repoted are medians and 89\\% credible intervals.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = var_names,
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = paste0("table:", str_remove(str_replace_all(i, replacement = "-", pattern = "_"), pattern = ".rds")),
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )

  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], "\\small", b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}

#### Alternative Opponent Definition ####

model_files <- list.files("output", pattern = "ol_cond_[a-z]+_pol_UR_[0-9]")
model_files
for (i in model_files) {
  mpol <- read_rds(paste0("output/", i))

  n_cases <- rep(NA, length(mpol))
  for (j in 1:length(mpol)) {
    n_cases[j] <- nrow(mpol[[j]]$data)
  }

  names(mpol) <- case_when(
    names(mpol) == "pol_inst_pres" ~ "President",
    names(mpol) == "pol_inst_police" ~ "Police",
    names(mpol) == "pol_inst_CEC" ~ "Central EC",
    names(mpol) == "pol_inst_gov" ~ "Government",
    names(mpol) == "pol_inst_part" ~ "Political Parties",
    names(mpol) == "pol_inst_parl" ~ "Parliament",
    names(mpol) == "pol_inst_courts" ~ "Courts",
    names(mpol) == "pol_inst_armed" ~ "Armed Forces",
  )

  if (str_detect(string = i, pattern = "la")) {
    tt <- "Latin American pooled sample."
  } else {
    tt <- "Russian sample."
  }
  BayesPostEst::mcmcReg(mpol[1:4],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[1:4]),
                        # custom.note = "%stars.  \\\\\nThis table examines the effect of information about fraud
                        # and punitive measures on institutional trust. \\\\\nFraud treatment group serves as the baseline.",
                        # caption = "Ordinal logistic regression results for Latin American sample",
                        custom.model.names = names(mpol)[1:4],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "United Russia Supporter",
                          "Control $\\times$ United Russia Supporter",
                          "Punishment $\\times$ United Russia Supporter",
                          "Judicial Punishment $\\times$ United Russia Supporter"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = "",
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex")
  )

  BayesPostEst::mcmcReg(mpol[5:8],
                        ci = 0.89,
                        pars = "b",
                        regex = T,
                        pointest = "median",
                        custom.gof.rows = list("Observations" = n_cases[5:8]),
                        custom.note = "%stars. Repoted are medians and 89\\% credible intervals.",
                        caption = paste0("Ordinal logistic regression estimates of treatment effects for ", tt),
                        custom.model.names = names(mpol)[5:8],
                        coefnames = rep(list(c(
                          "Intercept$_1$",
                          "Intercept$_2$",
                          "Intercept$_3$",
                          "Control",
                          "Punishment",
                          "Judicial Punishment",
                          "United Russia Supporter",
                          "Control $\\times$ United Russia Supporter",
                          "Punishment $\\times$ United Russia Supporter",
                          "Judicial Punishment $\\times$ United Russia Supporter"
                        )), 4),
                        float.pos = "h",
                        threeparttable = TRUE,
                        label = paste0("table:", str_remove(str_replace_all(i, replacement = "-", pattern = "_"), pattern = ".rds")),
                        file = paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex")
  )

  b1 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
  b2 <-  readLines(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))

  b <- c(
    b1[-c(1:2,
          str_which(b1, "\\\\end\\{tabular"):length(b1))],
    b2[-c(1:str_which(b2, "\\\\hline")[1])]
  )
  b_caption <- b[str_detect(b, pattern = "caption")]
  b <- b[!str_detect(b, pattern = "caption")]
  c(b[1:2], "\\small", b_caption, b[3:length(b)]) %>%
    write(., file = paste0("tables/", str_remove(i, pattern = ".rds"), ".tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_2.tex"))
  file.remove(paste0("tables/", str_remove(i, pattern = ".rds"), "_1.tex"))
}

