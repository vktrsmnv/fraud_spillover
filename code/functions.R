matching <- function(z,
                     score,
                     shuffle = TRUE,
                     random_ties = TRUE) {
  # set.seed(1201) # to get the same random draws if shuffling
  n <- length(score)
  nt <- sum(z)
  nc <- sum(1 - z)
  names(score) <- 1:n # set names to index in the main dataset
  matched <- rep(0, nt)
  if (shuffle) {
    score <- sample(score, size = n, replace = F)
  }
  scorec <- score[z == 0]
  scoret <- score[z == 1]
  if (random_ties) { # if shuffle == TRUE, this may not even be necessary
    which.min <- function(x) {
      y <- which(x == min(x))
      if (length(y) > 1L) sample(y, 1L) else y
    }
  }
  for (i in 1:nt) {
    min_score <- which.min(abs(scorec - scoret[i]))
    scorec <- scorec[-min_score]
    matched[i] <-
      as.numeric(names(min_score)) # name in the main dataset
    if (length(scorec) == 0) {
      break
    }
  }

  matched_ind <- c(matched, as.numeric(names(scoret)[1:i]))
  cnts <- as.numeric(1:n %in% matched_ind)
  out <- list(match.ind = matched_ind, cnts = cnts)
  # print(score)
  return(out)
}

model_calc <- function(data,
                       inst,
                       IVs,
                       model = "ol",
                       iter = 6000,
                       cores = 4,
                       chains = 4,
                       warmup = 3500,
                       seed = 1201,
                       name) {
  # empty objects for storing
  mods <- list()
  # polinst_mods_ol <- npolinst_mods_ol <- list()

  # transform variables to factors for OL model
  data1 <- data %>%
    mutate(
      across(starts_with("pol_inst_"), ~ as.numeric(.x)),
      across(starts_with("npol_inst"), ~ as.numeric(.x))
    )

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")

  for (DV in inst) {
    if (match(DV, inst) == 1) {
      if (model == "lm") {
        mods[[DV]] <-
          first <-
          brm(
            formula = paste(DV, IVs, sep = "~"),
            data = data,
            iter = iter,
            warmup = warmup,
            chains = chains,
            cores = cores,
            seed = seed
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
      if (model == "ol") {
        mods[[DV]] <-
          first <-
          brm(
            paste(DV, IVs, sep = "~"),
            data = data1,
            family = cumulative("logit"),
            iter = iter,
            warmup = warmup,
            chains = chains,
            cores = cores,
            seed = seed
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
    } else {
      if (model == "lm") {
        mods[[DV]] <-
          update(first,
            formula. = paste(DV, IVs, sep = "~"),
            newdata = data
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
      if (model == "ol") {
        mods[[DV]] <-
          update(first,
            formula. = paste(DV, IVs, sep = "~"),
            newdata = data1
          )
        write_rds(
          mods,
          paste0(
            "output/",
            model,
            "_",
            name,
            ".rds"
          )
        )
      }
    }
  }
}

mediation_calc <- function(data,
                           inst,
                           IVs,
                           model,
                           iter = 10000,
                           cores = 4,
                           chains = 4,
                           warmup = 7500,
                           seed = 1201,
                           name) {
  # empty objects for storing
  mods <- list()
  # polinst_mods_ol <- npolinst_mods_ol <- list()

  # transform variables to factors for OL model
  data1 <- data %>%
    mutate(across(starts_with("pol_inst_"), ~ as.numeric(.x)),
           across(starts_with("npol_inst"), ~ as.numeric(.x))) #

  # time <- format(Sys.time(), "%b%d_%H_%M_%S")
  for (DV in inst) {
    f1 <- bf(paste("pol_election", IVs, sep = "~"),
             family = "cumulative")
    f2 <- bf(paste(DV,
                   paste("pol_election", IVs, sep = "+"),
                   sep = "~"),
             family = "cumulative")
    mods[[DV]] <-
      brm(
        formula = f1 + f2 + set_rescor(FALSE),
        data = data1,
        family = cumulative("logit"),
        iter = iter,
        warmup = warmup,
        chains = chains,
        cores = cores,
        seed = seed
      )
    write_rds(mods,
              paste0("output/",
                     model,
                     "_",
                     name,
                     ".rds"))
  }

}
