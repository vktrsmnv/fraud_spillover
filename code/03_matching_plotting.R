names <- list.files("output/", pattern = "models_", include.dirs = T)

models <- list()

for (i in names){
  models <- c(models, readRDS(paste0("output/", i)))
}

saveRDS(models, file = "output/models.rds")

fake.predict.clmm <- function(model, newdata) {
  # Actual prediction function
  pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
    Theta <- c(-1000, theta, 1000)
    sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))
  }

  # Multiply each row by the coefficients
  coefs <- c(model$beta, unlist(model$ST))
  xbetas <- sweep(newdata, MARGIN=2, coefs, `*`)

  # Make predictions
  pred.mat <- data.frame(pred(eta=rowSums(xbetas), theta=model$Theta))
  colnames(pred.mat) <- levels(model$model[,1])
  pred.mat
}

# get prections from NN matching (all 1000 models, no extra uncertainty)

pps <- tibble()
for (i in 1:1000){
  pps <- bind_rows(pps,
                   fake.predict.clmm(models[[i]],
                                     newdata = data.frame(fraud1d = c(0,1))) %>%
                     bind_cols(fraud1d = c(0,1)))
}

pps0 <- pps %>% filter(fraud1d == 0)
pps1 <- pps %>% filter(fraud1d == 1)

diffs <- pps0[,1:4] - pps1[,1:4] %>%
  apply(., 2, quantile(0.025, 0.05, 0.5, 0.95, 0.975)) %>%
  janitor::clean_names()

diffs %>%
  mutate(significant = ifelse(lower < 0 & upper > 0, "no", "yes")) %>%
  mutate(
    institution = case_when(
      institution == "npol_inst_comp" ~ "Companies",
      institution == "npol_inst_banks" ~ "Banks",
      institution == "npol_inst_env" ~ "Environmental Organizations",
      institution == "npol_inst_UN" ~ "United Nations",
      institution == "npol_inst_WB" ~ "World Bank",
      institution == "npol_inst_WTO" ~ "WTO"
    ),
    institution_facet_name = paste0(institution, "\nN = ", n),
    Condition = condition %>%
      factor(., levels = c(
        "Control", "Fraud", "Punishment", "Judicial Punishment"
      )),
    category = case_when(
      category == 1 ~ "None\nat all",
      category == 2 ~ "Not very\nmuch",
      category == 3 ~ "Quite\na Lot",
      category == 4 ~ "A Great\nDeal",
    ) %>%
      fct_inorder()
  ) %>%
  arrange(institution) %>%
  mutate(institution_facet_name = fct_inorder(institution_facet_name))



