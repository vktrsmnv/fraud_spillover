# ------------------------------------------------------------------------------
# Perceptions of Election Fraud, Judicial Rulings and Diffuse Political Support
# Lion Behrens and Viktoriia Semenova 
# Reproduction File 
# ------------------------------------------------------------------------------

library(stringr)
library(tidyverse)
library(ordinal)
library(cobalt) # for covariate balance assessment with love.plot()
library(tikzDevice)      # to export plots
library(arm) # for se.coef function
library(stargazer)

### ------------------------------------
# 1. data preparation 
### ------------------------------------

  # ------------------------------------
  # 1.1 key variables of interest
  # ------------------------------------

  # data construction: wvs_nelda_merging.R
  wvs_nelda <- as.data.frame(readRDS("../data/WVS_NELDA.RDS"))
  for (col in 1:ncol(wvs_nelda))
    if (!is.null(attributes(wvs_nelda[,col])$label))
      colnames(wvs_nelda)[col] <- attributes(wvs_nelda[,col])$label
  
  # wave availability: election integrity perceptions
  fraud_vars <- which(str_detect(colnames(wvs_nelda), "How often in"))
  fraud_aval <- matrix(0, nrow=length(fraud_vars), ncol=7)
  rownames(fraud_aval) <- colnames(wvs_nelda)[fraud_vars]
  colnames(fraud_aval) <- str_c("Wave ", 1:7)
  iter <- 0
  for (var in fraud_vars){
    iter <- iter + 1  
    fraud_aval[iter, which(rowSums(table(wvs_nelda$Wave, wvs_nelda[,var])) != 0)] <- 1
  }
  print(fraud_aval) # fraud perception variables available in Waves 6-7
  
  wvs_nelda$fraud1 <- wvs_nelda$`How often in country's elections: Votes are counted fairly`
  wvs_nelda$fraud2 <- wvs_nelda$`How often in country's elections: Voters are threatened with  violence at the po`
  
  
  # wave availability: confidence in institutions
  conf_vars <- which(str_detect(colnames(wvs_nelda), "Confidence"))
  conf_aval <- matrix(0, nrow=length(conf_vars), ncol=7)
  rownames(conf_aval) <- colnames(wvs_nelda)[conf_vars]
  colnames(conf_aval) <- str_c("Wave ", 1:7)
  iter <- 0
  for (var in conf_vars){
    iter <- iter + 1  
    conf_aval[iter, which(rowSums(table(wvs_nelda$Wave, wvs_nelda[,var])) != 0)] <- 1
  }
  print(conf_aval) 
  
  ### LB: candidates are confidence variables that are observed in both waves 6 and 7 
  
  # outcome variables that we expect a spillover to 
  conf_aval[c("Confidence: Armed Forces", # real spillover
              "Confidence: The Press", # real spillover
              "Confidence: The Police", # real spillover
              "Confidence: Justice System/Courts", # real spillover
              "Confidence: Parliament", # endogenous to fraud treatment
              "Confidence: The Government", # endogenous to fraud treatment
              "Confidence: The Political Parties"), 6:7] # endogenous to fraud treatment
  
  # outcome varaibles that we don't expect a spillover to
  conf_aval[c("Confidence: Major Companies", 
              "Confidence: The United Nations", 
              "Confidence: Banks",
              "Confidence: Universities"), 6:7] 
  
  wvs_nelda$inst_armed <- fct_rev(as.factor(wvs_nelda$`Confidence: Armed Forces`))
  wvs_nelda$inst_press <- fct_rev(as.factor(wvs_nelda$`Confidence: The Press`))
  wvs_nelda$inst_police <- fct_rev(as.factor(wvs_nelda$`Confidence: The Police`))
  wvs_nelda$inst_courts <- fct_rev(as.factor(wvs_nelda$`Confidence: Justice System/Courts`))
  wvs_nelda$inst_parl <- fct_rev(as.factor(wvs_nelda$`Confidence: Parliament`))
  wvs_nelda$inst_gov <- fct_rev(as.factor(wvs_nelda$`Confidence: The Government`))
  wvs_nelda$inst_parties <- fct_rev(as.factor(wvs_nelda$`Confidence: The Political Parties`))
  wvs_nelda$inst_comp <- fct_rev(as.factor(wvs_nelda$`Confidence: Major Companies`))
  wvs_nelda$inst_UN <- fct_rev(as.factor(wvs_nelda$`Confidence: The United Nations`))
  wvs_nelda$inst_banks <- fct_rev(as.factor(wvs_nelda$`Confidence: Banks`))
  wvs_nelda$inst_uni <- fct_rev(as.factor(wvs_nelda$`Confidence: Universities`))
  
  # subset data to WVS waves 6-7
  wvs_nelda <- wvs_nelda[wvs_nelda$Wave == 6 | wvs_nelda$Wave == 7,]
  

  # ------------------------------------
  # 1.2 control variables
  # ------------------------------------

  # interest in politics
  table(wvs_nelda$Wave, 
        wvs_nelda$`Interest in politics`, 
        exclude = NULL)
  wvs_nelda$polint <- wvs_nelda$`Interest in politics`
  
  # generalized trust
  table(wvs_nelda$Wave, 
        wvs_nelda$`Most people can be trusted`,
        exclude = NULL)
  wvs_nelda$gentrust <- wvs_nelda$`Most people can be trusted`
  
  # voted in last national election (always, usually, never, not allowed)
  table(wvs_nelda$Wave, 
        wvs_nelda$`Vote in elections: National level`, 
        exclude = NULL)
  wvs_nelda$`Vote in elections: National level` <- 
    as.factor(wvs_nelda$`Vote in elections: National level`)
  wvs_nelda$votenat <- wvs_nelda$`Vote in elections: National level`
  
  # sex
  table(wvs_nelda$Wave, 
        wvs_nelda$Sex, 
        exclude = NULL)
  
  # age
  table(wvs_nelda$Wave, 
        wvs_nelda$Age, 
        exclude = NULL)
  
  # education 
  table(wvs_nelda$Wave, 
        wvs_nelda$`Highest educational level attained`,
        exclude = NULL) # wave 6
  table(wvs_nelda$Wave, 
        wvs_nelda$`Highest educational level attained - Respondent`,
        exclude = NULL) # wave 7
  
  wvs_nelda$edu_recoded <- NA
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 1)] <- 1
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 2)] <- 2
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 3)] <- 3
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 4)] <- 3
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 5)] <- 3
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 6)] <- 4
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 7)] <- 5
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained` == 8)] <- 6
  
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 0)] <- 1
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 1)] <- 2
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 2)] <- 3
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 3)] <- 4
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 4)] <- 5
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 5)] <- 5
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 6)] <- 6
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 7)] <- 6
  wvs_nelda$edu_recoded[which(wvs_nelda$`Highest educational level attained - Respondent` == 8)] <- 6
  
  table(wvs_nelda$Wave, 
        wvs_nelda$edu_recoded,
        exclude = NULL) 
  
  # employment status
  table(wvs_nelda$Wave, 
        wvs_nelda$`Employment status`,
        exclude = NULL)
  wvs_nelda$emplstat <- as.factor(wvs_nelda$`Employment status`)
  
  # scale of incomes
  table(wvs_nelda$Wave, 
        wvs_nelda$`Scale of incomes`,
        exclude = NULL)
  wvs_nelda$hhinc <- wvs_nelda$`Scale of incomes`
  
  
  # ------------------------------------
  # 1.3 excluded variables
  # ------------------------------------
  
  # urban/rural
  table(wvs_nelda$Wave, 
        wvs_nelda$`Urban/Rural habitat`,
        exclude = NULL)
  wvs_nelda$rural <- wvs_nelda$`Urban/Rural habitat`
  
  # political discussion
  table(wvs_nelda$Wave, 
        wvs_nelda$`How often discusses political matters with friends`, 
        exclude = NULL)
  wvs_nelda$poldiscu <- as.factor(wvs_nelda$`How often discusses political matters with friends`)
  
  
  # extent of political corruption
  table(wvs_nelda$Wave, 
        wvs_nelda$`Extent of political corruption`, 
        exclude = NULL)
  
  # scale of political corruption
  table(wvs_nelda$Wave, 
        wvs_nelda$`Scale corruption in [my country] - pay a bribe, give a gift, do a favor to other`, 
        exclude = NULL)
  wvs_nelda$polcorup <- wvs_nelda$`Scale corruption in [my country] - pay a bribe, give a gift, do a favor to other`
  
  # education 
  table(wvs_nelda$Wave, 
        wvs_nelda$`educational level respondent (country specific)`,
        exclude = NULL)
  table(wvs_nelda$Wave, 
        wvs_nelda$`Education (country specific)`,
        exclude = NULL)
  table(wvs_nelda$Wave, 
        wvs_nelda$`Education level (recoded)`,
        exclude = NULL)
  table(wvs_nelda$Wave, 
        wvs_nelda$`What age did you complete your education`,
        exclude = NULL)
  table(wvs_nelda$Wave, 
        wvs_nelda$`What age did you complete your education (recoded in intervals)`,
        exclude = NULL)
  
  # occupational group
  table(wvs_nelda$Wave, 
        wvs_nelda$`Respondent - Occupational group (WVS)`,
        exclude = NULL)
  wvs_nelda$occgroup <- as.factor(wvs_nelda$`Respondent - Occupational group (WVS)`)
  
  # income level
  table(wvs_nelda$Wave, 
        wvs_nelda$`Income level`,
        exclude = NULL)
  
  # institution of occupation
  table(wvs_nelda$Wave, 
        wvs_nelda$`Institution of occupation`,
        exclude = NULL)
  
  
  
 
  # ------------------------------------
  # 1.4 group indicator variables
  # ------------------------------------
  
  # 108 country x year combinations
  wvs_nelda$cntry_year <- as.factor(wvs_nelda$`Country - year`)
  
  
### ------------------------------------
# 2. ordinal logistic mixed models
### ------------------------------------

  ### first single-level regressions with fixed effects for cntry_year
  ### definitely control for social trust
  ### only based on wave 7 with more controls?
  ### wave 7: nest individuals in countries, control for year using fixed effects
  
  
  # reduce to wave 7
  wvs_nelda <- wvs_nelda[wvs_nelda$Wave == 7,]
  
  # dichotomize fraud variable to allow comparability with matching estimator
  wvs_nelda$fraud1d <- NA
  wvs_nelda$fraud1d[wvs_nelda$fraud1==1 | wvs_nelda$fraud1==2] <- 0
  wvs_nelda$fraud1d[wvs_nelda$fraud1==3 | wvs_nelda$fraud1==4] <- 1
    
  x <- "fraud1d"
  contr <- c("polint","gentrust", "votenat","Sex","Age", "edu_recoded", "emplstat", 
             "hhinc", "rural", "poldiscu", "polcorup", "`CoW country code alpha`", "`Year survey`")
  
  
  # summary statistics
  wvs_nelda_complete <- wvs_nelda[,c("inst_armed", "inst_press", "inst_police", "inst_courts",
                                     "inst_parl", "inst_gov", "inst_parties", "fraud1d", 
                                     "polint","gentrust", "votenat","Sex","Age", 
                                     "edu_recoded", "emplstat", 
                                     "hhinc", "rural", "poldiscu", "polcorup")]
  wvs_nelda_complete <- wvs_nelda_complete[complete.cases(wvs_nelda_complete),]
  wvs_nelda_complete <- as.data.frame(lapply(wvs_nelda_complete, as.numeric))
  
  stargazer(wvs_nelda_complete, 
            summary=T)
  
  # ------------------------------------
  # 2.1 institutions: real spillover
  # ------------------------------------
  
  y_spill <- c("inst_armed", "inst_police", "inst_courts")
  m_spill <- list()
  # iter <- 0
  for (depvar in y_spill) {
    # iter <- iter + 1
    m_spill[depvar] <- list(summary(clm(paste(depvar, paste(c(x, contr), collapse="+"), sep="~"),
                                   link="logit", data=wvs_nelda)))
  }
  # names(m_spill) <- y_spill
  
  
  # --------------------------------------
  # 2.2 institutions: endogenous spillover
  # --------------------------------------

  y_spill_end <- c("inst_parl", "inst_gov", "inst_parties")
  m_spill_end <- list()
  # iter <- 0
  for (depvar in y_spill_end) {
    # iter <- iter + 1
    m_spill_end[depvar] <- list(summary(clm(paste(depvar, paste(c(x, contr), collapse="+"), sep="~"),
                                      link="logit", data=wvs_nelda)))
  }
  # names(m_spill_end) <- y_spill_end
  
  
  
  # ------------------------------------
  # 2.3 institutions: no spillover
  # ------------------------------------
  
  y_nospill <- c("inst_comp", "inst_UN", "inst_banks")
  m_nospill <- list()
  iter <- 0
  for (depvar in y_nospill) {
    # iter <- iter + 1
    m_nospill[depvar] <- list(summary(clm(paste(depvar, paste(c(x, contr), collapse="+"), sep="~"),
                                          link="logit", data=wvs_nelda)))
  }
  # names(m_nospill) <- y_nospill
  
  
  
### ------------------------------------
# 3. frequentist matching
### ------------------------------------
  
  library(MatchIt)  # for matching
  library(optmatch) # for Optimal Matching
  library(cem) # for coarsened exact ,atching
  
  
    # -----------------------------
    # 3.1 data preparation 
    # -----------------------------
  
      # reduce data to wave 7
      wvs_nelda7 <- wvs_nelda[wvs_nelda$Wave == 7,]
      match_data <- wvs_nelda7[,c("inst_armed", "inst_press", "inst_police", "inst_courts",
                                 "inst_parl", "inst_gov", "inst_parties", "inst_comp", 
                                 "inst_UN", "inst_banks", "inst_uni", "fraud1", 
                                 "polint","gentrust", "votenat","Sex","Age", "edu_recoded", 
                                 "emplstat", "hhinc", "rural", "poldiscu", "polcorup")]
      match_data <- match_data[complete.cases(match_data),]
      
      # dichotomize treatment
      match_data$fraud1d <- NA
      match_data$fraud1d[match_data$fraud1==1 | match_data$fraud1==2] <- 0
      match_data$fraud1d[match_data$fraud1==3 | match_data$fraud1==4] <- 1
      
      # de-factorize outcomes
      match_data$inst_armed <- as.numeric(as.character(match_data$inst_armed))
      match_data$inst_press <- as.numeric(as.character(match_data$inst_press))
      match_data$inst_police <- as.numeric(as.character(match_data$inst_police))
      match_data$inst_courts <- as.numeric(as.character(match_data$inst_courts))
      match_data$inst_parl <- as.numeric(as.character(match_data$inst_parl))
      match_data$inst_gov <- as.numeric(as.character(match_data$inst_gov))
      match_data$inst_parties <- as.numeric(as.character(match_data$inst_parties))
      match_data$inst_comp <- as.numeric(as.character(match_data$inst_comp))
      match_data$inst_UN <- as.numeric(as.character(match_data$inst_UN))
      match_data$inst_banks <- as.numeric(as.character(match_data$inst_banks))
      match_data$inst_uni <- as.numeric(as.character(match_data$inst_uni))
      
      
      
      
    # ---------------------------------------------
    # 3.2 estimate PS model, generate matched data
    # ---------------------------------------------
      
      # define models to calculate propensity scores
      ps_model <- as.formula(fraud1d ~ Sex + Age + rural + emplstat + 
                               edu_recoded + hhinc + votenat + gentrust + polint + poldiscu + polcorup)
      
      
      
      
      # apply matching algorithms based on propensity scores
      m_exact <- matchit(ps_model, data=match_data, method = "exact") 
      #m_near <- matchit(ps_model, data=match_data, method = "nearest")
      #m_sub <- matchit(ps_model, data=match_data, method = "subclass")
      #m_opti <- matchit(ps_model, data=match_data, method = "optimal") # cannot allocate vector of size 4GB
      m_cem <- matchit(ps_model, data=match_data, method = "cem")
      
      # store matched data sets
      data_exact <- match.data(m_exact)
      #data_near <- match.data(m_near)
      #data_sub <- match.data(m_sub)
      #data_opti <- match.data(m_opti)
      data_cem <- match.data(m_cem)
      
      
    # ---------------------------------------------
    # 3.3 visual assessment of balance
    # ---------------------------------------------
      
      par(mfrow=c(1,1))
      
      # for love.plot: rename variables
      new.names <- c("Sex", "Age", "Rural", "Employment: Full time", "Employment: Part time", "Employment: Self employed",
                     "Employment: Retired/pensioned", "Employment: Housewife", "Employment: Student", "Employment: Unemployed", 
                     "Employment: Other", "Education", "Household Income", "Vote in national elections: Always", "Vote in national elections: Usually",
                     "Vote in national elections: Never", "Vote in national elections: Not allowed to vote",
                     "Generalized trust", "Political Interest", "Political Discussion: Frequently", "Political Discussion: Occasionally", "Political Discussion: Never", 
                     "Perceived Political Corruption")
      
      # for love.plot: pair old variables with new variable names
      names(new.names) <- c("Sex_2", "Age", "rural_2", "emplstat_1", "emplstat_2", "emplstat_3",
                            "emplstat_4", "emplstat_5", "emplstat_6", "emplstat_7", 
                            "emplstat_8", "edu_recoded", "hhinc", "votenat_1", "votenat_2", "votenat_3",
                            "votenat_4", "gentrust_2", "polint", "poldiscu_1", "poldiscu_2", "poldiscu_3", 
                            "polcorup")
      
      tikz('covbalance_cem.tex', standAlone = TRUE, width=7, height=7)
      
      cobalt::love.plot(m_cem, binary="std", var.names=new.names,
                  colors=c("#003056", "#b3b7b9")) +
        theme(legend.position = "top",
              legend.title = element_text()) +
        theme(
              axis.ticks.y = element_blank(),
              axis.line = element_line(color = "#003056"),
              rect = element_rect(color = "#003056"),
              line = element_line(color = "#003056"),
              text = element_text(color = "#003056"),
              axis.text.x = element_text(color = "#003056"),
              axis.text.y =  element_text(color = "#003056"),
              legend.title = element_blank()) +
        labs(title = "Coarsened Exact Matching") -> CEM
      # ggsave("CEM_epsa.pdf", width = 6, height = 6)  
      
      cobalt::love.plot(m_exact, binary="std", var.names=new.names,drop.distance = F,
                        colors=c("#003056", "#b3b7b9")) +
        theme(legend.position = "top",
              legend.title = element_text()) +
        theme(axis.ticks.y = element_blank(),
              axis.line = element_line(color = "#003056"),
              rect = element_rect(color = "#003056"),
              line = element_line(color = "#003056"),
              text = element_text(color = "#003056"),
              axis.ticks =  element_line(color = "#003056"),
              axis.text.x = element_text(color = "#003056"),
              axis.text.y =  element_blank(),
              legend.title = element_blank()) +
        labs(title = "Exact Matching") -> exact
      # library(patchwork)
      CEM + exact + plot_layout(guides = "collect")
      
      ggsave("matching_epsa.pdf", width = 10, height = 6)
      dev.off()
      tools::texi2dvi('covbalance_cem.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'covbalance_cem.pdf'))
     
      
      
      
      # for love.plot: rename variables
      new.names <- c("Sex", "Age", "Rural", "Employment: Full time", "Employment: Part time", "Employment: Self employed",
                     "Employment: Retired/pensioned", "Employment: Housewife", "Employment: Student", "Employment: Unemployed", 
                     "Employment: Other", "Education", "Household Income", "Vote in national elections: Usually",
                     "Vote in national elections: Never", "Vote in national elections: Not allowed to vote",
                     "Generalized trust", "Political Interest", "Political Discussion: Occasionally", "Political Discussion: Never", 
                     "Perceived Political Corruption")
      
      # for love.plot: pair old variables with new variable names
      names(new.names) <- c("Sex_2", "Age", "rural_2", "emplstat1", "emplstat2", "emplstat3",
                            "emplstat4", "emplstat5", "emplstat6", "emplstat7", 
                            "emplstat8", "edu_recoded", "hhinc", "votenat2", "votenat3",
                            "votenat4", "gentrust_2", "polint", "poldiscu2", "poldiscu3", 
                            "polcorup")
      
      
      tikz('covbalance_exact.tex', standAlone = TRUE, width=7, height=7)
        
        love.plot(m_exact, binary="std", var.names=new.names,
                    colors=c("black", "grey"))
      
      dev.off()
      tools::texi2dvi('covbalance_exact.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'covbalance_exact.pdf'))
      
      
      
      
    # ---------------------------------------------
    # 3.4 matching estimator
    # ---------------------------------------------
    
      # ATT average treatment effect on the treated
      n_algorithms <- 5
      att_data <- as.data.frame(matrix(NA, nrow=10*n_algorithms, ncol=5))
      colnames(att_data) <- c("y_var", "algorithm", "att", "se", "type")
      att_data$y_var <- c(rep("inst_armed", n_algorithms),
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
      att_data$type <- c(rep("real spillover", 15), 
                         rep("endogenous spillover", 15), 
                         rep("no effect expected", 20))
      att_data$algorithm <- rep(c("exact", "nearest", "subclass", "optimal", "cem"), 10)
      
      iter <- 0
      for (depvar in att_data$y_var) {
        iter <- iter + 1
        
        # based on exact matching
        data_exact$inst_armed <- as.factor(data_exact$inst_armed)
        # data_exact$inst_press <- as.factor(data_exact$inst_press)
        data_exact$inst_police <- as.factor(data_exact$inst_police)
        data_exact$inst_courts <- as.factor(data_exact$inst_courts)
        data_exact$inst_parl <- as.factor(data_exact$inst_parl)
        data_exact$inst_gov <- as.factor(data_exact$inst_gov)
        data_exact$inst_parties <- as.factor(data_exact$inst_parties)
        data_exact$inst_comp <- as.factor(data_exact$inst_comp)
        data_exact$inst_UN <- as.factor(data_exact$inst_UN)
        data_exact$inst_banks <- as.factor(data_exact$inst_banks)
        data_exact$inst_uni <- as.factor(data_exact$inst_uni)
        
        reg <- clm(paste(depvar, "fraud1d", sep="~"), data=data_exact, weights=weights)  
        att_data[att_data$y_var==depvar & att_data$algorithm=="exact", "att"] <- reg$beta
        att_data[att_data$y_var==depvar & att_data$algorithm=="exact", "se"] <- sqrt(reg$vcov[4,4])
        
        # based on nearest neighbor matching
        #reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_near, weights=weights)  
        #att_data[att_data$y_var==depvar & att_data$algorithm=="nearest", "att"] <- coef(reg)[2]
        #att_data[att_data$y_var==depvar & att_data$algorithm=="nearest", "se"] <- se.coef(reg)[2]
        
        # based on subclassification matching
        #reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_sub, weights=weights)  
        #att_data[att_data$y_var==depvar & att_data$algorithm=="subclass", "att"] <- coef(reg)[2]
        #att_data[att_data$y_var==depvar & att_data$algorithm=="subclass", "se"] <- se.coef(reg)[2]
        
        # based on optimal matching
        #reg <- lm(paste(depvar, "fraud1d", sep="~"), data=data_opti, weights=weights)  
        #att_data[att_data$y_var==depvar & att_data$algorithm=="optimal", "att"] <- coef(reg)[2]
        #att_data[att_data$y_var==depvar & att_data$algorithm=="optimal", "se"] <- se.coef(reg)[2]
        
        # based on exact matching
        data_cem$inst_armed <- as.factor(data_cem$inst_armed)
        # data_cem$inst_press <- as.factor(data_cem$inst_press)
        data_cem$inst_police <- as.factor(data_cem$inst_police)
        data_cem$inst_courts <- as.factor(data_cem$inst_courts)
        data_cem$inst_parl <- as.factor(data_cem$inst_parl)
        data_cem$inst_gov <- as.factor(data_cem$inst_gov)
        data_cem$inst_parties <- as.factor(data_cem$inst_parties)
        data_cem$inst_comp <- as.factor(data_cem$inst_comp)
        data_cem$inst_UN <- as.factor(data_cem$inst_UN)
        data_cem$inst_banks <- as.factor(data_cem$inst_banks)
        data_cem$inst_uni <- as.factor(data_cem$inst_uni)
        
        reg <- clm(paste(depvar, "fraud1d", sep="~"), data=data_cem, weights=weights)  
        att_data[att_data$y_var==depvar & att_data$algorithm=="cem", "att"] <- reg$beta
        att_data[att_data$y_var==depvar & att_data$algorithm=="cem", "se"] <- sqrt(reg$vcov[4,4])
        
      }
      
      
    # ---------------------------------------------
    # 3.5 plotting matching results
    # ---------------------------------------------
      # do variables that we match on have to be "pre-treatment" variables? Then we can only match on sociodemographics 
      # which preceed attitudes and not other attitudes or behaviors..
      
      # I could only show results from two algorithms with best covariate balance (e.g. exact and cem)
      
      # country-level control variables are not part yet!
      
      # definitely include the year as control!
      
      # open questions for slides
      # mean difference based on matched data or parametric model? our y-variables are not continuous
      # issues with propensity scores
      #     uncertainty not taken into account -> Bayesian approach
      #     King critique 
      
      
      
      
      
  
### ------------------------------------
# 4. Bayesian PSM
### ------------------------------------
 
  library(IUPS) # for bpsm
  
  # listwise deletion of missings
  wvs_nelda <- wvs_nelda[wvs_nelda$Wave == 7,]
  match_data <- wvs_nelda[,c("inst_armed", "fraud1", "polint", "gentrust", "Sex")]
  match_data <- match_data[complete.cases(match_data),]
  #match_data <- match_data[1:100,]
  
  # dichotomize treatment
  match_data$fraud1d <- NA
  match_data$fraud1d[match_data$fraud1==1 | match_data$fraud1==2] <- 0
  match_data$fraud1d[match_data$fraud1==3 | match_data$fraud1==4] <- 1
  
  # matrix of covariates
  Xcovs <- as.matrix(cbind(match_data$polint, 
                           match_data$gentrust, 
                           match_data$Sex))
  
  
  options(ties=F)
  res <- bpsm(Y = match_data$inst_armed,
            t = match_data$fraud1d,
            X = Xcovs,
            estimand = "ATE", 
            method = "BPSM")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




