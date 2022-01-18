#### plots for smaples 



fit1 <- stan_glm(TR_election ~ punishment*PA01 + fraud*PA01,
                           data = data_rus,
                           seed = 1201)
summary(fit1)
sjPlot::plot_model(fit1, "int")

dat <- data_rus %>% 
  dplyr::select(TR_election, PTR_pres, punishment, 
                fraud, punishment_judicial, opponent) %>% 
  drop_na()

med.fit <- lm(TR_election ~ (punishment + fraud + punishment_judicial) * opponent, data = dat)
out.fit <- lm(PTR_pres ~ TR_election + (punishment + fraud+ punishment_judicial)* opponent, data = dat)

med.out <- mediation::mediate(model.m = med.fit, 
                              model.y = out.fit, 
                              treat = "fraud", 
                              mediator = "TR_election",
                              sims = 100)


summary(med.out)

data_rus %>%
  dplyr::select(starts_with("SD"), RG02_01) %>% 
  datasummary_balance( ~ RG02_01,
            data = ., output = "latex_tabular",dinm=FALSE)


#############################


# Let's Add a Different Font for Plots!
font_add_google("Source Sans Pro")
showtext_auto()

# setting a global theme for the entire document
theme_set(ggthemes::theme_base(base_family = "Source Sans Pro") +
            theme(
              # plot.title.position = "plot",
              legend.title = element_blank(),
              legend.position = "top",
              text = element_text(color = mannheim_colors[1]),
              axis.ticks.y = element_blank()
              
            )
)


data %>% 
  dplyr::select(
    # starts_with("TR_"),
                starts_with("PTR"),
                starts_with("NTR"),
                QUESTNNR) %>% 
  pivot_longer(-QUESTNNR, names_to = "institution") %>% 
  count(QUESTNNR, institution, value) %>% 
  mutate(political = case_when(str_detect(institution, "PTR") ~ "Political Institutions",
                               str_detect(institution, "NTR") ~ "Non-political Institutions",
                               TRUE ~ "Elections"
  ),
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
                          institution == "NTR_WTO" ~ "WTO",
                          institution == "TR_election" ~ "Elections"
  ),
  value = case_when( value == 1 ~ "None at all", 
                     value == 2 ~ "Not very much", 
                     value == 3 ~ "Quite a Lot", 
                     value == 4 ~ "A Great Deal"),
  value = as_factor(value),
  value = forcats::fct_rev(value),
  political = as_factor(political),
  political = forcats::fct_rev(political),
  QUESTNNR = str_to_title(QUESTNNR)
  ) %>%
  filter(political != "Elections") %>%
  janitor::clean_names("title") %>%
  drop_na() %>%
  ggplot(aes(y = Institution , x = N, fill = (Value))) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(Political ~ Questnnr,  scales = "free",space='free') + 
  labs(y = "",
       x = "", 
       subtitle = "How much confidence would you have in the following institutions?") +
  # theme_minimal()+
  # ggthemes::theme_base() +
  theme(plot.title.position = "plot",
        legend.position = "bottom",
        legend.spacing = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-25,0,0,0),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "#003056"),
        rect = element_rect(color = "#003056"),
        line = element_line(color = "#003056"),
        text = element_text(color = mannheim_colors[1]),
        legend.title = element_blank()) +
  scale_fill_mannheim(palette = "main",
                       guide = guide_legend(reverse = TRUE)) 

ggsave(filename = "DVs.pdf", width = 9, height = 6)

  # viridis::scale_fill_viridis(discrete = T, 
  #                              option = "C", 
  #                              end = 0.8, 
  #                             guide = guide_legend(reverse = TRUE))
  


  # geom_text(size = 3, position = position_stack(vjust = 0.5))


wvs_nelda1 <- wvs_nelda %>% janitor::clean_names()
wvs_nelda_complete1 <- wvs_nelda_complete %>% janitor::clean_names()
wvs_nelda1 <- as.data.frame(lapply(wvs_nelda1, as.numeric))

ft1 <- rstanarm::stan_glm(inst_armed~fraud1d+polint+gentrust+votenat+
                            sex+age+edu_recoded+emplstat+hhinc+rural+
                            poldiscu+polcorup + 
                            # as.factor(co_w_country_code_numeric) +
                            as.factor(year_survey)
                          ,
                          data = wvs_nelda1,
                          seed = 1201, chains = 1, iter = 30000)
summary(ft1)






