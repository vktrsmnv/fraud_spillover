# Perceptions of Election Fraud, Judicial Rulings and Diffuse Political Support
# Lion Behrens and Viktoriia Semenova 
# Reproduction File 

# 1. Setup #########################################################

setup <- function() {
  p_needed <- c(
    "tidyverse",
    "rstudioapi",
    "janitor",
    "magrittr",
    "sjlabelled",
    "conflicted",
    "here"
  )
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  lapply(p_needed, require, character.only = TRUE)

  conflict_prefer("select", "dplyr")

  setwd(dirname(getActiveDocumentContext()$path)) # set directory to the document
}
setup()


# 2. Data Cleaning #########################################################

##### Observational Data: WVS Wave 7
## Data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

wvs7 <- read_rds(
  here("data/World_Values_Survey_Wave_7_Inverted_R_v1_6_2.rds")
) %>%
  label_to_colnames() %>%
  clean_names()

wvs <- wvs7 %>%
  select(
    year = year_of_survey,
    cow_alpha = co_w_country_code_alpha,
    cow_numeric = co_w_country_code_numeric,
    gentrust = most_people_can_be_trusted,
    polint = interest_in_politics,
    votenat = vote_in_elections_national_level,
    age,
    sex,
    edu_three = highest_educational_level_respondent_recoded_into_3_groups,
    edu_nine = highest_educational_level_respondent_a_s_father_isced_2011,
    emplstat = employment_status,
    sector = sector_of_employment,
    hhinc = scale_of_incomes, 
    savings = family_savings_during_past_year,
    rural = urban_rural,
    poldiscu = how_often_discusses_political_matters_with_friends,
    polcorup = perceptions_of_corruption_in_the_country,
    occgroup = respondent_occupational_group,
    inst_armed = confidence_armed_forces,
    inst_police = confidence_the_police,
    inst_courts = confidence_justice_system_courts,
    inst_parl = confidence_parliament,
    inst_gov = confidence_the_government,
    inst_parties = confidence_the_political_parties,
    inst_comp = confidence_major_companies,
    inst_UN = confidence_the_united_nations,
    inst_banks = confidence_banks,
    inst_wto = confidence_the_world_trade_organization_wto,
    inst_wb = confidence_the_world_bank,
    inst_uni = confidence_universities,
    inst_election = confidence_elections,
    fraud1 = how_often_in_country_a_s_elections_votes_are_counted_fairly,
    vdem_elections = clean_elections_index_0_to_1_index_v_dem_2019,
    pei_elections = index_of_perceptions_of_electoral_integrity_0_100_imputed_electoral_integri
  ) %>%
  mutate(
    fraud1d = case_when(
      fraud1 %in% 1:2 ~ 0, # Not at all often & Not often
      fraud1 %in% 3:4 ~ 1 # Fairly often & Very often
    ),
    fraud1d1 = case_when(
      fraud1 %in% 1 ~ 0, # Not at all often
      fraud1 %in% 2:4 ~ 1 # Not often & Fairly often & Very often
    ),
    fraud1d2 = case_when(
      fraud1 %in% 1:3 ~ 0, # Not at all often & Not often & Fairly often
      fraud1 %in% 4 ~ 1 #  Very often
    )
  )

# match_data <- wvs %>%
#   # drop_na() %>%
#   select(-c(cow_alpha, fraud1d1, fraud1d2)) %>%
#   mutate(across(everything(), ~ as.numeric(.x)))

# write_rds(match_data, here("data/match_data.rds"))
write_rds(wvs, here("data/wvs7.rds"))

