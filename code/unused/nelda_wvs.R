



data <- readRDS("WVS_TimeSeries_R_v1_2")


unique(data$COUNTRY_ALPHA)
unique(NELDA$stateid)

unique(data$COW_NUM)
unique(NELDA$ccode)

sum(unique(data$COW_ALPHA) %in% unique(NELDA$stateid))

library(lubridate)

library(stringr)

library(dplyr)
NELDA$date <- as_date(paste0(NELDA$year, 
                             "-", 
                             str_pad(str_remove(NELDA$mmdd, '.{2}$'), width=2, side="left", pad="0"), 
                             "-", 
                             str_extract(NELDA$mmdd, '.{2}$')))


data$date <- ymd(as.numeric(data$S012))

WVS_countries <- data %>% select(COW_ALPHA, COW_NUM, COUNTRY_ALPHA, date) %>% distinct()
# NELDA_countries <- NELDA %>% select(stateid, ccode, country) %>% distinct()

# tt <- dplyr::full_join(WVS_countries, NELDA_countries, by = c('COW_NUM' = 'ccode'))

WVS_NELDA <- tibble()
# WVS_NELDA_parl <- tibble()
# WVS_NELDA_exec <- tibble()

for (i in 1:nrow(WVS_countries)){
  try({NELDA %>% 
  filter(ccode == WVS_countries$COW_NUM[i], date <= WVS_countries$date[i]) %>% 
  filter(date == max(date)) %>% 
  mutate(days_after_elections_all = WVS_countries$date[i] - date) %>% 
  bind_cols(., WVS_countries[i,])  -> temp
  
  WVS_NELDA %<>% bind_rows(temp, .)})

  # try({NELDA %>% 
  # filter(stateid ==  WVS_countries$COW_ALPHA[i], date <= WVS_countries$date[i]) %>% 
  # filter(types == "Legislative/Parliamentary", date == max(date)) %>% 
  # mutate(days_after_elections_parl = WVS_countries$date[i] - date)%>% 
  # bind_cols(., WVS_countries[i,]) %>% 
  # mutate(last_election = "parl") -> temp_parl
  #   WVS_NELDA_parl %<>% bind_rows(temp_parl, .)})

  # NELDA %>% 
  # filter(stateid ==  WVS_countries$COW_ALPHA[i], date <= WVS_countries$date[i]) %>% 
  # filter(types == "Executive", date == max(date)) %>% 
  # mutate(days_after_elections_exec = WVS_countries$date[i] - date)%>% 
  # bind_cols(., WVS_countries[i,]) %>% 
  # mutate(last_election = "exec")-> temp_exec
  # WVS_NELDA_exec %<>% bind_rows(temp_exec, .)
}


data %>% left_join(., WVS_NELDA) -> tt

