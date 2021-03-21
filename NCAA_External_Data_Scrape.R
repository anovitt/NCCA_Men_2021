library(tidyverse)
library(data.table)
library(RSelenium)
library(rvest)
#library(broom)

#######
# pull ken pom data
#######

# functions
# is_all_numeric checks if column are numeric vs alpha for converting

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# column names k pom data
datNames <- c("Rnk","Team","Conf","W_L","AdjEM","Adjo","Adjo_Rnk","AdjD","AdjD_Rnk","AdjT","AdjT_Rnk","Luck","Luck_Rnk",
              "SOS_AdjEM","SOS_AdjEM_Rnk","SOS_OppO","SOS_OppO_Rnk","SOS_OppD","SOS_OppD_Rnk","NCSOS_AdjEM","NCSOS_AdjEM_Rnk")

years <- c(2003:2021)

datKPom <- data.table()

for(j in years){
  
  print(j)
  
  url <- paste0("https://kenpom.com/index.php?y=",j,".htm")
  kPom_Tempo<- xml2::read_html(url) %>%
    html_nodes(css = 'table') %>%
    html_table(fill = TRUE)
  
  dat_temp <- as.data.frame(kPom_Tempo[[1]])
  dat_temp <- as.data.table(dat_temp)
  
  # Set the column names
  
  names(dat_temp) <- datNames
  
  # Clean up the data and bind together by year
  
  datNamesDrop <- !grepl("Rnk", names(dat_temp))

  dat_temp <-
    dat_temp %>%
    filter(!grepl("Strength", SOS_AdjEM),
           !grepl("Rk", Rnk)) %>%
    select(-ends_with("Rnk")) %>%
    select(-c("Conf","W_L")) %>%
    mutate_if(is_all_numeric,as.numeric) %>%
    mutate(Year = j)
  
  datKPom <- rbind(datKPom,dat_temp)
  
}

write.csv(datKPom, file = "R/kaggle_mania_2021_Men/data/dat_KPom.csv", row.names = FALSE)


#####
#  Hoop math
#####

# functions
# is_all_numeric checks if column are numeric vs alpha for converting

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# offense
rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

years <- c(2012:2021)

datHoopMathOff <- data.table()

for(j in years){
  
  print(j)
  
  url <- paste0("http://hoop-math.com/leader_o",j,".php")
  
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl"
    html_table(fill=T) # rvest convert dataframe
  
  datTemp <- as.data.table(signals[[1]])
  
  names(datTemp) <- c("Team", "eFG_o", "per_shots_at_rim_o", "FG_per_at rim_o", "per_assisted_at_rim_o",     
                      "per_shot_2pt_J_o", "FG_per_2pt_J_o","per_assisted_2pt_J_o", "per_shots_3pt_o", "3FG_per_o",                 
                      "per_assisted_3pt_o", "per_shots_transition_o", "Transition_eFG_per_o","Non_transtion_eFG_per_o"   )
  
  datTemp <-
  datTemp %>%
    mutate(Transition_eFG_per_o = stringr::str_sub(Transition_eFG_per_o,1,4),
           Non_transtion_eFG_per_o = stringr::str_sub(Non_transtion_eFG_per_o,1,4)) %>%
    mutate_if(is_all_numeric,as.numeric) %>%
    mutate(Year = j)
  
  #print(datTemp)
  
  datHoopMathOff <- rbind(datHoopMathOff,datTemp)
  
}

write.csv(datHoopMathOff, file = "R/kaggle_mania_2021_Men/data/datHoopMathOff.csv", row.names = FALSE)


datHoopMathDef <- data.table()

for(j in years){
  
  print(j)
  
  url <- paste0("http://hoop-math.com/leader_d",j,".php")
  
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl"
    html_table(fill=T) # rvest convert dataframe
  
  datTemp <- as.data.table(signals[[1]])
  
  names(datTemp) <- c("Team", "eFG_d", "per_shots_at_rim_d", "FG_per_at rim_d", "per_blocked_at_rim_d",     
                      "per_shot_2pt_J_d", "FG_per_2pt_J_d","per_blocked_2pt_J_d", "per_shots_3pt_d", "3FG_per_d",                 
                      "per_blocked_3pt_d", "per_shots_transition_d", "Transition_eFG_per_d","Non_transtion_eFG_per_d"   )
  
  datTemp <-
    datTemp %>%
    mutate(Transition_eFG_per_d = stringr::str_sub(Transition_eFG_per_d,1,4),
           Non_transtion_eFG_per_d = stringr::str_sub(Non_transtion_eFG_per_d,1,4)) %>%
    mutate_if(is_all_numeric,as.numeric) %>%
    mutate(Year = j)
  
  #print(datTemp)
  
  datHoopMathDef <- rbind(datHoopMathDef,datTemp)
  
}

write.csv(datHoopMathDef, file = "R/kaggle_mania_2021_Men/data/datHoopMathDef.csv", row.names = FALSE)

# Transition Off

datHoopMathTO <- data.table()

for(j in years){
  
  print(j)
  
  url <- paste0("http://hoop-math.com/leader_to",j,".php")
  
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl"
    html_table(fill=T) # rvest convert dataframe
  
  datTemp <- as.data.table(signals[[1]])
  
  names(datTemp) <- c("Team", 
                      "per_FGA_to", "per_tot_FGA_to",
                      "eFG_to", "eFG_non_to", 
                      "eFG_rebound_0_10_to","per_init_FGA_rebound_0_10_to",
                      "eFG_rebound_11_30_to","per_init_FGA_rebound_11_30_to",
                      
                      "eFG_opp_score_0_10_to","per_init_FGA_opp_score_0_10_to",
                      "eFG_opp_score_11_30_to","per_init_FGA_opp_score_11_30_to",
                      
                      "eFG_steal_0_10_to","per_init_FGA_steal_0_10_to",
                      "eFG_steal_11_30_to","per_init_FGA_steal_11_30_to")
                      
           
  datTemp <-
    datTemp %>%
    mutate_all(funs(gsub("%","",.))) %>%
    mutate_if(is_all_numeric,as.numeric) %>%
    mutate(Year = j)

  
  datHoopMathTO <- rbind(datHoopMathTO,datTemp)
  
}

write.csv(datHoopMathTO, file = "R/kaggle_mania_2021_Men/data/datHoopMathTO.csv", row.names = FALSE)


# Transition Def

datHoopMathTD <- data.table()

for(j in years){
  
  print(j)
  
  url <- paste0("http://hoop-math.com/leader_td",j,".php")
  
  remDr$navigate(url)
  Sys.sleep(5) # give the page time to fully load
  
  html <- remDr$getPageSource()[[1]]
  
  signals <- read_html(html) %>% # parse HTML
    html_nodes("table") %>% # extract table nodes with class = "tbl"
    html_table(fill=T) # rvest convert dataframe
  
  datTemp <- as.data.table(signals[[1]])
  
  names(datTemp) <- c("Team", 
                      "per_FGA_init_td", "per_tot_FGA_td",
                      "eFG_td", "eFG_non_td", 
                      "eFG_rebound_init_0_10_td","eFG_rebound_0_10_td",
                      "per_init_FGA_rebound_11_30_td","eFG_rebound_11_30_td",
                      
                      "per_init_FGA_opp_score_0_10_td","eFG_opp_score_0_10_td",
                      "per_init_FGA_opp_score_11_30_td","eFG_opp_score_11_30_td",
                      
                      "per_init_FGA_steal_0_10_td","eFG_steal_0_10_td",
                      "per_init_FGA_steal_11_30_td","eFG_steal_11_30_td")
  
  datTemp <-
    datTemp %>%
    mutate_all(funs(gsub("%","",.))) %>%
    mutate_if(is_all_numeric,as.numeric) %>%
    mutate(Year = j)
  
  
  datHoopMathTD <- rbind(datHoopMathTD,datTemp)
  
}

write.csv(datHoopMathTD, file = "R/kaggle_mania_2021_Men/data/datHoopMathTD.csv", row.names = FALSE)

