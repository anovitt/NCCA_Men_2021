library(data.table)
library(tidyverse)
library(lme4)
library(Metrics)

# Load the data

regresults <- fread("R/kaggle_mania_2021_Men/data/MRegularSeasonDetailedResults.csv")
results <- fread("R/kaggle_mania_2021_Men/data/MNCAATourneyDetailedResults.csv")
sub <- fread("R/kaggle_mania_2021_Men/data/MSampleSubmissionStage2.csv")
seeds <- fread("R/kaggle_mania_2021_Men/data/MNCAATourneySeeds.csv")
masey <- fread("R/kaggle_mania_2021_Men/data/MMasseyOrdinals.csv")
kpom <- fread("R/kaggle_mania_2021_Men/data/dat_KPom.csv")
hoopMatho <- fread("R/kaggle_mania_2021_Men/data/datHoopMathOff.csv")
hoopMathd <- fread("R/kaggle_mania_2021_Men/data/datHoopMathDef.csv")
hoopMathto <- fread("R/kaggle_mania_2021_Men/data/datHoopMathTO.csv")
hoopMathtd <- fread("R/kaggle_mania_2021_Men/data/datHoopMathTD.csv")
seeds$Seed = as.numeric(substring(seeds$Seed,2,3))

# Load team names and join external data to match kaggle id numbers

names_to_TeamID <- fread("R/kaggle_mania_2021_Men/data/MTeamSpellings.csv")
teams <- fread("R/kaggle_mania_2021_Men/data/MTeams.csv")

#yearholdout <- 2019  # change this for round 2.

datKPom <-
kpom %>%
  mutate(Team = gsub('[[:digit:]]+', '', Team),
         Team = stringr::str_to_lower(Team),
         Team = str_trim(Team, "right")) %>%
  left_join(names_to_TeamID,by = c("Team" = "TeamNameSpelling")) %>%
  #mutate(Season = Year - 1) %>%
  mutate(Season = Year ) %>%
  select(-c(Team,Year)) %>%
  filter(Season > 2003)

datKPom[Season ==2021,]



datHoopMatho <-
  hoopMatho %>%
  mutate(Team = stringr::str_to_lower(Team)) %>%
  left_join(names_to_TeamID,by = c("Team" = "TeamNameSpelling")) %>%
  mutate(Season = Year - 1) %>%
  select(-c(Team,Year))

datHoopMathd <-
  hoopMathd %>%
  mutate(Team = stringr::str_to_lower(Team)) %>%
  left_join(names_to_TeamID,by = c("Team" = "TeamNameSpelling")) %>%
  mutate(Season = Year - 1) %>%
  select(-c(Team,Year))

datHoopMathto <-
  hoopMathto %>%
  mutate(Team = stringr::str_to_lower(Team)) %>%
  left_join(names_to_TeamID,by = c("Team" = "TeamNameSpelling")) %>%
  mutate(Season = Year - 1) %>%
  select(-c(Team,Year))

datHoopMathtd <-
  hoopMathtd %>%
  mutate(Team = stringr::str_to_lower(Team)) %>%
  left_join(names_to_TeamID,by = c("Team" = "TeamNameSpelling")) %>%
  mutate(Season = Year - 1) %>%
  select(-c(Team,Year))
  
# prep kpom data 

datKPom_T1 <- datKPom
datKPom_T2 <- datKPom

names(datKPom_T1) <- paste0("T1_",names(datKPom))
names(datKPom_T2) <- paste0("T2_",names(datKPom))
  
# Hoop match data prep.

datHoopMatho_T1 <- datHoopMatho
datHoopMatho_T2 <- datHoopMatho

names(datHoopMatho_T1) <- paste0("T1_",names(datHoopMatho))
names(datHoopMatho_T2) <- paste0("T2_",names(datHoopMatho))

datHoopMathd_T1 <- datHoopMathd
datHoopMathd_T2 <- datHoopMathd

names(datHoopMathd_T1) <- paste0("T1_",names(datHoopMathd))
names(datHoopMathd_T2) <- paste0("T2_",names(datHoopMathd))

datHoopMathto_T1 <- datHoopMathto
datHoopMathto_T2 <- datHoopMathto

names(datHoopMathto_T1) <- paste0("T1_",names(datHoopMathto))
names(datHoopMathto_T2) <- paste0("T2_",names(datHoopMathto))

datHoopMathtd_T1 <- datHoopMathtd
datHoopMathtd_T2 <- datHoopMathtd

names(datHoopMathtd_T1) <- paste0("T1_",names(datHoopMathtd))
names(datHoopMathtd_T2) <- paste0("T2_",names(datHoopMathtd))

# Select value ranking systems
valid_masey = 
  masey %>%
  group_by(SystemName) %>% 
  summarize(nn=min(Season),mm=max(Season), n=n(), nd=n_distinct(TeamID)) %>% 
  filter(nn==2004,mm==2021)

last_rank = masey %>% 
  filter(SystemName %in% valid_masey$SystemName, RankingDayNum <= 200) %>% 
  group_by(SystemName,TeamID) %>% 
  mutate(r=row_number(desc(RankingDayNum)))%>% 
  filter(r==1) %>% 
  select(-r,-RankingDayNum) %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank) %>%
  replace(is.na(.), 200)

last_rank_T1 = last_rank; names(last_rank_T1) = paste0('T1_',names(last_rank))
last_rank_T2 = last_rank; names(last_rank_T2) = paste0('T2_',names(last_rank))


regresults <-
regresults %>%
  filter(Season > 2003) %>%
  mutate(
    WPOSS = WFGA - WOR + WTO +(0.475 * WFTA),
    LPOSS =  LFGA - LOR + LTO +(0.475 * LFTA),
    WOFF_EFF = (WFGM + WFGM3)/(WFGA + WFGA3 + WOR + 0.4* WFTA),
    LOFF_EFF = (LFGM + LFGM3)/(LFGA + LFGA3 + LOR + 0.4* LFTA),
    WDEF_EFF = (LFGM + LFGM3)/(WFGA + WFGA3 + WOR + 0.4* WFTA),
    LDEF_EFF = (WFGM + WFGM3)/(LFGA + LFGA3 + LOR + 0.4* LFTA)
    
  )

#repeat results twice with switched team positions
regular_season = 
  rbind(
    select(regresults,
           Season,
           DayNum,
           T1=WTeamID,
           T1_Points=WScore,
           T2=LTeamID,
           T2_Points=LScore,
           Location=WLoc,
           NumOT,
           T1_fgm=WFGM,
           T1_fga=WFGA,
           T1_fgm3=WFGM3,
           T1_fga3=WFGA3,
           T1_ftm=WFTM, 
           T1_fta=WFTA,
           T1_or=WOR, 
           T1_dr=WDR, 
           T1_ast=WAst, 
           T1_to=WTO, 
           T1_stl=WStl, 
           T1_blk=WBlk, 
           T1_pf=WPF,
           
           T1_Poss = WPOSS,
           T1_Off_Eff = WOFF_EFF,
           T1_Def_Eff = WDEF_EFF,
           
           
           T2_fgm=LFGM,
           T2_fga=LFGA,
           T2_fgm3=LFGM3,
           T2_fga3=LFGA3,
           T2_ftm=LFTM, 
           T2_fta=LFTA,
           T2_or=LOR, 
           T2_dr=LDR, 
           T2_ast=LAst, 
           T2_to=LTO, 
           T2_stl=LStl, 
           T2_blk=LBlk, 
           T2_pf=LPF,
           
           T2_Poss = LPOSS,
           T2_Off_Eff = LOFF_EFF,
           T2_Def_Eff = LDEF_EFF
    ),
    select(regresults,
           Season,
           DayNum,
           T1=LTeamID,
           T1_Points=LScore,
           T2=WTeamID,
           T2_Points=WScore,
           Location=WLoc,
           NumOT,
           T1_fgm=LFGM,
           T1_fga=LFGA,
           T1_fgm3=LFGM3,
           T1_fga3=LFGA3,
           T1_ftm=LFTM, 
           T1_fta=LFTA,
           T1_or=LOR, 
           T1_dr=LDR, 
           T1_ast=LAst, 
           T1_to=LTO, 
           T1_stl=LStl, 
           T1_blk=LBlk, 
           T1_pf=LPF,
           
           T1_Poss = LPOSS,
           T1_Off_Eff = LOFF_EFF,
           T1_Def_Eff = LDEF_EFF,
           
           T2_fgm=WFGM,
           T2_fga=WFGA,
           T2_fgm3=WFGM3,
           T2_fga3=WFGA3,
           T2_ftm=WFTM, 
           T2_fta=WFTA,
           T2_or=WOR, 
           T2_dr=WDR, 
           T2_ast=WAst, 
           T2_to=WTO, 
           T2_stl=WStl, 
           T2_blk=WBlk, 
           T2_pf=WPF,
           
           T2_Poss = WPOSS,
           T2_Off_Eff = WOFF_EFF,
           T2_Def_Eff = WDEF_EFF
           
    ) %>% mutate(Location=ifelse(Location=='A','H',ifelse(Location=='H','A','N')))
  ) %>%
  mutate(Location = ifelse(Location == "H", 1 , ifelse(Location == "A",2,ifelse(Location == "N",3,4 ))))

### Collect tourney results - double the data by swapping team positions

t1 = results[, c("Season", "DayNum", "WTeamID", "LTeamID", "WScore", "LScore")] %>% mutate(ResultDiff = WScore - LScore)
t2 = results[, c("Season", "DayNum", "LTeamID", "WTeamID", "LScore", "WScore")] %>% mutate(ResultDiff = LScore - WScore)
names(t1) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
names(t2) = c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff")
tourney = rbind(t1, t2)

tourney = 
  rbind(
    select(regresults,
           Season,
           DayNum,
           T1=WTeamID,
           T1_Points=WScore,
           T2=LTeamID,
           T2_Points=LScore,
           Location=WLoc       
    ),
    select(regresults,
           Season,
           DayNum,
           T1=LTeamID,
           T1_Points=LScore,
           T2=WTeamID,
           T2_Points=WScore,
           Location=WLoc 
    ) %>% mutate(Location=ifelse(Location=='A','H',ifelse(Location=='H','A','N')))
  ) %>%
  mutate(ResultDiff = T1_Points - T2_Points) %>%
  mutate(Location = ifelse(Location == "H", 1 , ifelse(Location == "A",2,ifelse(Location == "N",3,4 ))))

### Fit GLMM on regular season data (selected march madness teams only) - extract random effects for each team

march_teams = select(seeds, Season, Team = TeamID)

march_teams

X =  regular_season %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T1" = "Team")) %>% 
  inner_join(march_teams, by = c("Season" = "Season", "T2" = "Team")) %>% 
  select(Season, T1, T2, T1_Points, T2_Points, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)


quality = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~  (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  quality[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
quality = do.call(rbind, quality)

quality_T1 <- quality
quality_T2 <- quality

names(quality_T1) <- paste0("T1_",names(quality_T1))
names(quality_T2) <- paste0("T2_",names(quality_T2))

quality_T1 %>%
  arrange(desc(T1_quality)) %>%
  mutate(T1_Team_Id = as.integer(T1_Team_Id)) %>%
  left_join(select(teams,TeamID,TeamName),by=c("T1_Team_Id" = "TeamID"))

### Fit GLMM on regular season data (all teams all games) - extract random effects for each team

X =  regular_season %>% 
select(Season, T1, T2, T1_Points, T2_Points, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

qualityAll = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~  (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  qualityAll[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}
qualityAll = do.call(rbind, qualityAll)

qualityAll_T1 <- qualityAll
qualityAll_T2 <- qualityAll

names(qualityAll_T1) <- paste0("T1_",names(qualityAll_T1))
names(qualityAll_T2) <- paste0("T2_",names(qualityAll_T2))

qualityAll_T1 %>%
  arrange(desc(T1_quality)) %>%
  mutate(T1_Team_Id = as.integer(T1_Team_Id)) %>%
  left_join(select(teams,TeamID,TeamName),by=c("T1_Team_Id" = "TeamID"))

# Off Efficiency Quality

X =  regular_season %>% 
  select(Season, T1, T2, T1_Points, T2_Points, T1_Off_Eff, T2_Off_Eff, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

OffEffqualityAll = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~ T1_Off_Eff + T2_Off_Eff + (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  OffEffqualityAll[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}

OffEffqualityAll = do.call(rbind, OffEffqualityAll)

OffEffqualityAll_T1 <- OffEffqualityAll
OffEffqualityAll_T2 <- OffEffqualityAll

names(OffEffqualityAll_T1) <- paste0("T1_",names(OffEffqualityAll_T1))
names(OffEffqualityAll_T2) <- paste0("T2_",names(OffEffqualityAll_T2))

OffEffqualityAll_T1 %>%
  arrange(desc(T1_quality)) %>%
  mutate(T1_Team_Id = as.integer(T1_Team_Id)) %>%
  left_join(select(teams,TeamID,TeamName),by=c("T1_Team_Id" = "TeamID"))



# Def Efficiency Quality


X =  regular_season %>% 
  select(Season, T1, T2, T1_Points, T2_Points, T1_Def_Eff, T2_Def_Eff, NumOT) %>% distinct()
X$T1 = as.factor(X$T1)
X$T2 = as.factor(X$T2)

DefEffqualityAll = list()
for (season in unique(X$Season)) {
  glmm = glmer(I(T1_Points > T2_Points) ~ T1_Def_Eff + T2_Def_Eff + (1 | T1) + (1 | T2), data = X[X$Season == season & X$NumOT == 0, ], family = binomial) 
  random_effects = ranef(glmm)$T1
  DefEffqualityAll[[season]] = data.frame(Season = season, Team_Id = as.numeric(row.names(random_effects)), quality = exp(random_effects[,"(Intercept)"]))
}

DefEffqualityAll = do.call(rbind, DefEffqualityAll)

DefEffqualityAll_T1 <- DefEffqualityAll
DefEffqualityAll_T2 <- DefEffqualityAll

names(DefEffqualityAll_T1) <- paste0("T1_",names(DefEffqualityAll_T1))
names(DefEffqualityAll_T2) <- paste0("T2_",names(DefEffqualityAll_T2))

DefEffqualityAll_T1 %>%
  arrange(desc(T1_quality)) %>%
  mutate(T1_Team_Id = as.integer(T1_Team_Id)) %>%
  left_join(select(teams,TeamID,TeamName),by=c("T1_Team_Id" = "TeamID"))

### Regular season statistics

season_summary = 
  regular_season %>%
  mutate(win14days = ifelse(DayNum > 118 & T1_Points > T2_Points, 1, 0),
         last14days = ifelse(DayNum > 118, 1, 0)) %>% 
  group_by(Season, T1) %>%
  summarize(
    WinRatio14d = sum(win14days) / sum(last14days),
    Points_mean=mean(T1_Points),
    Points_sd=sd(T1_Points),
    Points_median=median(T1_Points),
    fgm_mean=mean(T1_fgm),     
    fga_mean=mean(T1_fga), 
    fgp=sum(T1_fgm)/sum(T1_fga),
    fgm3_mean=mean(T1_fgm3), 
    fga3_mean=mean(T1_fga3),
    fg3p=sum(T1_fgm3)/sum(T1_fga3),    
    ftm_mean=mean(T1_ftm), 
    fta_mean=mean(T1_fta),
    ftp=sum(T1_ftm)/sum(T1_fta),        
    or_mean=mean(T1_or), 
    dr_mean=mean(T1_dr), 
    orp=sum(T1_or)/sum(T1_or+T1_dr),          
    ast_mean=mean(T1_ast), 
    to_mean=mean(T1_to),
    astto=sum(T1_ast)/sum(T1_to),        
    stl_mean=mean(T1_stl), 
    blk_mean=mean(T1_blk), 
    pf_mean=mean(T1_pf), 
    fgm3fgm2=sum(T1_fgm3)/sum(T1_fgm),
    off_eff=sum(T1_fgm + T1_fgm3)/sum(T1_fga+T1_fga3+T1_or+0.4*T1_fta),
    def_eff=sum(T2_fgm + T2_fgm3)/sum(T1_fga+T1_fga3+T1_or+0.4*T1_fta),
    poss = mean(T1_Poss),
    off_eff_mean = mean(T1_Off_Eff),
    def_eff_mean = mean(T1_Def_Eff),
    
    fgm_sd=sd(T1_fgm),     
    fga_sd=sd(T1_fga), 
    fgm3_sd=sd(T1_fgm3), 
    fga3_sd=sd(T1_fga3),
    ftm_sd=sd(T1_ftm), 
    fta_sd=sd(T1_fta),
    or_sd=sd(T1_or), 
    dr_sd=sd(T1_dr), 
    ast_sd=sd(T1_ast), 
    to_sd=sd(T1_to),
    stl_sd=sd(T1_stl), 
    blk_sd=sd(T1_blk), 
    pf_sd=sd(T1_pf),
    poss_sd = sd(T1_Poss),
    off_eff_sd = sd(T1_Off_Eff),
    def_eff_sd = sd(T1_Def_Eff),
    
    fgm_median=median(T1_fgm),     
    fga_median=median(T1_fga), 
    fgm3_median=median(T1_fgm3), 
    fga3_median=median(T1_fga3),
    ftm_median=median(T1_ftm), 
    fta_median=median(T1_fta),
    or_median=median(T1_or), 
    dr_median=median(T1_dr), 
    ast_median=median(T1_ast), 
    to_median=median(T1_to),
    stl_median=median(T1_stl), 
    blk_median=median(T1_blk), 
    pf_median=median(T1_pf),
    poss_median = median(T1_Poss),
    off_eff_median = median(T1_Off_Eff),
    def_eff_median = median(T1_Def_Eff),
    
    OppFgmMean = mean(T2_fgm),
    OppFgaMean = mean(T2_fga),
    OppFgm3Mean = mean(T2_fgm3),
    OppFga3Mean = mean(T2_fga3),
    OppFgaMin = min(T2_fga)  
  )

season_summary_T1 = season_summary
season_summary_T2 = season_summary
names(season_summary_T1) = c("Season", "T1", paste0("T1_",names(season_summary_T1)[-c(1,2)]))
names(season_summary_T2) = c("Season", "T2", paste0("T2_",names(season_summary_T2)[-c(1,2)]))

### Combine all features into a data frame

data_matrix =
  tourney %>% 
  #filter(Season == 2012 ) %>%
  filter(Season >= 2003 & Season <= 2019) %>%
  left_join(season_summary_T1, by = c("Season", "T1")) %>% 
  left_join(season_summary_T2, by = c("Season", "T2")) %>%
  
  left_join(select(seeds, Season, T1 = TeamID, T1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, T2_Seed = Seed), by = c("Season", "T2")) %>% 
  
  left_join(qualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(qualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  left_join(OffEffqualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(OffEffqualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  left_join(DefEffqualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(DefEffqualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  #left_join(quality_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  #left_join(quality_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%

  left_join(datKPom_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  left_join(datKPom_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMatho_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMatho_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathd_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathd_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathto_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathto_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathtd_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathtd_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(last_rank_T1,by=c("Season"="T1_Season",'T1'='T1_TeamID')) %>%
  #left_join(last_rank_T2,by=c("Season"="T2_Season",'T2'='T2_TeamID')) %>%
  
  #mutate(SeedDiff = T1_Seed - T2_Seed) %>%
  mutate(SeedDiff = T1_Seed - T2_Seed,
         SeedDiff = ifelse(abs(SeedDiff) < 6,0,SeedDiff)) %>%
  
  replace(is.na(.), 0)

data_matrix <- as.data.table(data_matrix)


#top25DT <- 
#data_matrix %>%
#  filter( T1_AP <= 25 | T2_AP <= 25) %>%
#  select(Season,T1,T1_Points,T2,T2_Points,T1_AP,T2_AP) %>%
#  mutate(T1_25Wins= ifelse(T1_Points > T2_Points, 1, 0),
#          T2_25Wins = ifelse(T2_Points > T1_Points,1 , 0)) %>%
#  group_by(Season, T1) %>%
#  summarise(Wins25 = sum(T1_25Wins)) %>%
#  as.data.table()

#top25DT_T1 <- top25DT
#top25DT_T2 <- top25DT

#names(top25DT_T1) <- paste0("T1_",names(top25DT_T1))
#names(top25DT_T2) <- paste0("T2_",names(top25DT_T2))


#data_matrix <-
#data_matrix %>%
#  #left_join(top25DT, by = c("Season" = "Season", "T1" = "T1")) %>%
#  #left_join(top25DT, by = c("Season" = "Season", "T2" = "T1")) 
#  left_join(top25DT_T1, by = c("Season" = "T1_Season","T1" = "T1_T1")) %>%
#  left_join(top25DT_T2, by = c("Season" = "T2_Season","T2" = "T2_T1")) 



#data_matrix <-
##  data_matrix %>% 
#  filter(Season < yearholdout)

#data_matrix <-
#data_matrix %>%
#  select(setdiff(names(data_matrix),c('T1_AP','T2_AP'))) %>%
#  mutate(T1 = as.integer(T1),
#         T2 = as.integer(T2))

write.csv(data_matrix,file='data_matrix.csv',row.names = FALSE)

#sub$Season = 2020 set to 2021 at stage 2

sub$Season = as.numeric(substring(sub$ID,1,4))
sub$T1 = as.numeric(substring(sub$ID,6,9))
sub$T2 = as.numeric(substring(sub$ID,11,14))


Z <-
  sub %>% 
  left_join(season_summary_T1, by = c("Season", "T1")) %>% 
  left_join(season_summary_T2, by = c("Season", "T2")) %>%
  
  left_join(select(seeds, Season, T1 = TeamID, T1_Seed = Seed), by = c("Season", "T1")) %>% 
  left_join(select(seeds, Season, T2 = TeamID, T2_Seed = Seed), by = c("Season", "T2")) %>% 
  
  left_join(qualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(qualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  left_join(OffEffqualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(OffEffqualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  left_join(DefEffqualityAll_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  left_join(DefEffqualityAll_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  #left_join(quality_T1, by = c("Season" = "T1_Season", "T1"="T1_Team_Id")) %>%
  #left_join(quality_T2, by = c("Season" = "T2_Season", "T2"="T2_Team_Id")) %>%
  
  left_join(datKPom_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  left_join(datKPom_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMatho_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMatho_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathd_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathd_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathto_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathto_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(datHoopMathtd_T1, by = c("Season" = "T1_Season","T1" = "T1_TeamID")) %>%
  #left_join(datHoopMathtd_T2, by = c("Season" = "T2_Season","T2" = "T2_TeamID")) %>%
  
  #left_join(last_rank_T1,by=c("Season"="T1_Season",'T1'='T1_TeamID')) %>%
  #left_join(last_rank_T2,by=c("Season"="T2_Season",'T2'='T2_TeamID')) %>%
  
  #left_join(top25DT_T1, by = c("Season" = "T1_Season","T1" = "T1_T1")) %>%
  #left_join(top25DT_T2, by = c("Season" = "T2_Season","T2" = "T2_T1")) %>%
  
  #mutate(SeedDiff = T1_Seed - T2_Seed) %>%
  mutate(SeedDiff = T1_Seed - T2_Seed,
         SeedDiff = ifelse(abs(SeedDiff) < 6,0,SeedDiff)) %>%
  
  replace(is.na(.), 0)

 

write.csv(Z , file = 'sub_matrix.csv',row.names = FALSE)

Z <- Z 
mutate(T1 = as.integer(T1),
       T2 = as.integer(T2),
       Season = as.integer(Season)) #%>%
as.data.table()


# normalize the datat prior to moving to python

#features = setdiff(names(data_matrix), c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff", "Location"))
features = setdiff(names(data_matrix), c("Season", "DayNum", "T1", "T2", "T1_Points", "T2_Points", "ResultDiff", "Location","T1_Seed","T2_Seed"))

features_names_bind <- c("Season",  "T1", "T2", "T1_Points", "T2_Points", "ResultDiff", "Location")
features_names_bind_sub <- c("ID","Pred","Season",  "T1", "T2")
#features = subset(features,!grepl("SOS",features))  Decreased performance

dat <- Z[, ..features]
preproc1 <- caret::preProcess(dat[, ..features], method=c("center", "scale"))

Z_norm <- predict(preproc1, dat[,..features])

Z_norm <- cbind(Z[, ..features_names_bind_sub],Z_norm)

dat <- data_matrix[, ..features]
preproc1 <- caret::preProcess(dat[, ..features], method=c("center", "scale"))

data_matrix_norm <- predict(preproc1, dat[,..features])

data_matrix_norm <- cbind(data_matrix[, ..features_names_bind], data_matrix_norm)

write.csv(Z_norm, file = 'C:/Users/tnovi/Documents/R/kaggle_mania_2021_Men/sub_matrix_nn.csv',row.names = FALSE)
write.csv(data_matrix_norm, file = 'C:/Users/tnovi/Documents/R/kaggle_mania_2021_Men/data_matrix_nn.csv',row.names = FALSE)

# run NCAA_Mens_xboost_2020.R
# run Python nn code
