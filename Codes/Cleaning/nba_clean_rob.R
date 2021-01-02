#######################
##  Analysis of      ##
##  Connection       ##
##  Between          ##
##  NBA Salaries     ##
##                   ##
##      and          ## 
##                   ## 
##  Performance      ##
##  Metrics          ##
##      NO. 1        ##
##                   ##
##Cleaning the data  ##
##                   ##
#######################


# Clear memory
rm(list=ls())

library(tidyverse)


# Call the player data from github
player_data <- "/Users/steve_j/Documents/CEU /data_analysis/DA_2/DA2_Term/players (1) copy.csv"
df_player <- read_csv( player_data )



# Drop all rows containing missing values (N/A)
df_player1 <- df_player %>% drop_na()

# Drop all columns redundant for analysis 
df_player1 <- df_player1 %>% select(-c(birthDate, birthPlace, college, draft_pick, draft_team, height, highSchool, position, shoots, weight ))

# Rename the id column 
df_player1 <-df_player1 %>%  rename(player_id = id)


# Call the salary data from github
salary_data <- "/Users/steve_j/Documents/CEU /data_analysis/DA_2/DA2_Term/salaries_1985to2018 (1).csv"
df_salary <- read_csv( salary_data )

# Taking a sample of 15 years 
df_salary <- df_salary %>% filter( season_end < 2003 )
df_salary <- df_salary %>% filter( season_start >= 1988 )

# The average NBA career is about 4.5 years. Let's drop all players who played less then 5 seasons in the NBA in order to establish a common starting base
# Checking how many observations are to be dropped 
df_salary[df_salary$player_id %in% names(which(table(df_salary$player_id) <= 5)), ]

# Dropping observations 
df_salary <- df_salary[df_salary$player_id %in% names(which(table(df_salary$player_id) >= 5)), ]


# Drop all columns redundant for analysis
df_salary <- df_salary %>% select(-c(league, season, season_end, season_start, team))

# Calculating average salary for players' career 
df_salary1 <- df_salary %>% group_by(player_id) %>% summarise(avg_salary = mean(salary))

# Merging the two tables 
df_full <- full_join(df_player1, df_salary1)

# Dropping all entries that have no records of salaries 
df_full <- df_full %>% drop_na()

# Dropping all entries that have no records of 3pt shooting 
df_full <- df_full[!grepl("-", df_full$`career_FG3%`),]

# Cleaning the draft round values & converting to numeric 
df_full <-df_full %>% 
  mutate( draft_round = substr(df_full$draft_round, start = 1, stop = 1)) %>% 
  transform(draft_round = as.numeric(draft_round))
str(df_full$draft_round)  

# Converting the free throw percentage column to numeric 
df_full <-df_full %>% transform(career_FT. = as.numeric(career_FT.))
str(df_full$career_FT.)

# Dropping the N/As that resulted from the conversion 
df_full <- df_full %>% drop_na()

# Filtering out players who played less then a 100 games for the whole career, to take care of possible influential observations (e.g. 100% FG )
df_full <-df_full %>% filter( career_G > 100 )

# Making the table more transparent and keeping only the relevant variables 
df_full <- df_full %>% transmute ( name = df_full$name,
                                   avg_salary = df_full$avg_salary / 1000000,
                                   eFG = career_eFG.,
                                   PER = career_PER,
                                   tot_games = career_G,
                                   draft_round = df_full$draft_round)


# Until 1989 there were  7 rounds in the NBA draft. After 1989, there are only 2 rounds. In addition, most players with significant salaries are drafted in the first or second round, thus let's filter out players who were drafted after the 2nd round 

# Checking how many observations are to be dropped 
df_round_filter <- df_full %>% filter( draft_round > 2 )

# Filtering out the players who were drafted after the 2nd round 
df_full <- df_full %>% filter( draft_round < 3 )

# Saving the clean data 
write.csv(df_full, "/Users/steve_j/Documents/CEU /data_analysis/DA_2/DA2_Term/Data//nba_clean_rob.csv", row.names = FALSE)

