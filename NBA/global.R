library(nbastatR)
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(jsonlite)
library(lubridate)
library(extrafont)
library(ggrepel)
library(ggimage)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(anytime)
library(runner)
library(fs)
library(googlesheets4)
library(googledrive)
library(scales)
library(jsonlite)
library(httr)
library(stringi)

Sys.setenv (TZ="America/Los_Angeles")
# designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# gs4_auth(path = '.secrets', email = 'jyablonski9@gmail.com')
# drive_auth(cache = ".secrets", email = "jyablonski9@gmail.com")
gs4_auth_configure(api_key = "AIzaSyAzox2eJqkur-VSuSxwIRbKCs_-m-ky4d8")
gs4_deauth()

today <- Sys.Date()
todayDate <- Sys.Date()
yesterday <- Sys.Date()-1
isSeasonActive <- TRUE
today <-  format(today, format = "%B %d, %Y")


# custom theme
theme_jacob <- function () { 
  theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "floralwhite"),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

get_gamelogs_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/gameLogs.csv')$change_time, units = 'hours') > 8.0){
    gameLogs <- range_speedread(
      ss = 'https://docs.google.com/spreadsheets/d/1MwPs5VAfAukhCyIoPxxZs5weQbj1RGTvcsaAyzFp0Lo/edit#gid=1232535158',
      sheet = 1)
    write_csv(gameLogs, 'data/gameLogs.csv')
    return(gameLogs)
  }
  else {
    df <- read_csv('data/gameLogs.csv')
    return(df)
  }
}

get_injuries_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/injuryData.csv')$change_time, units = 'hours') > 8.0){
    injuries <- range_speedread(
      ss = 'https://docs.google.com/spreadsheets/d/1MwPs5VAfAukhCyIoPxxZs5weQbj1RGTvcsaAyzFp0Lo/edit#gid=1232535158',
      sheet = 2)
    write_csv(injuries, 'data/injuryData.csv')
    return(injuries)
  }
  else {
    df <- read_csv('data/injuryData.csv')
    return(df)
  }
}

get_playbyplay_data <- function(){
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/playbyplay_df.csv')$change_time, units = 'hours') > 8.0){
    playbyplay_df <- range_speedread(
      ss = 'https://docs.google.com/spreadsheets/d/1MwPs5VAfAukhCyIoPxxZs5weQbj1RGTvcsaAyzFp0Lo/edit#gid=1232535158',
      sheet = 5)
    write_csv(playbyplay_df, 'data/playbyplay_df.csv')
    return(playbyplay_df)
  }
  else {
    df <- read_csv('data/playbyplay_df.csv')
    return(df)
  }
}

getSchedule <- function(){
  currentmonth <- tolower(format(Sys.Date(), "%B"))
  nextmonth <- tolower(format(Sys.Date() + 30, "%B"))
  year <- 2021
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", currentmonth, ".html")
  webpage <- read_html(url)
  col_names <- webpage %>% 
    html_nodes("table#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  col_names <- c("game_id", col_names)
  dates <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>% 
    html_text()
  dates <- as.data.frame(dates) %>%
    filter(dates != 'Playoffs')
  # dates <- dates[dates != "Playoffs"]
  game_id <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>%
    html_attr("csk")
  game_id <- game_id[!is.na(game_id)]
  data <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(col_names) - 2, byrow = TRUE)
  month_df1 <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
  names(month_df1) <- col_names
  return(month_df1)
}

get_transactions <- function(){
  url <- paste0('https://www.basketball-reference.com/leagues/NBA_2021_transactions.html')
  webpage <- read_html(url)
  
  new1 <- html_nodes(webpage, "div#content > ul > li") %>% 
    map_df(~{
      data_frame(
        Date = html_node(.x, "span") %>% html_text(trim = TRUE),
        Event = html_nodes(.x, "p") %>% html_text(trim = TRUE)
      )
    }) 
  new2 <- new1 %>%
    mutate(Date2 = as.Date(Date, tryFormats = '%B %d, %Y')) %>%
    filter(!str_detect(Event, 'G-League'),
           Date2 >= '2020-04-01') %>%
    select(Date, Event)
  return(new2)
}

getContracts <- function(){
  url <- 'https://www.basketball-reference.com/contracts/players.html'
  webpage <- read_html(url)
  col_names <- webpage %>%
    html_table() %>%
    as.data.frame() %>%
    filter(Var.2 != 'Player',
           Salary != 'Salary') %>%
    select(Var.2, Var.3, Salary, Var.11) %>%
    rename(Player = Var.2, Team = Var.3, Salary = Salary, Total_Salary_Owed = Var.11) %>%
    mutate(Player = stri_trans_general(Player,  id = "Latin-ASCII"),
           Total_Salary_Owed = case_when(Total_Salary_Owed == "" ~ '0',
                                         TRUE ~ Total_Salary_Owed),
           Player = replace(Player, Player == 'J.J. Redick', 'JJ Redick'),
           Player = replace(Player, Player == 'T.J. Leaf', 'TJ Leaf'),
           Player = replace(Player, Player == 'Marcus Morris', 'Marcus Morris Sr.'),
           Player = replace(Player, Player == 'Danuel House', 'Danuel House Jr.'),
           Player = replace(Player, Player == 'Robert Williams', 'Robert Williams III'),
           Player = replace(Player, Player == 'Otto Porter', 'Otto Porter Jr.'),
           Player = replace(Player, Player == 'Kevin Knox', 'Kevin Knox II'),
           Player = replace(Player, Player == 'Lonnie Walker', 'Lonnie Walker IV'),
           Player = replace(Player, Player == 'Sviatoslav Mykhailiuk', 'Svi Mykhailiuk'),
           Player = replace(Player, Player == 'Harry Giles', 'Harry Giles III'),
           Salary2 = gsub("\\$", "", Salary),
           Salary3 = as.numeric(gsub("\\,", "", Salary2)),
           Salary4 = gsub("\\$", "", Total_Salary_Owed),
           Salary5 = as.numeric(gsub("\\,", "", Salary4)),
           Team = replace(Team, Team == 'BRK', 'BKN'),
           Team = replace(Team, Team == 'PHO', 'PHX'),
           Team = replace(Team, Team == 'CHO', 'CHA')) %>%
    select(Player, Team, Salary3, Salary5) %>%
    group_by(Player) %>%
    distinct() %>%
    ungroup() %>%
    rename(Salary = Salary3, `Total Salary Owed` = Salary5)
}

get_clean_foul_data <- function(df){
  df <- df %>%
    pivot_longer(descriptionPlayHome:descriptionPlayVisitor, names_to = 'fouls') %>%
    select(value, idGame) %>%
    filter(!is.na(value)) %>%
    filter(str_detect(value, regex('foul', ignore_case = TRUE))) %>%
    filter(!str_detect(value, 'Turnover')) %>%
    filter(!str_detect(value, 'FLAGRANT')) %>%
    separate(value, c("A", "B", "referee"), sep = "[(]") %>%
    mutate(referee = str_sub(referee, 1, str_length(referee)-1)) %>%
    separate(A, c('F', 'G', 'H'), sep = " ") %>%
    mutate(G = str_replace(G, "Jr.", ""),
           G = str_replace(G, "Sr.", ""),
           G = str_replace(G, "III", ""),
           G = str_replace(G, "Hill", ""),
           H = str_replace(H, "Offensive", "OFFENSIVECHARGE")) %>%
    unite("foul_type", G:H, sep = "") %>%
    select(referee, foul_type, idGame) %>%
    mutate(foul_type = toupper(foul_type),
           foul_type = str_replace(foul_type, "PERSONALTAKE", "PERSONAL"))
  
  return(df)
  
}

get_advanced_stats <- function(){
  url <- 'https://www.basketball-reference.com/leagues/NBA_2021.html'
  webpage <- read_html(url)
  data <- webpage %>% 
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>% 
    html_node("#misc_stats") %>% 
    html_table()
  
  
  data2 <- data %>%
    janitor::row_to_names(row_number = 1) %>%
    select(Rk:Pace) %>%
    select(-Rk, -PW, -PL, -MOV, -SOS, -SRS) %>%
    mutate(Team = str_replace_all(Team, pattern = "\\*", replacement = ""),
           NRtg = str_replace_all(NRtg, pattern = "\\+", replacement = ""),
           ORtg = as.numeric(ORtg),
           DRtg = as.numeric(DRtg),
           NRtg = as.numeric(NRtg),
           Pace = as.numeric(Pace)) %>%
    left_join(acronyms) %>%
    select(-Team) %>%
    rename(Team = Team1, ORTG = ORtg, DRTG = DRtg, NRTG = NRtg) %>%
    select(Team, everything())
  
  data2 <- data2[-31,]
  return(data2)
}


# Loading in Data 
acronyms <- read_csv('data/acronyms.csv')
injuryData <- get_injuries_data()
salary <- read_csv('data/salary.csv')
gameLogs <- get_gamelogs_data()
dataBREFTeamJoined <- read_csv('data/dataBREFTeamJoined.csv')
team_ratings <- get_advanced_stats()
schedule <- getSchedule()
transactions <- get_transactions()
contracts <- read_csv('data/contracts1.csv')
pbp_data <- get_playbyplay_data()
last_season_wins <- read_csv('data/lastseasonwins.csv')

rm(dataBREFMiscTeams, dataBREFPerGameTeams, dataBREFPerPossTeams, dataBREFPlayerAdvanced, dataBREFShootingTeams,
   dataBREFStandings, dataBREFStandingsDivTeams, dataBREFTotalsTeams, dataBREFStandingsConfTeams, df_dict_nba_players,
   df_dict_nba_teams, df_nba_player_dict)

###### Data Extraction Complete ######
# Data Manipulation ----

full_team_names <- dataBREFTeamJoined %>%
  distinct(nameTeam, slugTeamBREF) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'BRK', 'BKN')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'PHO', 'PHX')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'CHO', 'CHA')) %>%
  rename(Team = slugTeamBREF, FullName = nameTeam)

injury_data <- injuryData %>%
  select(Player, Team, Date, Status, Injury, Description) %>%
  mutate(Team = replace(Team, Team == 'Atlanta Hawks', 'ATL')) %>%
  mutate(Team = replace(Team, Team == 'Boston Celtics', 'BOS')) %>%
  mutate(Team = replace(Team, Team == 'Brooklyn Nets', 'BKN')) %>%
  mutate(Team = replace(Team, Team == 'Charlotte Hornets', 'CHA')) %>%
  mutate(Team = replace(Team, Team == 'Chicago Bulls', 'CHI')) %>%
  mutate(Team = replace(Team, Team == 'Cleveland Cavaliers', 'CLE')) %>%
  mutate(Team = replace(Team, Team == 'Dallas Mavericks', 'DAL')) %>%
  mutate(Team = replace(Team, Team == 'Denver Nuggets', 'DEN')) %>%
  mutate(Team = replace(Team, Team == 'Detroit Pistons', 'DET')) %>%
  mutate(Team = replace(Team, Team == 'Golden State Warriors', 'GSW')) %>%
  mutate(Team = replace(Team, Team == 'Houston Rockets', 'HOU')) %>%
  mutate(Team = replace(Team, Team == 'Indiana Pacers', 'IND')) %>%
  mutate(Team = replace(Team, Team == 'Los Angeles Clippers', 'LAC')) %>%
  mutate(Team = replace(Team, Team == 'Los Angeles Lakers', 'LAL')) %>%
  mutate(Team = replace(Team, Team == 'Memphis Grizzlies', 'MEM')) %>%
  mutate(Team = replace(Team, Team == 'Miami Heat', 'MIA')) %>%
  mutate(Team = replace(Team, Team == 'Milwaukee Bucks', 'MIL')) %>%
  mutate(Team = replace(Team, Team == 'Minnesota Timberwolves', 'MIN')) %>%
  mutate(Team = replace(Team, Team == 'New Orleans Pelicans', 'NOP')) %>%
  mutate(Team = replace(Team, Team == 'New York Knicks', 'NYK')) %>%
  mutate(Team = replace(Team, Team == 'Oklahoma City Thunder', 'OKC')) %>%
  mutate(Team = replace(Team, Team == 'Orlando Magic', 'ORL')) %>%
  mutate(Team = replace(Team, Team == 'Philadelphia 76ers', 'PHI')) %>%
  mutate(Team = replace(Team, Team == 'Phoenix Suns', 'PHX')) %>%
  mutate(Team = replace(Team, Team == 'Portland Trail Blazers', 'POR')) %>%
  mutate(Team = replace(Team, Team == 'Sacramento Kings', 'SAC')) %>%
  mutate(Team = replace(Team, Team == 'San Antonio Spurs', 'SAS')) %>%
  mutate(Team = replace(Team, Team == 'Toronto Raptors', 'TOR')) %>%
  mutate(Team = replace(Team, Team == 'Utah Jazz', 'UTA')) %>%
  mutate(Team = replace(Team, Team == 'Washington Wizards', 'WAS')) %>%
  left_join(full_team_names)

injury_data_count <- injury_data %>%
  group_by(Team) %>%
  summarise('Active Injuries' = n())

salary_Merge <- contracts %>%
  select(Player, Salary)

ts_percent <- gameLogs %>%
  group_by(Player) %>%
  mutate(season_ts_percent = sum(PTS) / (2 * (sum(FGA) + (0.44 * sum(FTA)))),
         season_ts_percent = round(season_ts_percent, 3)) %>%
  select(Player, season_ts_percent) %>%
  distinct() %>%
  ungroup()

GP <- gameLogs %>%
  group_by(Player) %>%
  summarise(GP = n())

gameLogs_Two <- gameLogs %>%
  mutate(game_ts_percent = PTS / (2 * (FGA + (0.44 * FTA)))) %>%
  left_join(ts_percent) %>%
  left_join(salary_Merge) %>%
  group_by(Player) %>%
  mutate(MVPCalc = (mean(PTS) + (mean(PlusMinus) + (2 * mean(STL + BLK) + (0.5 * mean(TRB)) -  (1.5 * mean(TOV)) + mean(AST)))),
         Date = as.Date(Date)) %>%
  ungroup() %>%
  mutate(MVPCalc_game = (PTS + (2 * PlusMinus) + (2 * STL + BLK) + (0.5 * TRB) - TOV + AST)) %>%
  left_join(GP) %>%
  left_join(full_team_names) %>%
  mutate(abc3 = (MVPCalc * GP)) %>%
  group_by(Player, GameID) %>%
  distinct() %>%
  ungroup()

player_teams <- gameLogs_Two %>%
  select(Player, Team) %>%
  mutate(team1 = Team)

gg <- contracts %>%
  left_join(player_teams)

rm(gameLogs)

gameLogs_Yesterday <- gameLogs_Two %>%
  filter(Date == max(Date))

top_15_yesterday <- gameLogs_Yesterday %>%
  select(Player, Team, PTS, Outcome, Salary) %>%
  top_n(15, PTS) %>%
  arrange(desc(PTS)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Player, Team, PTS, Outcome, Salary)

team_Wins_Yesterday <- gameLogs_Yesterday %>%
  filter(Date == max(Date)) %>%
  group_by(Team, Opponent, Location, GameID, Outcome, Date) %>%
  summarise(Team_PTS_Scored = sum(PTS)) %>%
  ungroup() %>%
  rename('Team Points Scored' = Team_PTS_Scored) %>%
  select(-GameID, Team, Outcome, Location, 'Team Points Scored', Opponent, Date) %>%
  arrange(Outcome)

recent_Bans2 <- team_Wins_Yesterday %>%
  group_by(Location, Outcome) %>%
  summarise(Count = n()) %>%
  filter(Location == 'H') %>%
  ungroup() %>%
  mutate(WinP = Count / sum(Count),
         WinP = format(round(WinP, 2), nsmall = 2)) %>%
  mutate(Outcome = replace(Outcome, Outcome == 'L', 'Road Wins'),
         Outcome = replace(Outcome, Outcome == 'W', 'Home Wins')) %>%
  select(-Location) %>%
  rename(Location = Outcome, 'Win Percentage' = WinP) %>%
  pivot_wider(names_from = Location, values_from = c(Count, 'Win Percentage'))

recent_Bans <- team_Wins_Yesterday %>%
  summarise(NumberofGames = n(),
            Date = max(Date)) %>%
  mutate(NumberofGames = NumberofGames / 2) %>%
  rename('Number of Games' = NumberofGames) %>%
  bind_cols(recent_Bans2)

top_score_distribution <- gameLogs_Two %>%
  mutate(month = month(Date)) %>%
  filter(PTS >= 40) %>%
  mutate(month = factor(month.name[month], levels = month.name)) %>%
  count(Player, month) %>%
  group_by(month) %>%
  summarise(sum = sum(n))

number_of_games <- gameLogs_Two %>%
  select(GameID) %>%
  distinct() %>%
  count(name = "Number of Games")

home_road_winpercent <- gameLogs_Two %>%
  group_by(Location, Outcome) %>%
  distinct(GameID) %>%
  filter(Location == 'H') %>%
  summarise(n = n()) %>%
  mutate('Win Percentage' = round(n / sum(n) * 100, 1)) %>%
  rename(Wins = n) %>%
  mutate(Outcome = replace(Outcome, Outcome == 'L', 'Away'),
         Outcome = replace(Outcome, Outcome == 'W', 'Home')) %>%
  ungroup() %>%
  select(-Location) %>%
  rename(Location = Outcome) %>%
  pivot_wider(names_from = Location, values_from = c(Wins, 'Win Percentage')) %>%
  mutate(Home = 'Home',
         Away = 'Away',
         'Total Games' = sum(Wins_Away + Wins_Home)) %>%
  rename('Road Wins' = Wins_Away, 'Home Wins' = Wins_Home, 'Win Percentage Road' = 'Win Percentage_Away',
         'Win Percentage Home' = 'Win Percentage_Home')

back_to_backs <- gameLogs_Two %>% # get team win percentage on these games.  
  group_by(Team, isB2BSecond) %>%
  distinct(GameID) %>%
  filter(isB2BSecond == TRUE) %>%
  summarise(n = n())

team_Wins_Yesterday2 <- team_Wins_Yesterday %>%
  select(-Date) %>%
  mutate(Location = replace(Location, Location == 'H', 'Home'),
         Location = replace(Location, Location == 'A', 'Road'),
         Outcome = replace(Outcome, Outcome == 'W', 'Win'),
         Outcome = replace(Outcome, Outcome == 'L', 'Loss'),
         Vs = 'Vs') %>%
  select(Team, `Team Points Scored`, Outcome, Vs, Opponent) %>%
  arrange(desc(Outcome))

win_streak <- gameLogs_Two %>%
  mutate(win = case_when(Outcome == 'W' ~ 1,
                         TRUE ~ 0)) %>%
  group_by(Team, GameID, win) %>%
  distinct(GameID) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(streak = streak_run(win)) %>%
  filter(GameID == max(GameID)) %>%
  ungroup() %>%
  mutate(type = case_when(win == 0 ~ 'Losing',
                          TRUE ~ 'Winning'),
         winbb = case_when(win == 0 ~ 0,
                           TRUE ~ as.double(streak))) %>%
  select(Team, winbb) %>%
  rename(`Win Streak` = winbb)

team_wins <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, Outcome) %>%
  distinct(GameID) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Outcome, values_from = n) %>%
  select(Team, W, L) %>%
  left_join(injury_data_count) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  rename(Wins = W, Losses = L) %>%
  left_join(win_streak) %>%
  left_join(full_team_names) %>%
  mutate(WinPercentage = (Wins / (Wins + Losses))) %>%
  arrange(FullName)

conferences <- dataBREFTeamJoined %>%
  select(nameConference, slugTeamBREF) %>%
  rename(Conference = nameConference, Team = slugTeamBREF) %>%
  mutate(Team = replace(Team, Team == 'BRK', 'BKN')) %>%
  mutate(Team = replace(Team, Team == 'PHO', 'PHX')) %>%
  mutate(Team = replace(Team, Team == 'CHO', 'CHA'))

east_standings <- team_wins %>%
  left_join(conferences) %>%
  filter(Conference == 'Eastern') %>%
  select(-Conference) %>%
  mutate(Seed = min_rank(desc(WinPercentage))) %>%
  select(-WinPercentage) %>%
  select(Seed, FullName, Wins, Losses, `Win Streak`, 'Active Injuries') %>%
  arrange(Seed) %>%
  rename(Team = FullName)

west_standings <- team_wins %>%
  left_join(conferences) %>%
  filter(Conference == 'Western') %>%
  select(-Conference) %>%
  mutate(Seed = min_rank(desc(WinPercentage))) %>%
  select(-WinPercentage) %>%
  select(Seed, FullName, Wins, Losses, `Win Streak`, 'Active Injuries') %>%
  rename(Team = FullName) %>%
  mutate(Seed = replace(Seed, Team == 'Houston Rockets', 5),
         Seed = replace(Seed, Team == 'Utah Jazz', 6)) %>%
  arrange(Seed)


team_points_histogram <- gameLogs_Two %>%
  group_by(Team, GameID, Outcome, Opponent) %>%
  summarise(Total_PTS = sum(PTS))

top_20pt_scorers <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  filter(mean(PTS) >= 20) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team, GP) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  mutate(Rank = row_number(),
         Top5 = case_when(Rank <= 5 ~ 'Top 5 MVP Candidate',
                          TRUE ~ 'Other'))

top5 <- top_20pt_scorers %>%
  filter(Top5 == 'Top 5 MVP Candidate') %>%
  select(Player, Top5)


team_ppg_scorers <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Player) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team, GP) %>%
  mutate(season_ts_percent = round(season_ts_percent, 3)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  left_join(top5) %>%
  mutate(Top5 = replace_na(Top5, 'Other')) %>%
  left_join(full_team_names)


team_ratings_logo <- team_ratings %>%
  mutate(logo = case_when(Team == 'ATL' ~ 'logos/atl.png',
                          Team == 'BOS' ~ 'logos/bos.png',
                          Team == 'BKN' ~ 'logos/bkn.png',
                          Team == 'CHA' ~ 'logos/cha.png',
                          Team == 'CHI' ~ 'logos/chi.png',
                          Team == 'CLE' ~ 'logos/cle.png',
                          Team == 'DAL' ~ 'logos/dal.png',
                          Team == 'DEN' ~ 'logos/den.png',
                          Team == 'DET' ~ 'logos/det.png',
                          Team == 'GSW' ~ 'logos/gsw.png',
                          Team == 'HOU' ~ 'logos/hou.png',
                          Team == 'IND' ~ 'logos/ind.png',
                          Team == 'LAC' ~ 'logos/lac.png',
                          Team == 'LAL' ~ 'logos/lal.png',
                          Team == 'MEM' ~ 'logos/mem.png',
                          Team == 'MIA' ~ 'logos/mia.png',
                          Team == 'MIL' ~ 'logos/mil.png',
                          Team == 'MIN' ~ 'logos/min.png',
                          Team == 'NOP' ~ 'logos/nop.png',
                          Team == 'NYK' ~ 'logos/nyk.png',
                          Team == 'OKC' ~ 'logos/okc.png',
                          Team == 'ORL' ~ 'logos/orl.png',
                          Team == 'PHI' ~ 'logos/phi.png',
                          Team == 'PHX' ~ 'logos/phx.png',
                          Team == 'POR' ~ 'logos/por.png',
                          Team == 'SAC' ~ 'logos/sac.png',
                          Team == 'SAS' ~ 'logos/sas.png',
                          Team == 'TOR' ~ 'logos/tor.png',
                          Team == 'UTA' ~ 'logos/uta.png',
                          Team == 'WAS' ~ 'logos/was.png'))


######### Data Manipulation Complete ########
# Graphs ----

top20_plot <- function(df){
  p <- df %>%
    ggplot(aes(avg_PTS, season_ts_percent, color = Top5, text = paste(Player, '<br>',
                                                                      Team, '<br>',
                                                                      'PPG: ', round(avg_PTS, 1), '<br>',
                                                                      'TS%: ', round(season_ts_percent, 3), '<br>',
                                                                      'Games Played: ', GP))) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(fill = Top5)) +
    scale_y_continuous(labels = scales::percent, limits=c(.40, .88), breaks=seq(.40, .88, by = .08)) + 
    scale_color_manual(values = c('light blue', 'orange')) +
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(color = 'Top 5 MVP Candidate', fill = 'Top 5 MVP Candidate',
         title = 'Player Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35))
  
}

team_ppg_plot <- function(df){
  p <- df %>%
    ggplot(aes(avg_PTS, season_ts_percent, color = Top5, text = paste(Player, '<br>',
                                                                      Team, '<br>',
                                                                      'PPG: ', round(avg_PTS, 1), '<br>',
                                                                      'TS%: ', round(season_ts_percent, 3), '<br>',
                                                                      'Games Played: ', GP))) +
    geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(fill = Top5)) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c('light blue', 'orange')) +
    scale_fill_manual(values = c('light blue', 'orange')) +
    labs(color = 'Top 5 MVP Candidate', fill = 'Top 5 MVP Candidate',
         title = 'Player Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
         x = 'Average Points per Game',
         y = 'True Shooting Percentage') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35))
  
}

# add green + red annotations for good & bad
#   geom_text_repel(aes(label = Team), nudge_y = -0.6) +
geom_logos <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
  geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., geom = 'logos', size = .13)
}

annotations <- data.frame(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Good Defense \nBad Offense","Bad Defense \nBad Offense"
                   ,"Good Defense \nGood Offense","Bad Defense \nGood Offense"),
  hjustvar = c(0,0,.98,.98) ,
  vjustvar = c(.98,0,.98,0)) #<- adjust

team_plot <- team_ratings_logo %>%
  ggplot(aes(ORTG, DRTG)) +
  geom_vline(xintercept = mean(team_ratings$DRTG), linetype = "dashed") +
  geom_hline(yintercept = mean(team_ratings$ORTG), linetype = "dashed") +
  geom_logos(aes(image = logo)) +
  geom_text(data = annotations, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText)) +
  scale_y_reverse() +
  labs(title = 'Offensive vs Defensive Ratings',
       x = 'Offensive Rating',
       y = 'Defensive Rating') +
  theme_minimal(base_size=15, base_family="Gill Sans MT") %+replace% 
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
  ) +
  scale_size_identity() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')


#### TEST STUFF #########
team_choices <- unique(team_wins$FullName)

opp_pts <- gameLogs_Two %>%
  group_by(Team, GameID, Date) %>%
  summarise(tot_pts_opp = sum(PTS)) %>%
  rename(Opponent = Team)
  
team_mov <- gameLogs_Two %>%
  group_by(Team, GameID, Date) %>%
  summarise(tot_pts = sum(PTS)) %>%
  left_join(full_team_names) %>%
  left_join(opp_pts) %>%
  filter(Team != Opponent) %>%
  mutate(mov = tot_pts - tot_pts_opp,
         outcome = case_when(mov > 0 ~ 'W',
                             TRUE ~ 'L'))

rm(opp_pts)

mov_plot <- function(df){
  cols <- c('W' = 'dark green', 'L' = 'red')
  p <- df %>%
    ggplot(aes(Date, mov)) +
    geom_col(alpha = 0.7, aes(fill = outcome, text = paste(Date, '<br>',
                                                           outcome, ' vs', Opponent, '<br>',
                                                           'Scoreline: ', tot_pts, ' - ', tot_pts_opp, '<br>',
                                                           'Margin of Victory: ', mov))) +
    scale_y_continuous(breaks = c(-50, -45, -40, -35, -30, -25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
    scale_fill_manual(values = cols) +
    labs(x = NULL,
         y = 'Margin of Victory',
         title = paste0(df$FullName[1], ' Game Log History \n 2020-2021 NBA Season')) +
    theme_jacob() +
    theme(axis.text.x = element_blank())
  
  
  ggplotly(p, tooltip = c('text'))
  
}

schedule_main <- schedule %>%
  select(date_game, game_start_time, visitor_team_name, visitor_pts, home_team_name) %>%
  rename(Date = date_game, `Start Time (EST)` = game_start_time, `Team 1` = visitor_team_name, Vs = visitor_pts,
         `Team 2` = home_team_name) %>%
  mutate(Vs = replace_na(Vs, 'Vs'),
         Date = as.Date(Date, format = '%a, %b %d, %Y'),
         `Day of Week` = wday(Date, label = TRUE, abbr = FALSE),
         `Start Time (EST)` = str_replace_all(`Start Time (EST)`, 'p', ' PM'),
         bb = substr(`Start Time (EST)`, 0, 4),
         b = parse_date_time(`Start Time (EST)`, '%I:%M %p')) %>%
  filter(Date >= todayDate) %>%
  arrange(Date, b) %>%
  select(Date, `Day of Week`, `Start Time (EST)`, `Team 1`, Vs, `Team 2`)


team_rank <- team_wins %>%
  mutate(Rank = row_number()) %>%
  select(FullName, Rank) %>%
  rename(Team = FullName) %>%
  mutate(Team = replace(Team, Team == 'LA Clippers', 'Los Angeles Clippers'))

opponent_rank <- team_wins %>%
  mutate(opp_Rank = row_number()) %>%
  select(FullName, opp_Rank) %>%
  rename(Team = FullName) %>%
  mutate(Team = replace(Team, Team == 'LA Clippers', 'Los Angeles Clippers')) %>%
  rename(Opponent = Team)

opponent_prac <- schedule_main %>%
  mutate(game_id = row_number()) %>%
  mutate(opponent = `Team 1`,
         opponent2 = `Team 2`) %>%
  select(game_id, opponent, opponent2)

schedule_plot_df <- schedule_main %>%
  mutate(game_id = row_number()) %>%
  pivot_longer(cols = starts_with('Team'),
               names_to = 'Teambb',
               values_to = 'value') %>%
  select(-Teambb) %>%
  rename(Team = value) %>%
  select(Team, game_id) %>%
  left_join(opponent_prac) %>%
  pivot_longer(cols = starts_with('opponent'),
               names_to = 'team',
               values_to = 'valuebb') %>%
  filter(Team != valuebb) %>%
  select(-team) %>%
  rename(Opponent = valuebb) %>%
  left_join(team_rank) %>%
  left_join(opponent_rank) %>%
  group_by(Team) %>%
  summarise(avg_opp_rank = mean(opp_Rank)) %>%
  left_join(team_rank) %>%
  mutate(differential = avg_opp_rank - Rank,
         Team = fct_reorder(Team, avg_opp_rank))

schedule_plot <- function(df){
  
  p <- df %>%
    ggplot(aes(avg_opp_rank, Team, fill = differential > 0)) +
    geom_col(aes(text = paste0('Team: ', Team, '<br>',
                               'Team Rank: ', Rank, '<br>',
                               'Average Opponent Rank: ', avg_opp_rank))) +
    labs(y = NULL,
         x = 'Average Opponent Rank',
         title = 'Strength of Schedule Breakdown for the remaining month') +
    theme_jacob() +
    theme(legend.position='none')
  
  ggplotly(p, tooltip = c('text'))
}
# schedule_plot(schedule_plot_df)

regular_valuebox_function <- function(df){
  if (nrow(df) > 0){
    valueBox(value = paste0(df$`Wins`, '-', df$`Losses`), "Win / Loss Record", icon = icon("list"), color = "purple")
  }
  else {
    valueBox(value = paste0('No Data Available'), "Team Hasn't Played Yet", icon = icon("list"), color = "purple")
  }
}

last_season_valuebox_function <- function(df){
  if (nrow(df) > 0){
    valueBox(value = paste0(df$`Wins`, '-', df$`Losses`), "Last Season's Win / Loss Record", icon = icon("list"), color = "purple")
  }
  else {
    valueBox(value = paste0('No Data Available'), "Team Hasn't Played Yet", icon = icon("list"), color = "purple")
  }
}

####
game_types <- team_mov %>%
  filter(mov >= 1) %>%
  mutate(clutch_game = case_when(mov<= 5 ~ 'Clutch Game',
                                 mov >= 5 & mov <= 10 ~ '10 Pt Game',
                                 mov > 10 ~ 'Blowout Game',
                                 TRUE ~ 'Help'),
         season_date = case_when(Date <= '2020-07-29' ~ 'Pre-Bubble',
                                 Date > '2020-07-29' ~ 'Bubble',
                                 TRUE ~ 'Help')) %>%
  select(GameID, Date, clutch_game, season_date, mov) %>%
  group_by(clutch_game) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(pct_total = n / sum(n)) %>%
  ungroup() %>%
  mutate(explanation = case_when(clutch_game == 'Blowout Game' ~ 'more than 10 points',
                                 clutch_game == 'Clutch Game' ~ '5 points or less',
                                 TRUE ~ 'between 6 - 10 points'))


game_types_plot <- function(df){
  p <- df %>%
    ggplot(aes(clutch_game, pct_total)) +
    geom_col(position = 'dodge', aes(text = paste0(clutch_game, 's account for ', round(pct_total * 100, 1), '% of all',
                                                   ' games played.', '<br>', 'Number of Observations: ', n, '<br>', '<br>',
                                                   clutch_game, 's are defined as games that were decided by ', explanation))) +
    scale_y_continuous(labels = percent_format()) +
    labs(x = 'Game Type',
         y = 'Percent of Total Games',
         fill = NULL,
         title = 'Game Type Distribution for 2020-21 NBA Season') +
    theme_jacob() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
  
  ggplotly(p, tooltip = c('text')) %>%
    layout(legend = list(orientation = "h", x = 0.35, y = 1.03))
}

# to do - last 2 weeks / last 5 games ppg + TS%
# do ppg vs ts% for each team and maybe make a slider option for TS% analysis, salary analysis, and contract analysis?

pbp_data_new <- get_clean_foul_data(pbp_data)

# i think makes group based on salary ranges
contract_df <- gameLogs_Two %>%
  select(Player, Team, MVPCalc, abc3, Salary, GP, FullName) %>%
  distinct() %>%
  mutate(salary_rank = case_when(Salary >= 30000000 ~ "$30+ M",
                                 Salary >= 25000000 & Salary < 30000000 ~ "$25-30 M",
                                 Salary >= 20000000 & Salary < 25000000 ~ "$20-25 M",
                                 Salary >= 15000000 & Salary < 20000000 ~ "$15-20 M",
                                 Salary >= 10000000 & Salary < 15000000 ~ "$10-15 M",
                                 Salary >= 5000000  & Salary < 10000000 ~ "$5-10 M",
                                 TRUE ~ "< $5 M")) %>%
  group_by(salary_rank) %>%
  mutate(rankingish = round(percent_rank(abc3), 3),
         pvm_rank = round(mean(MVPCalc), 4),
         rankish_text = rankingish * 100) %>%
  ungroup() %>%
  mutate(total = n()) %>%
  group_by(salary_rank) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(pct_total = count / total,
         pct_total = round(pct_total, 3),
         salary_rank = fct_reorder(salary_rank, count),
         color_var = case_when(rankish_text >= 60 & Salary >= 25000000 ~ 'Superstars',
                               rankish_text >= 90 ~ 'Great Value',
                               rankish_text < 90 & rankish_text >= 20 ~ 'Normal',
                               TRUE ~ 'Bad Value'))

contracts_dist_plot <- function(df){
  df %>%
    select(count, salary_rank, pct_total, pvm_rank) %>%
    distinct() %>%
    ggplot(aes(count, salary_rank, fill = pvm_rank, label = scales::percent(pct_total))) +
    geom_col() +
    geom_text(aes(x = count, y = salary_rank, label = scales::percent(pct_total)), position = position_dodge(width = 1), hjust = -0.07,
              size = 4.5) +
    scale_x_continuous(limits = c(0, 250)) +
    labs(x = 'Number of Contracts',
         y = NULL,
         title = 'Distribution of Contracts in the 2020-21 NBA Season') +
    theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

value_plot <- function(df){
  p <- df %>%
    ggplot(aes(as.numeric(Salary), MVPCalc, fill = color_var)) +
    geom_point(aes(text = paste0(Player, '<br>',
                                 FullName, '<br>',
                                 'Salary: $', formatC(Salary, format = 'f', big.mark = ",", digits = 0), '<br>',
                                 'Player Value Metric: ', MVPCalc, '<br>',
                                 'Games Played: ', GP, '<br>',
                                 Player, ' is in the top ', rankish_text, '% Percentile for all players in this Salary Group (',
                                 salary_rank, ')')),
               size = 2.5, shape = 21, alpha = 0.7) +
    scale_x_continuous(labels = label_dollar()) +
    theme_jacob() +
    labs(y = 'Player Value Metric',
         x = 'Salary',
         title = 'What are the least & most valuable contracts in the 2020-21 NBA Season ?',
         fill = 'Color Legend') +
    scale_fill_manual(values=c("red", "green", "grey70", 'purple'))
  
  ggplotly(p, tooltip = c('text'))
}
value_plot(contract_df)

# contracts_dist_plot(contract_df)

league_avg_ppg <- gameLogs_Two %>%
  filter(Type == 'Regular Season') %>%
  group_by(Team, GameID, Date) %>%
  summarise(total_pts = sum(PTS)) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarise(avg_pts = mean(total_pts)) %>% # this is cutoff for avg ppg by team 
  ungroup() %>%
  summarise(league_avg = mean(avg_pts)) %>%
  mutate(league_avg = round(league_avg, 1),
         last_yr_ppg = 111.8,
         pct_change = (league_avg / last_yr_ppg) * 100 - 100,
         pct_change = round(pct_change, 2))

upcoming_games <- schedule_main %>%
  filter(Date == min(Date)) %>%
  group_by(Date, `Day of Week`) %>%
  count() %>%
  rename(gp = n)

main_bans <- upcoming_games %>%
  cbind(home_road_winpercent) %>% 
  cbind(league_avg_ppg) %>%
  mutate(full_date = format(Date, '%B %d, %Y'),
         record = paste0(`Home Wins`, " - ", `Road Wins`)) 

gp_valuebox_function <- function(df){
  if (df$gp == 1){
    valueBox(
      value = df$full_date, HTML(paste0("Next Gameday Date <br> <br> ", df$gp,
                                             " Upcoming Game")),
      icon = icon("calendar"), color = "blue"
    )
  }
  else {
    valueBox(
      value = df$full_date, HTML(paste0("Next Gameday Date <br> <br> ", df$gp,
                                             " Upcoming Games")),
      icon = icon("calendar"), color = "blue"
    )
  }
}

contract_value <- contracts %>%
  left_join(GP) %>%
  mutate(GP = replace_na(GP, 0)) %>%
  group_by(Team) %>%
  mutate(team_gp = max(GP),
         team_max_value = (sum(Salary) * team_gp),
         team_achieved_value = (sum(Salary * GP))) %>%
  ungroup() %>%
  mutate(missing_value = team_max_value - team_achieved_value,
         pct_achieved = team_achieved_value / team_max_value,
         pct_missing = missing_value / team_max_value,
         pct_achieved = round(pct_achieved, 3),
         pct_missing = round(pct_missing, 3))

