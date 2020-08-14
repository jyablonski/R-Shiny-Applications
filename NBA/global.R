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


Sys.setenv (TZ="America/Los_Angeles")
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
gs4_auth(path = '.secrets', email = 'jyablonski9@gmail.com')
drive_auth(cache = ".secrets", email = "jyablonski9@gmail.com")

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

teamStatsDownload <- function() {
  if (isSeasonActive == TRUE & as.double(Sys.time() - file_info('data/dataBREFTeamJoined.csv')$change_time, units = 'hours') > 8.0){
    dataBREFTeamJoined <- bref_teams_stats(seasons = 2020) %>%
      unnest(dataTable)
    rm(dataBREFMiscTeams, dataBREFPerGameTeams, dataBREFPerPossTeams, dataBREFPlayerAdvanced, dataBREFShootingTeams,
       dataBREFStandings, dataBREFStandingsDivTeams, dataBREFTotalsTeams, dataBREFStandingsConfTeams, df_dict_nba_players,
       df_dict_nba_teams, df_nba_player_dict)
    write_csv(dataBREFTeamJoined, 'data/dataBREFTeamJoined.csv', append = FALSE)
    return(dataBREFTeamJoined)
    
  }
  else {
    dataBREFTeamJoined <- read_csv('data/dataBREFTeamJoined.csv')
    return(dataBREFTeamJoined)
  }
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

getSchedule <- function(){
  currentmonth <- tolower(format(Sys.Date(), "%B"))
  nextmonth <- tolower(format(Sys.Date() + 30, "%B"))
  year <- format(Sys.Date(), "%Y")
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



# Loading in Data 
injuryData <- get_injuries_data()
salary <- read_csv('data/salary.csv')
gameLogs <- get_gamelogs_data()
dataBREFTeamJoined <- teamStatsDownload()
schedule <- getSchedule()


rm(dataBREFMiscTeams, dataBREFPerGameTeams, dataBREFPerPossTeams, dataBREFPlayerAdvanced, dataBREFShootingTeams,
   dataBREFStandings, dataBREFStandingsDivTeams, dataBREFTotalsTeams, dataBREFStandingsConfTeams, df_dict_nba_players,
   df_dict_nba_teams, df_nba_player_dict)

###### Data Extraction Complete ######
# Data Manipulation ----

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
  mutate(Team = replace(Team, Team == 'Washington Wizards', 'WAS'))

injury_data_count <- injury_data %>%
  group_by(Team) %>%
  summarise('Active Injuries' = n())

salary_Merge <- salary %>%
  select(Player, Salary)

ts_percent <- gameLogs %>%
  group_by(Player) %>%
  mutate(season_ts_percent = sum(PTS) / (2 * (sum(FGA) + (0.44 * sum(FTA))))) %>%
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
  mutate(MVPCalc = (mean(PTS) + (2 * mean(PlusMinus) + (2 * mean(STL + BLK) + (0.5 * mean(TRB) - mean(TOV) + mean(AST))))),
         Date = as.Date(Date)) %>%
  ungroup() %>%
  left_join(GP) %>%
  mutate(abc1 = (MVPCalc / Salary),
         abc2 = (Salary / MVPCalc),
         abc3 = (MVPCalc * GP),
         GV = case_when(abc3 >= 1800 & Salary >= 24000000 ~ 'Superstars',
                        abc3 >= 1800 & Salary <= 8000000 ~ 'Great Value',
                        abc3 < 900 & Salary > 15000000 ~ 'Stinkers',
                        TRUE ~ 'Other'))

gameLogs_Yesterday <- gameLogs_Two %>%
  filter(Date == max(Date))

gameLogs_Weekly <- gameLogs_Two %>%
  filter(Date > (max(Date) - 7))

gameLogs_Monthly <- gameLogs_Two %>%
  filter(month(Date) == month(max(Date)))

top_15_yesterday <- gameLogs_Yesterday %>%
  select(Player, Team, PTS, Outcome, Salary) %>%
  top_n(15, PTS) %>%
  arrange(desc(PTS)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Player, Team, PTS, Outcome, Salary)

team_ratings <- dataBREFTeamJoined %>%
  select(slugTeamBREF, ortgTeamMisc, drtgTeamMisc, nrtgTeamMisc, paceTeamMisc) %>%
  rename(Team = slugTeamBREF, ORTG = ortgTeamMisc, DRTG = drtgTeamMisc, NRTG = nrtgTeamMisc, Pace = paceTeamMisc) %>%
  mutate(DateOfEntry = Sys.Date()-1) %>%
  mutate(Team = replace(Team, Team == 'BRK', 'BKN')) %>%
  mutate(Team = replace(Team, Team == 'PHO', 'PHX')) %>%
  mutate(Team = replace(Team, Team == 'CHO', 'CHA'))


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
  pivot_wider(names_from = Location, values_from = c(Count, 'Win Percentage')) %>%
  rename('Road Wins' = 'Count_Road Wins', 'Home Wins' = 'Count_Home Wins', 'Win Percentage Road' = 'Win Percentage_Road Wins',
         'Win Percentage Home' = 'Win Percentage_Home Wins')

recent_Bans <- team_Wins_Yesterday %>%
  summarise(NumberofGames = n(),
            Date = max(Date)) %>%
  mutate(NumberofGames = NumberofGames / 2) %>%
  rename('Number of Games' = NumberofGames) %>%
  bind_cols(recent_Bans2)

full_team_names <- dataBREFTeamJoined %>%
  distinct(nameTeam, slugTeamBREF) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'BRK', 'BKN')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'PHO', 'PHX')) %>%
  mutate(slugTeamBREF = replace(slugTeamBREF, slugTeamBREF == 'CHO', 'CHA')) %>%
  rename(Team = slugTeamBREF, FullName = nameTeam)

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
  mutate('Win Percentage' = round(n / sum(n) * 100, 2)) %>%
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
  group_by(Team, Outcome) %>%
  distinct(GameID) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Outcome, values_from = n) %>%
  select(Team, W, L) %>%
  arrange(desc(W)) %>%
  left_join(injury_data_count) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  rename(Wins = W, Losses = L) %>%
  left_join(win_streak) %>%
  left_join(full_team_names) %>%
  mutate(WinPercentage = (Wins / (Wins + Losses)))

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
  arrange(Seed) %>%
  rename(Team = FullName)


team_points_histogram <- gameLogs_Two %>%
  group_by(Team, GameID, Outcome, Opponent) %>%
  summarise(Total_PTS = sum(PTS))

top_20pt_scorers <- gameLogs_Two %>%
  group_by(Player) %>%
  filter(mean(PTS) >= 20) %>%
  transmute(avg_PTS = mean(PTS), season_ts_percent, MVPCalc, Team) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(MVPCalc)) %>%
  mutate(Rank = row_number(),
         Top5 = case_when(Rank <= 5 ~ 'Top 5 MVP Candidates',
                          TRUE ~ 'Other'))

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

# old
# top20_plot <- top_20pt_scorers %>%
#   ggplot(aes(avg_PTS, season_ts_percent, color = Top5)) +
#   geom_point(size = 6, alpha = 1, pch = 21, color = 'black', aes(fill = Top5)) +
#   geom_label_repel(data = subset(top_20pt_scorers, Top5 == 'Top 5'), aes(label = Player), nudge_y = -0.008, color = 'black') +
#   scale_y_continuous(labels = scales::percent, limits=c(.52, .68), breaks=seq(.52, .68, by = .04)) + 
#   scale_color_manual(values = c('light blue', 'orange')) +
#   scale_fill_manual(values = c('light blue', 'orange')) +
#   labs(color = 'Top 5 MVP Candidates', fill = 'Top 5 MVP Candidates',
#        title = 'Player Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
#        x = 'Average Points per Game',
#        y = 'True Shooting Percentage') +
#   theme_jacob() +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')

top20_plot <- top_20pt_scorers %>%
  ggplot(aes(avg_PTS, season_ts_percent, color = Top5, text = paste(Player, '<br>',
                                                                    Team, '<br>',
                                                             'PPG: ', round(avg_PTS, 1), '<br>',
                                                             'TS%: ', round(season_ts_percent, 3)))) +
  geom_point(size = 6, alpha = 0.7, pch = 21, color = 'black', aes(fill = Top5)) +
  scale_y_continuous(labels = scales::percent, limits=c(.52, .68), breaks=seq(.52, .68, by = .04)) + 
  scale_color_manual(values = c('light blue', 'orange')) +
  scale_fill_manual(values = c('light blue', 'orange')) +
  labs(color = 'Top 5 MVP Candidates', fill = 'Top 5 MVP Candidates',
       title = 'Player Efficiency Tracker \n PPG vs TS% for all 20+ PPG Scorers',
       x = 'Average Points per Game',
       y = 'True Shooting Percentage') +
  theme_jacob() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')


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
team_choices <- unique(team_wins$Team)

  
team_mov <- gameLogs_Two %>%
  group_by(Team, Date, Outcome, Opponent) %>%
  summarise(Margin_of_Victory = sum(PlusMinus) / 5,
            team_pts = sum(PTS)) %>%
  ungroup() %>%
  mutate(Opp_PTS = team_pts - Margin_of_Victory) %>%
  left_join(full_team_names)


value_plot <- function(df){
  
  p <- df %>%
    filter(GP > 20) %>%
    ggplot(aes(Salary, MVPCalc, fill = GV)) +
    geom_point(aes(text = paste0(Player, '<br>',
                                 'Salary: ', formatC(Salary, format = 'f', big.mark = ",", digits = 0), '<br>',
                                 'MVP Metric: ', round(MVPCalc, 2), '<br>',
                                 'GP: ', GP)), size = 2.5, shape = 21, alpha = 0.7) +
    scale_x_continuous(labels = label_dollar()) +
    theme_jacob() +
    labs(y = 'Custom Player Value Metric',
         title = 'Who are the best & worst contract values in the 2019-2020 NBA Season ?',
         fill = 'Color Legend') +
    scale_fill_manual(values=c("green", "grey70", "red", 'purple'))
  
  ggplotly(p, tooltip = c('text'))
}


mov_plot <- function(df){
  p <- df %>%
    ggplot(aes(Date, Margin_of_Victory)) +
    geom_col(alpha = 0.7, aes(fill = Outcome, text = paste(Date, '<br>',
                                                                            Outcome, ' vs', Opponent, '<br>',
                                                                            'Scoreline: ', team_pts, ' - ', Opp_PTS, '<br>',
                                                                            'Margin of Victory: ', Margin_of_Victory))) +
    scale_y_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25)) +
    scale_fill_manual(values = c("red", "dark green")) +
    labs(x = NULL,
         y = 'Margin of Victory',
         title = paste0(df$FullName[1], ' Game Log History \n 2019-2020 NBA Season'),
         subtitle = '2019-2020 NBA Season') +
    theme_jacob()
  
  ggplotly(p, tooltip = c('text'))
  
}

mov_plot_backup <- function(df){
  p <- df %>%
    ggplot(aes(Date, Margin_of_Victory)) +
    geom_col(color = 'black', alpha = 0.7, aes(fill = Outcome)) +
    geom_smooth(method = 'loess', se = FALSE, color = 'grey20', alpha = 0.4) +
    scale_y_continuous(breaks = c(-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25)) +
    scale_fill_manual(values = c("red", "dark green")) +
    labs(x = NULL,
         y = 'Margin of Victory',
         title = paste0(df$FullName, ' Game Log History \n 2019-2020 NBA Season'),
         subtitle = '2019-2020 NBA Season') +
    theme_jacob()
  
  ggplotly(p)
  
}

schedule_main <- schedule %>%
  select(date_game, game_start_time, visitor_team_name, visitor_pts, home_team_name) %>%
  rename(Date = date_game, `Start Time (EST)` = game_start_time, `Team 1` = visitor_team_name, Vs = visitor_pts,
         `Team 2` = home_team_name) %>%
  mutate(Vs = replace_na(Vs, 'Vs'),
         Date = as.Date(Date, format = '%a, %b %d, %Y'),
         `Day of Week` = wday(Date, label = TRUE, abbr = FALSE),
         `Start Time (EST)` = str_replace_all(`Start Time (EST)`, 'p', ' PM')) %>%
  select(Date, `Day of Week`, `Start Time (EST)`, `Team 1`, Vs, `Team 2`) %>%
  filter(Date >= todayDate)

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

schedule_prac <- schedule_main %>%
  mutate(game_id = row_number()) %>%
  pivot_longer(cols = starts_with('Team'),
               names_to = 'Teambb',
               values_to = 'value') %>%
  select(-Teambb) %>%
  rename(Team = value) %>%
  select(Team, game_id) %>%
  left_join(opponent_prac)

bubble_ranks <- schedule_prac %>%
  pivot_longer(cols = starts_with('opponent'),
               names_to = 'team',
               values_to = 'valuebb') %>%
  filter(Team != valuebb) %>%
  select(-team) %>%
  rename(Opponent = valuebb) %>%
  left_join(team_rank) %>%
  left_join(opponent_rank)

bubble_analysis <- bubble_ranks %>%
  group_by(Team) %>%
  summarise(avg_opp_rank = mean(opp_Rank)) %>%
  left_join(team_rank) %>%
  mutate(differential = avg_opp_rank - Rank,
         Team = fct_reorder(Team, avg_opp_rank))

cc <- bubble_analysis %>%
  ggplot(aes(avg_opp_rank, Team, fill = differential > 0)) +
  geom_col(aes(text = paste0('Team: ', Team, '<br>',
                             'Team Rank: ', Rank, '<br>',
                             'Average Opponent Rank: ', avg_opp_rank))) +
  labs(y = NULL,
       x = 'Average Opponent Rank',
       title = 'Which Teams are getting the Easiest Schedule in the Orlando Bubble ?') +
  theme_jacob() +
  theme(legend.position='none')

# bubble team data
gameLogs_bubble <- gameLogs_Two %>%
  filter(Date >= as.Date('2020-07-29')) %>%
  group_by(Team, Outcome) %>%
  distinct(GameID) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = Outcome, values_from = n) %>%
  select(Team, W, L) %>%
  arrange(desc(W)) %>%
  left_join(injury_data_count) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  rename(Wins = W, Losses = L) %>%
  left_join(win_streak) %>%
  left_join(full_team_names) %>%
  mutate(WinPercentage = (Wins / (Wins + Losses)))

bubble_gp <- gameLogs_Two %>%
  filter(Date >= as.Date('2020-07-29')) %>%
  group_by(Team, GameID) %>%
  distinct(GameID) %>%
  ungroup() %>%
  group_by(Team) %>%
  summarise(GP = n())

threePointShooting_Pre_Bubble <- gameLogs_Two %>%
  filter(Date <= as.Date('2020-07-29')) %>%
  group_by(Team) %>%
  summarise(total_threes_attempted_pre = sum(threePAttempted),
            total_threes_made_pre = sum(threePFGMade)) %>%
  ungroup() %>%
  mutate(threePointPercentage_pre = (total_threes_made_pre / total_threes_attempted_pre),
         threePointPercentage_pre = round(threePointPercentage_pre, 4))



threePointShooting_Bubble <- gameLogs_Two %>%
  filter(Date >= as.Date('2020-07-29')) %>%
  group_by(Team) %>%
  summarise(total_threes_attempted = sum(threePAttempted),
            total_threes_made = sum(threePFGMade)) %>%
  ungroup() %>%
  mutate(threePointPercentage = (total_threes_made / total_threes_attempted),
         threePointPercentage = round(threePointPercentage, 4)) %>%
  left_join(threePointShooting_Pre_Bubble) %>%
  mutate(threeP_Percent_Differential = threePointPercentage - threePointPercentage_pre) %>%
  left_join(bubble_gp) %>%
  mutate(Team = fct_reorder(Team, threeP_Percent_Differential))


# league average has gone up by 0.2%, basically no meaningful change.
dd <- gameLogs_Two %>%
  filter(Date <= as.Date('2020-07-29')) %>%
  summarise(total_threes_attempted = sum(threePAttempted),
            total_threes_made = sum(threePFGMade)) %>%
  ungroup()

threebb <- threePointShooting_Bubble %>%
  select(Team, threeP_Percent_Differential)

ee <- gameLogs_bubble %>%
  left_join(threebb) %>%
  mutate(Shooting_Change = case_when(threeP_Percent_Differential > 0 ~ 'Better Shooting',
                        TRUE ~ 'Worse Shooting'))
bubble_shooting_percentages <- ee %>%
  group_by(Shooting_Change) %>%
  summarise(total_wins = sum(Wins),
            total_losses = sum(Losses)) %>%
  ungroup() %>%
  mutate(WinPercentage = (total_wins / (total_wins + total_losses)),
         WinPercentage = round(WinPercentage, 4))

bubble_bans <- bubble_shooting_percentages %>%
  select(WinPercentage) %>%
  mutate(WinPercentage = WinPercentage * 100,
         WinPercentage = round(WinPercentage, 2))

plot_bubble_shooting <- function(df) {
  p <- df %>%
    ggplot(aes(threeP_Percent_Differential, Team, fill = threeP_Percent_Differential > 0 )) +
    geom_col(aes(text = paste0(Team, '<br>', 
                               'Pre Bubble 3P%: ', threePointPercentage_pre * 100, '%', '<br>',
                               'Bubble 3P%: ', threePointPercentage * 100, '%', '<br>',
                               'Differential: ', round(threeP_Percent_Differential * 100, 2), '%'))) +
    scale_x_continuous(labels = percent_format()) +
    annotate('text', x = -0.03, y = 19.5, label = paste0('Win Percentage: ', bubble_bans[1,],'%'), color = 'deepskyblue3') +
    annotate('text', x = 0.03, y = 3.5, label = paste0('Win Percentage: ', bubble_bans[2,],'%'), color = 'firebrick2') +
    labs(title = 'Which Teams are Hot from 3 Point Range in the Orlando Bubble ?',
         x = '3P% Differential (Pre-Bubble vs Bubble)',
         y = NULL) +
    theme_jacob() +
    theme(legend.position='none')
  
  ggplotly(p, tooltip = c('text'))
}
