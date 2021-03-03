library(tidyverse)

#turning off warnings
options(warn=-1)

##reading in non-tracking data

#includes schedule info for games
df_games <- read_csv("../input/nfl-big-data-bowl-2021/games.csv",
                     col_types = cols()) %>% 
  rename(game_id = gameId, game_date = gameDate, game_time_eastern = gameTimeEastern, home_team_abbr = homeTeamAbbr, visitor_team_abbr = visitorTeamAbbr)

#includes play-by-play info on specific plays
df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv",
                     col_types = cols()) %>% 
  rename(game_id = gameId, play_id = playId, play_description = playDescription, yards_to_go = yardsToGo, possession_team = possessionTeam, play_type = playType,  yardline_side = yardlineSide, yardline_number = yardlineNumber, offense_formation = offenseFormation, defenders_in_the_box = defendersInTheBox, number_of_pass_rushers = numberOfPassRushers, type_dropback = typeDropback, presnap_visitor_score = preSnapVisitorScore, presnap_home_score = preSnapHomeScore, game_clock = gameClock, absolute_yardline_number = absoluteYardlineNumber, penalty_codes = penaltyCodes, penalty_jersey_numbers = penaltyJerseyNumbers, pass_result = passResult, offense_play_result = offensePlayResult, play_result = playResult, is_defensive_pi = isDefensivePI)

#includes background info for players
df_players <- read_csv("../input/nfl-big-data-bowl-2021/players.csv",
                       col_types = cols()) %>% 
  rename(nfl_id = nflId, birth_date = birthDate, college_name = collegeName, display_name = displayName)

#includes targeted receiver by play
df_targetedReceiver <- read_csv("../input/nfl-big-data-bowl-2021-bonus/targetedReceiver.csv",
                                col_types = cols()) %>% 
  rename(game_id = gameId, play_id = playId, target_nfl_id = targetNflId)

#read in week 1 coverage data
df_coverages_week1 <- read_csv("../input/nfl-big-data-bowl-2021-bonus/coverages_week1.csv",
                               col_types = cols()) %>% 
  rename(game_id = gameId, play_id = playId)

##Reading tracking data (needs to be done iteratively)

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("../input/nfl-big-data-bowl-2021/week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

#Rename tracking data columns
df_tracking <- df_tracking %>%
  rename(nfl_id = nflId, display_name = displayName, jersey_number = jerseyNumber, frame_id = frameId, game_id = gameId, play_id = playId, play_direction = playDirection)

#Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
df_tracking <- df_tracking %>%
  mutate(x = ifelse(play_direction == "left", 120-x, x),
         y = ifelse(play_direction == "left", 160/3 - y, y))

#direction and orientation
df_tracking <- df_tracking %>%
  mutate(dir = case_when(play_direction == "left" & dir > 180 ~ dir - 180,
                         play_direction == "left" & dir < 90 ~ dir + 180,
                         play_direction == "left" & dir >= 90 & dir <= 180 ~ dir + 180,
                         TRUE ~ dir)) %>%
  mutate(o = case_when(play_direction == "left" & o > 180 ~ o - 180,
                       play_direction == "left" & o < 90 ~ o + 180,
                       play_direction == "left" & o >= 90 & o <= 180 ~ o + 180,
                       TRUE ~ o))

#read in nflfastR data to identify QB that passed the football on plays where there's more than one QB on the field
pbp_18 <- read_csv("../input/nflfastr/play_by_play_2018.csv",
                   col_types = cols())

passer_18 <- pbp_18 %>% #filter(!is.na(passer_player_name)) %>% 
  mutate(old_game_id = as.double(old_game_id),
         play_id = as.double(play_id)) %>% 
  select(game_id = old_game_id, play_id, passer_player_name, passer_jersey_number)

#defining events that designate pass thrown & pass arrival
pass_thrown_events <- c("pass_forward")
pass_arrival_events <- c('pass_outcome_caught',
                         'pass_arrived',
                         'pass_outcome_incomplete',
                         'pass_outcome_interception',
                         'pass_outcome_touchdown')
offense_positions <- c("QB", "RB", "TE", "WR", "FB", "HB", "LS", "K", "P")
defense_positions <- c("OLB", "CB", "ILB", "SS", "DB", "LB", "MLB", "FS", "S", "DT", "DE", "NT", "DL")

#add side of ball details to tracking dataframe
df_tracking <- df_tracking %>% mutate(side_of_ball = case_when(position %in% offense_positions ~ "offense",
                                                               position %in% defense_positions ~ "defense",
                                                               TRUE ~ as.character(position)))

#create defense data frame
def_team_df <- df_tracking %>% 
  filter(side_of_ball == "defense" & display_name != "Football") %>%
  select(game_id, play_id, frame_id, nflId_def = nfl_id, def_player_name = display_name, def_side = side_of_ball, event, x_def = x, y_def = y, s_def = s, a_def = a, dis_def = dis, o_def = o, dir_def = dir, def_jersey_number = jersey_number, def_position = position) %>% 
  distinct()

#create football data frame
football_df <- df_tracking %>% 
  filter(display_name == "Football") %>%
  select(game_id, play_id, frame_id, x_ball = x, y_ball = y, s_ball = s, a_ball = a, dis_ball = dis) 

#create offense (targeted receiver) data frame
off_team_df <- df_tracking %>% 
  filter(side_of_ball == "offense" & display_name != "Football") %>%
  left_join(df_targetedReceiver, by = c("game_id", "play_id")) %>% 
  mutate(targeted_rec = if_else(nfl_id == target_nfl_id, 1, 0)) %>% 
  filter(targeted_rec == 1) %>% 
  select(game_id, play_id, frame_id, nflId_off = nfl_id, off_player_name = display_name, off_side = side_of_ball, x_off = x, y_off = y, s_off = s, a_off = a, dis_off = dis, o_off = o, dir_off = dir, targeted_rec, route, off_jersey_number = jersey_number, off_position = position) %>% 
  distinct()

#create qb data frame. joined with nflfastR data to identify passing QB and removed duplicate rows
qb_df <- df_tracking %>% 
  filter(position == "QB") %>% 
  left_join(passer_18, by = c("game_id", "play_id")) %>% 
  group_by(game_id, play_id, frame_id) %>% 
  mutate(n_name = n()) %>%
  ungroup() %>%
  mutate(last_name = str_extract(display_name, '[^ ]+$')) %>%
  mutate(passer_flag = case_when(n_name > 1 & (jersey_number == passer_jersey_number | str_detect(passer_player_name, last_name)) ~ 1,
                                 n_name <= 1 ~ 1,
                                 TRUE ~ 0)) %>% 
  filter(passer_flag == 1) %>% 
  select(game_id, play_id, frame_id, nflId_qb = nfl_id, qb_name = display_name, x_qb = x, y_qb = y, s_qb = s, a_qb = a, dis_qb = dis, o_qb = o, dir_qb = dir, qb_jersey_number = jersey_number) %>% 
  distinct()

#create receiver route data frame - players with at least 100 routes
rec_routes_df <- df_tracking %>% 
  select(game_id, play_id, frame_id, nflId_off = nfl_id, off_player_name = display_name, off_side = side_of_ball, x_off = x, y_off = y, s_off = s, a_off = a, dis_off = dis, o_off = o, dir_off = dir, route, off_jersey_number = jersey_number, off_position = position) %>% 
  filter(off_side == "offense" & off_player_name != "Football") %>%
  mutate(route = if_else(is.na(route) | route == "undefined", "UNDEFINED", route)) %>% 
  group_by(nflId_off, off_player_name, off_position, route) %>% 
  summarize(n_play = n_distinct(play_id, game_id)) %>% 
  filter(sum(n_play)>100 & off_position == 'WR') %>% 
  group_by(nflId_off, off_player_name, off_position) %>% 
  mutate(percentage = n_play / sum(n_play))

#adjust the frame_ids for the two plays where the football's tracking event frame_ids do not match the event frame_ids of the players
football_df <- football_df %>% 
  mutate(frame_id = if_else(game_id == '2018092304' & play_id == '1687', frame_id + 8, frame_id)) %>% #Titans-Jaguars 9/23
  mutate(frame_id = if_else(game_id == '2018101410' & play_id == '431', frame_id + 6, frame_id)) #Cowboys-Jaguars 10/14

#calculate distance defensive player is from targeted receiver, football, and QB
distance_to_opp_df <- def_team_df %>% 
  left_join(off_team_df, by =  c("game_id", "play_id", "frame_id")) %>% 
  left_join(football_df, by =  c("game_id", "play_id", "frame_id")) %>% 
  left_join(qb_df, by =  c("game_id", "play_id", "frame_id")) %>% 
  mutate(distance_from_opponent = sqrt((x_def - x_off)^2 + (y_def - y_off)^2),
         def_dist_to_football = sqrt((x_def - x_ball)^2 + (y_def - y_ball)^2),
         off_dist_to_football = sqrt((x_off - x_ball)^2 + (y_off - y_ball)^2),
         def_dist_to_qb = sqrt((x_def - x_qb)^2 + (y_def - y_qb)^2))

#adjust frame_ids for Tony Jefferson vs. Broncos on 9/23 where Tony's frame_ids don't align with rest of defense
distance_to_opp_df <- distance_to_opp_df %>% 
  group_by(game_id, play_id, event) %>% 
  mutate(frame_id = if_else(event != "None" & is.na(o_def), names(which.max(table(frame_id))),as.character(frame_id))) %>%
  mutate(frame_id = as.numeric(frame_id)) %>% 
  ungroup()

#remove play where there's no football data in Bears - Dolphins game from 10/14/18
distance_to_opp_df <- distance_to_opp_df %>% filter(game_id != '2018101404' & play_id != '2003')

#remove data frames no longer needed
rm(off_team_df, def_team_df, football_df, qb_df, passer_18)

#create master dataframe. creates features to analyze for xComp model
df_master <- suppressWarnings(distance_to_opp_df %>%
                                group_by(game_id, play_id) %>%
                                #filter plays that have pass thrown & pass arrival events and targeted receiver
                                filter(is.finite(min(frame_id[event %in% pass_arrival_events])) & is.finite(min(frame_id[event %in% pass_thrown_events])) & !is.na(targeted_rec)) %>% 
                                summarize(pass_thrown_frame = min(frame_id[event %in% pass_thrown_events]),
                                          pass_arrival_frame = min(frame_id[event %in% pass_arrival_events]),
                                          receiver_separation_at_arrival = min(distance_from_opponent[frame_id == pass_arrival_frame]),
                                          receiver_separation_at_throw = min(distance_from_opponent[frame_id == pass_thrown_frame]),
                                          def_at_pass_arrival = nflId_def[frame_id == pass_arrival_frame & distance_from_opponent == receiver_separation_at_arrival],
                                          def_at_pass_arrival_name = def_player_name[frame_id == pass_arrival_frame & distance_from_opponent == receiver_separation_at_arrival],
                                          def_at_pass_arrival_number = def_jersey_number[frame_id == pass_arrival_frame & distance_from_opponent == receiver_separation_at_arrival],
                                          def_pass_arrival_position = def_position[frame_id == pass_arrival_frame & distance_from_opponent == receiver_separation_at_arrival],
                                          targeted_receiver = nflId_off[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival],
                                          targeted_receiver_name = off_player_name[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival],
                                          targeted_receiver_number = off_jersey_number[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival],
                                          targeted_receiver_position = off_position[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival],
                                          air_distance_qb_throw_rec_catch = sqrt((x_qb[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival] - x_off[frame_id == pass_arrival_frame & nflId_def ==  def_at_pass_arrival])^2 + (y_qb[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival] - y_off[frame_id == pass_arrival_frame & nflId_def ==  def_at_pass_arrival])^2),
                                          route = route[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival],
                                          sec_to_throw = (pass_thrown_frame - min(frame_id[event == 'ball_snap'])) / 10, #each frame is about 1/10 of a sec
                                          sec_from_throw_to_arrival = ((pass_arrival_frame -  pass_thrown_frame)/ 10), 
                                          rec_to_sideline_dist_at_arrival = min(min(y_off[frame_id == pass_arrival_frame]) - 0, 160/3 - min(y_off[frame_id == pass_arrival_frame])),
                                          qb_dist_to_nearest_def_at_throw = min(def_dist_to_qb[frame_id == pass_thrown_frame]),
                                          qb_mph_at_throw = round(s_qb[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival]*(3600/1760),2),
                                          rec_mph_at_throw = round(min(s_off[frame_id == pass_thrown_frame])*(3600/1760),2),
                                          def_mph_at_throw = round(s_def[frame_id == pass_thrown_frame & nflId_def ==  def_at_pass_arrival]*(3600/1760),2),
                                          rec_mph_at_arrival = round(min(s_off[frame_id == pass_arrival_frame])*(3600/1760),2),
                                          def_mph_at_arrival = round(s_def[frame_id == pass_arrival_frame & nflId_def ==  def_at_pass_arrival]*(3600/1760),2),
                                          rec_dist_traveled_throw_arrival = sum(dis_off[frame_id >= pass_thrown_frame & frame_id <= pass_arrival_frame & nflId_def ==  def_at_pass_arrival]),
                                          def_dist_traveled_throw_arrival = sum(dis_def[frame_id >= pass_thrown_frame & frame_id <= pass_arrival_frame & nflId_def ==  def_at_pass_arrival])))

#remove 12 plays with no QB listed. fake punts, fake FGs, etc.
df_master <- df_master %>% 
  filter(!is.na(qb_mph_at_throw))

#merge play details with master dataframe
df_master <- df_master %>% 
  group_by(game_id, play_id) %>% 
  left_join(df_plays, by = c("game_id", "play_id")) %>% 
  distinct() %>% 
  ungroup()

#merge home and away team information with master dataframe
df_master <- df_master %>% 
  left_join((df_games %>% select(game_id, home_team_abbr, visitor_team_abbr, week)), by = "game_id")

#create binary pass caught flag to use as the response variable in logistic regression xComp model 
df_master <- df_master %>% 
  mutate(pass_caught = if_else(pass_result == "C", 1, 0))

#add defensive position group to group specific positions together 
df_master <- df_master %>% 
  mutate(def_position_group = case_when(def_pass_arrival_position %in% c('CB', 'DB') ~ 'CB',
                                        def_pass_arrival_position %in% c('S', 'FS', 'SS') ~ 'S',
                                        def_pass_arrival_position %in% c('ILB', 'MLB', 'LB', 'OLB') ~ 'LB',
                                        TRUE ~ def_pass_arrival_position))

#Combine NA and undefined routes into single UNDEFINED category. Filter on secondary players only
df_master <- df_master %>% mutate(route = if_else(is.na(route) | route == "undefined", "UNDEFINED", route)) %>% 
  filter(def_position_group %in% c('CB', 'S'))

#bin air distance variable to create average separation values
df_master <- df_master %>%  mutate(air_distance_bin=cut(air_distance_qb_throw_rec_catch, breaks=c(-Inf, 0, 5, 10, 15, 20, Inf), labels=c("behind LOS", "0-5 yds","5-10 yds","10-15 yds","15-20 yds", "20+ yards")))

#create avg_separation dataframe for xComp predictions - mean separation - remove outliers > 2 sd away from the mean
avg_separation <- df_master %>% filter(!(abs(receiver_separation_at_arrival - mean(receiver_separation_at_arrival)) > 2*sd(receiver_separation_at_arrival))) %>% 
  group_by(route, air_distance_bin) %>% 
  summarize(avg_sep = mean(receiver_separation_at_arrival),
            med_sep = median(receiver_separation_at_arrival),
            n_play= n())

#boxplot showing distribution of separation by route
options(repr.plot.width = 16, repr.plot.height = 12)
df_master %>% 
  ggplot(aes(x = reorder(route, receiver_separation_at_arrival, mean), y = receiver_separation_at_arrival, color = route)) +
  geom_boxplot() +
  theme_minimal()+
  labs(x = element_blank(), y = 'Separation (yds)', title = 'Receiver Separation at Pass Arrival', subtitle = 'by Route\n2018 NFL Season', 
       caption = "Data: NFL Big Data Bowl 2021\nPlot: Mike Irene") +
  theme(legend.position = "none",plot.title=element_text(size = 20), plot.subtitle=element_text(size = 16, face = "italic"), plot.caption=element_text(size = 14), axis.text.x = element_text(size = 14), strip.text.x = element_text(size = 14, face = "bold"), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))

#boxplot showing distribution of separation by air distance bin
df_master %>% 
  mutate(air_distance_bin=cut(air_distance_qb_throw_rec_catch, breaks=c(-Inf, 0, 5, 10, 15, 20, Inf), labels=c("behind LOS", "0-5 yds","5-10 yds","10-15 yds","15-20 yds", "20+ yards"))) %>% 
  ggplot(aes(x = air_distance_bin, y = receiver_separation_at_arrival, color = air_distance_bin)) +
  geom_boxplot() +
  theme_minimal()+
  labs(x = element_blank(), y = 'Separation (yds)', title = 'Receiver Separation at Pass Arrival', subtitle = 'by Air Distance between QB at Pass Throw & Receiver at Pass Arrival\n2018 NFL Season', 
       caption = "Data: NFL Big Data Bowl 2021\nPlot: Mike Irene") +
  theme(legend.position = "none",plot.title=element_text(size = 20), plot.subtitle=element_text(size = 16, face = "italic"), plot.caption=element_text(size = 14), axis.text.x = element_text(size = 14), strip.text.x = element_text(size = 14, face = "bold"), axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 18)) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))

#create avg_separation dataframe for xComp predictions - mean separation - remove outliers > 2 sd away from the mean
avg_separation <- df_master %>% filter(!(abs(receiver_separation_at_arrival - mean(receiver_separation_at_arrival)) > 2*sd(receiver_separation_at_arrival))) %>% 
  group_by(route, air_distance_bin) %>% 
  summarize(avg_sep = mean(receiver_separation_at_arrival),
            med_sep = median(receiver_separation_at_arrival),
            n_play= n())

#join avg_separation data to master dataframe
df_master <- df_master %>%
  left_join(avg_separation, by = c('route', 'air_distance_bin'))

#xComp Model creation
#used natural log of receiver_separation_at_arrival to reduce variation caused by outliers
model.test <- glm(pass_caught ~  
                    log(receiver_separation_at_arrival) + qb_mph_at_throw  + sec_to_throw + qb_dist_to_nearest_def_at_throw + air_distance_qb_throw_rec_catch
                  , family = binomial(logit)
                  , data = df_master)
summary(model.test)

#create master dataframe using average or median separation by position group instead of actual separation
df_master_avg_sep <- df_master %>% mutate(receiver_separation_at_arrival = avg_sep)

# Predict using both actual separation and average separation. xComp saved comes from difference between actual and average separation 
predictions <- predict(model.test, df_master, type = "response")
predict_avg_separation <- predict(model.test, df_master_avg_sep, type = "response")
df_pred <- data.frame(df_master, predictions, predict_avg_separation)

#top players by position group
#CB
df_pred %>% 
  filter(def_position_group == 'CB') %>% 
  mutate(def_team = if_else(home_team_abbr == possession_team, visitor_team_abbr, home_team_abbr),
         xComp = predictions,
         xComp_avg_sep = predict_avg_separation,
         receiver_separation_at_arrival = receiver_separation_at_arrival,
         xCompSaved = xComp_avg_sep - xComp) %>% 
  group_by(def_at_pass_arrival_name, def_team) %>% 
  summarize(total_xComp_saved = round(sum(xCompSaved),2),
            n_play = n(),
            xComp_saved_per_play = round(sum(xCompSaved) / n(),3)) %>%
  filter(n_play >= 50) %>% 
  arrange(-xComp_saved_per_play) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  mutate(Rank = paste0(row_number()))

#Safeties
df_pred %>% 
  filter(def_position_group == 'S') %>% 
  mutate(def_team = if_else(home_team_abbr == possession_team, visitor_team_abbr, home_team_abbr),
         xComp = predictions,
         xComp_avg_sep = predict_avg_separation,
         receiver_separation_at_arrival = receiver_separation_at_arrival,
         xCompSaved = xComp_avg_sep - xComp) %>% 
  group_by(def_at_pass_arrival_name, def_team) %>% 
  summarize(total_xComp_saved = round(sum(xCompSaved),2),
            n_play = n(),
            xComp_saved_per_play = round(sum(xCompSaved) / n(),3)) %>%
  filter(n_play >= 50) %>% 
  arrange(-xComp_saved_per_play) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  mutate(Rank = paste0(row_number()))

#xCS per play by route. Shows where players are better / worse than average at limiting completions. Minimum 5 plays defended per route. B. McCain
df_pred %>% 
  filter(def_at_pass_arrival_name == 'Bobby McCain') %>%
  mutate(def_team = if_else(home_team_abbr == possession_team, visitor_team_abbr, home_team_abbr),
         xComp = predictions,
         xComp_avg_sep = predict_avg_separation,
         receiver_separation_at_arrival = round(receiver_separation_at_arrival,2),
         xCompSaved = xComp_avg_sep - xComp) %>% 
  group_by(def_at_pass_arrival_name, def_position_group, def_team, route) %>% 
  summarize(total_xComp_saved = round(sum(xCompSaved),4),
            n_play = n(),
            xComp_saved_per_play = round(sum(xCompSaved) / n(),3),
            act_comp_pct_allowed =  round(sum(pass_caught == 1)/n(),4),
            epa_allowed_per_play = sum(epa)/ n()) %>%
  filter(n_play >= 5) %>%
  ungroup() %>% 
  arrange(-xComp_saved_per_play) %>% 
  select(def_at_pass_arrival_name, Team = def_team, route, xComp_saved_per_play) %>% 
  spread(route, xComp_saved_per_play)

