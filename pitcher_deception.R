# Jeanine Minnick
# creating a leaderboard of MLB's most deceptive pitchers

# use distance from average release point, spin rate, strike rates both swinging and looking (except on 3-0), and hit rate on strikes thrown

library(dplyr)
library(data.table)
library(grDevices)
library(geometry)

# using data from 2017-October 12 2020
savant <- read.csv("/Volumes/My Passport/Savant Data 2017-2020 season/savant101320.csv")
savant$player_name <- as.character(savant$player_name)
savant <- savant %>% filter(game_year == '2019' | game_year == '2020')

# quantify "deception" value

# weight these release point distances to average for each pitcher
# will want to factor each part of overall formula (what parts are more important to deception)
# weights are based on relative values of the actions with respect to deception

# swingingk_pct - want higher
# lookingk_pct - want higher
# rel_var_dbd   1/rel_var_dbd - want lower
# hit_pct  1/hit_pct - want lower
# speed_var - want higher  
# bauer_avg - want higher
# break_area - want higher

# deception <- swingingk_pct + lookingk_pct + (1/rel_var_dbd) + (1/hit_pct) + speed_var + bauer_avg + break_area

#################
#################
# all savant data
# pitchers with at least 500 pitches from 2019 & 2020
pitch_count <- as.data.frame(savant %>% group_by(pitcher) %>% tally()) # count total pitches for each pitcher id
id_list <- (pitch_count %>% filter(n >= 500))[,1] # filter out players without 500 total pitches, get this as a player list

deception_mat <- matrix(data = NA, nrow = length(id_list), ncol = 10) # create a matrix for the calculated values to be placed
deception_mat <- as.data.frame(deception_mat)
colnames(deception_mat) <- c("player_id", "player_name", "deception", "swingingk_pct", "lookingk_pct", "rel_var_dbd_inv", "hit_pct_inv", "speed_var", "bauer_avg", "break_area")

# overall loop for calculating values for each individual pitcher
for (i in 1:length(id_list)) {
  deception_mat[i,1] <- id_list[i] # apply pitcher id to deception table
  
  # filter data for player
  player_data <- savant %>% filter(pitcher == id_list[i])
  
  deception_mat[i,2] <- player_data$player_name[1] # apply pitcher name to deception table
  
  # calculate swinging k percent
  swingingk_pct <- sum(grepl("swinging_strike", player_data$description, fixed = TRUE))/nrow(player_data)
  deception_mat[i,4] <- swingingk_pct # apply pitcher swinging k percent to deception table
  
  # calculate looking k percent - exclude pitches on 3-0 counts
  player_data_no30 <- player_data %>% filter(!(balls == 3 & strikes == 0))
  lookingk_pct <- sum(grepl("called_strike", player_data_no30$description, fixed = TRUE))/nrow(player_data_no30)
  deception_mat[i,5] <- lookingk_pct # apply pitcher looking k percent to deception table
  
  # calculate release variability on a day-by-day basis - likely to make small mechanical changes, mound starting point, etc. on a gam-by-game basis
  date_list <- unique(as.character(player_data$game_date)) # want individual dates the pitcher threw on
  
  # run loop for each date
  for (k in 1:length(date_list)) {
    avg_rel_x <- mean(player_data$release_pos_x[player_data$game_date == date_list[k]], na.rm = TRUE) # average release point (x)
    avg_rel_z <- mean(player_data$release_pos_z[player_data$game_date == date_list[k]], na.rm = TRUE) # average release point (z)
    
    # calculate the distance from average release point for each pitch
    for (j in 1:nrow(player_data)) {
      if (player_data$game_date[j] == date_list[k]) {
        player_data$dist_from_rel_center[j] <- sqrt((player_data$release_pos_x[j] - avg_rel_x)^2 + (player_data$release_pos_z[j] - avg_rel_z)^2)
      }
    }
  }
  
  rel_var_dbd <- var(player_data$dist_from_rel_center, na.rm = TRUE) # variance of distance from release point center
  deception_mat[i,6] <- 1/rel_var_dbd # apply release point variance to deception table
  
  # hit percent for balls contacted
  hit_pct <- sum(player_data$events %like% "home_run|single|double|triple")/sum(player_data$description %like% "hit_into_play|foul")
  deception_mat[i,7] <- 1/hit_pct # apply hit percent to deception table
  
  # speed differences (higher differences means greater variability in pitch speed)
  speed_var <- var(player_data$release_speed, na.rm = TRUE) 
  deception_mat[i,8] <- speed_var # apply speed variance to deception table
  
  # fastball Bauer Units 
  for (b in 1:nrow(player_data)) {
    if (player_data$pitch_type[b] == "FF" | player_data$pitch_type[b] == "FT" | player_data$pitch_type[b] == "FC") {
      player_data$bauer_unit[b] <- player_data$release_spin_rate[b]/player_data$release_speed[b]
    }
    else {player_data$bauer_unit[b] <- NA}
  }
  
  # for a small number of players, they did not throw one of the pitch types above, but did throw a slider - used to calculate in these instances
  if (sum(!is.na(player_data$bauer_unit)) == 0) {
    for (b in 1:nrow(player_data)) {
      if (player_data$pitch_type[b] == "SI") {
        player_data$bauer_unit[b] <- player_data$release_spin_rate[b]/player_data$release_speed[b]
      }
      else {player_data$bauer_unit[b] <- NA}
    }
  }
  
  bauer_avg <- mean(player_data$bauer_unit, na.rm = TRUE)
  deception_mat[i,9] <- bauer_avg # apply bauer unit average to deception table
  
  # use ax az for break - breaking pitches have higher numbers
  # want larger area within triangle - larger break range means more deception
  accel_x <- player_data %>% filter(pitch_type != '') %>% group_by(pitch_type) %>% summarise(avg_ax = mean(ax)) %>% pull(2) # calculate average ax for each pitch type
  accel_z <- player_data %>% filter(pitch_type != '') %>% group_by(pitch_type) %>% summarise(avg_ax = mean(az)) %>% pull(2) # calculate average az for each pitch type
  
  # calculate pitch acceleration area if pitcher has more than 2 pitches
  if (length(accel_x) > 2) {
    point_order <- chull(accel_x, accel_z)
    accel_x <- accel_x[point_order]
    accel_z <- accel_z[point_order]
    break_area <- polyarea(accel_x, accel_z)
    deception_mat[i,10] <- break_area
  } else{deception_mat[i,10] <- sqrt((accel_x[1] - accel_x[2])^2 + (accel_z[1] - accel_z[2])^2)} # <- calculate distance from pitches if only two pitch types
  
  
  # calculate deception and apply to table
  deception <- swingingk_pct + lookingk_pct + (1/rel_var_dbd) + (1/hit_pct) + speed_var + bauer_avg + break_area
  deception_mat[i,3] <- deception
}



# calculate average of each part of deception equation - we will make all values equal in our definition

# each needs to equal 14.28571 to make average 100
mean(deception_mat$swingingk_pct)*126.0255
mean(deception_mat$lookingk_pct)*89.09515
mean(deception_mat$rel_var_dbd_inv)/10.588821
mean(deception_mat$hit_pct_inv)*2.422145
mean(deception_mat$speed_var)/1.912593
mean(deception_mat$bauer_avg)/1.718192
mean(deception_mat$break_area)/10.51374

# copy deception table to another (final) table
deception_final <- deception_mat

# final formula
deception_final$deception <- deception_final$swingingk_pct*126.0255 + deception_final$lookingk_pct*89.09515 +
  deception_final$rel_var_dbd_inv/10.588821 + deception_final$hit_pct_inv*2.422145 + deception_final$speed_var/1.912593 +
  deception_final$bauer_avg/1.718192 + deception_final$break_area/10.51374




