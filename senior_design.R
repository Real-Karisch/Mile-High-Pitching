
library(pitchRx)
library(dplyr)


#### general linear mixed effect models; propensity score analysis/matching

"2015_0[4-9]_[0-9]{2}_col_[a-z]{6}"
home <- "2015_0[4-9]_[0-9]{2}_[a-z]{6}_colmlb"
all <- "2015_0[4-9]_[0-9]{2}_[a-z]{6}_[a-z]{6}"

reg.all <- gids[grepl(all, gids)]
reg.away <- gids[grepl("2015_0[4-9]_[0-9]{2}_colmlb_[a-z]{6}", gids)]
reg.home <- gids[grepl(home, gids)]

all_15 <- scrape(game.ids = reg.all)
all_pitch_15 <- all_15$pitch

all_pitch_15 <- as.data.frame(matrix(nrow = 0, ncol = 49))
for (i in 1:13 * 5 - 2) {
  new <- as.data.frame(all_15[i])
  if (ncol(new) < 49) {
    diff <- 49 - ncol(new)
    new[45-diff:49] <- matrix(data = 0, nrow = nrow(new), ncol = diff)
  }
  all_pitch_15 <- rbind(all_pitch_15, new[1:49])
}

col_home_15 <- scrape(game.ids = reg.home)
col_away_15 <- scrape(game.ids = reg.away)

col_home_pitch_15 <- col_home_15$pitch
col_away_pitch_15 <- col_away_15$pitch

col_home_pitch_15 <- subset(col_home_pitch_15, inning_side == "top")
col_away_pitch_15 <- subset(col_away_pitch_15, inning_side == "bottom")

#### barplot ####

par(mfrow=c(1,2))

total_pitches_col_home <- subset(col_home_pitch_15, !is.na(pitch_type))
total_pitches_col_away <- subset(col_away_pitch_15, !is.na(pitch_type))

pitch_class <- function (dat, vect) {
  pitch_class <- vector(length = length(vect))
  i <- 1
  for (i in 1:length(vect)) {
    if (vect[i] == "FF" ||
        vect[i] == "FT" ||
        vect[i] == "FS" ||
        vect[i] == "FA" ||
        vect[i] == "FC") {
      pitch_class[i] <- "fastball"
    } else if (vect[i] == "CU" ||
               vect[i] == "KC") {
      pitch_class[i] <- "curve"
    } else if (vect[i] == "SI") {
      pitch_class[i] <- "sinker"
    }
    else if (vect[i] == "SL") {
      pitch_class[i] <- "slider"
    }
    else if (vect[i] == "CH") {
      pitch_class[i] <- "change"
    }
    else {
      pitch_class[i] <- "other"
    }
    i <- i + 1
  }
  dat$pitch_class <- pitch_class
  return(dat)
}

type_away <- pitch_class(total_pitches_col_away, total_pitches_col_away$pitch_type)
type_home <- pitch_class(total_pitches_col_home, total_pitches_col_home$pitch_type)

fast_away <- nrow(subset(type_away, pitch_class == "fastball"))
curve_away <- nrow(subset(type_away, pitch_class == "curve"))
slider_away <- nrow(subset(type_away, pitch_class == "slider"))
sinker_away <- nrow(subset(type_away, pitch_class == "sinker"))
change_away <- nrow(subset(type_away, pitch_class == "change"))
other_away <- nrow(subset(type_away, pitch_class == "other"))

y <- c(fast_away,curve_away,slider_away,sinker_away,change_away,other_away)

coords <- as.matrix(rbind(x,y))

barplot(coords, beside = TRUE, legend.text = c("Home","Away"),
        names.arg = c("Fastball","Curve","Slider","Sinker","Change","Other"),
        main = "Rockies Pitch Selection for 2015 Season")
#### chi squared ####

fast_away_percent <- nrow(subset(type_away, pitch_class == "fastball")) / nrow(type_away)
curve_away_percent <- nrow(subset(type_away, pitch_class == "curve")) / nrow(type_away)
slider_away_percent <- nrow(subset(type_away, pitch_class == "slider")) / nrow(type_away)
sinker_away_percent <- nrow(subset(type_away, pitch_class == "sinker")) / nrow(type_away)
change_away_percent <- nrow(subset(type_away, pitch_class == "change")) / nrow(type_away)
other_away_percent <- nrow(subset(type_away, pitch_class == "other")) / nrow(type_away)

fast_home <- nrow(subset(type_home, pitch_class == "fastball"))
curve_home <- nrow(subset(type_home, pitch_class == "curve"))
slider_home <- nrow(subset(type_home, pitch_class == "slider"))
sinker_home <- nrow(subset(type_home, pitch_class == "sinker"))
change_home <- nrow(subset(type_home, pitch_class == "change"))
other_home <- nrow(subset(type_home, pitch_class == "other"))

x <- c(fast_home,curve_home,slider_home,sinker_home,change_home,other_home)
p <- c(fast_away_percent,curve_away_percent,slider_away_percent,sinker_away_percent,change_away_percent,other_away_percent)
chisq.test(x, p = p)

#### ####

col_home_15_ff <- subset(type_home, pitch_class == "fastball")
col_away_15_ff <- subset(type_away, pitch_class == "fastball")

ff_percent_col_home <- nrow(col_home_15_ff) / nrow(total_pitches_col_home)
ff_percent_col_away <- nrow(col_away_15_ff) / nrow(total_pitches_col_away)

#total_break_home <- subset(col_home_pitch_15, !is.na(break_length))
#total_break_away <- subset(col_away_pitch_15, !is.na(break_length))

total_break_home <- subset(type_home, !is.na(break_length))
total_break_away <- subset(type_away, !is.na(break_length))

total_speed_home <- subset(type_home, !is.na(end_speed))
total_speed_away <- subset(type_away, !is.na(end_speed))

curve_home <- subset(type_home, pitch_class == "curve")
curve_away <- subset(type_away, pitch_class == "curve")

fast_speed_home <- subset(total_speed_home, pitch_class == "fastball")
fast_speed_away <- subset(total_speed_away, pitch_class == "fastball")

diff_speed_home <- fast_speed_home$start_speed - fast_speed_home$end_speed
diff_speed_away <- fast_speed_away$start_speed - fast_speed_away$end_speed

t.test(diff_speed_away, diff_speed_home, alternative = "less")

hist(diff_speed_away)

curve_home$break_length <- as.numeric(curve_home$break_length)
curve_away$break_length <- as.numeric(curve_away$break_length)

mean_break_curve_home <- mean(curve_home$break_length)
mean_break_curve_away <- mean(curve_away$break_length)

t.test(curve_home$break_length,curve_away$break_length, alternative = "less")

total_spin_break_home <- subset(total_break_home, !is.na(spin_rate))
total_spin_break_away <- subset(total_break_away, !is.na(spin_rate))

curve_spin_break_home <- subset(total_spin_break_home, pitch_class == "curve")
curve_spin_break_away <- subset(total_spin_break_away, pitch_class == "curve")

spin_break_lm_home <- lm(curve_spin_break_home$break_length ~ curve_spin_break_home$spin_rate)
spin_break_lm_away <- lm(curve_spin_break_away$break_length ~ curve_spin_break_away$spin_rate)

plot(curve_spin_break_home$break_length ~ curve_spin_break_home$spin_rate)
abline(spin_break_lm_home)
summary(spin_break_lm_home)

plot(curve_spin_break_away$break_length ~ curve_spin_break_away$spin_rate)
abline(spin_break_lm_away)
summary(spin_break_lm_away)

hist(total_spin_break_away$spin_rate)
hist(total_spin_break_home$spin_rate)

merged_away <- merge(col_away_15$atbat, col_away_15$pitch, by = "event_num")
merged_home <- merge(col_away_15$atbat, col_away_15$pitch, by = "event_num")
merged_away <- join(col_away_15$atbat, col_away_15$pitch, by = "event_num")
