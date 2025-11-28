library(tidyverse)
library(lubridate)
library(zoo)

set.seed(123)

### Create Calendar ###
days <- 112  # 16 weeks
date_seq <- seq(from = as.Date("2024-04-22"),  # NEW START DATE
                length.out = days, by = "day") 

### Assign daily sport type ###
sports <- c("Rest", "Swim", "Bike", "Run")

# Weekly triathlon pattern (1 swim/week)
weekly_pattern <- c("Rest", "Bike", "Run", "Swim", "Bike", "Bike", "Run")
sport_plan <- rep(weekly_pattern, length.out = days)

### Generate TSS values ###
tss_base <- case_when(
  sport_plan == "Rest" ~ 0, 
  sport_plan == "Swim" ~ sample(30:80, 1),
  sport_plan == "Bike" ~ sample(40:200, 1),
  sport_plan == "Run"  ~ sample(40:150, 1)
)

week <- rep(1:16, each = 7)
progression_factor <- ifelse(week %% 4 == 0, 0.6, 1 + (week %% 4) * 0.15)
tss <- round(tss_base * progression_factor)

### Duration ###
duration_min <- round(tss * runif(days, 0.8, 1.4))

### Realistic Ironman Swim Durations ###
duration_min <- ifelse(
  sport_plan == "Swim",
  sample(c(45, 50, 60, 75, 90),
         size = sum(sport_plan == "Swim"),
         replace = TRUE,
         prob = c(0.4, 0.3, 0.2, 0.07, 0.03)),
  duration_min
)

### Distance ###
distance_km <- case_when(
  sport_plan == "Rest" ~ 0,
  sport_plan == "Swim" ~ round(duration_min / 17, 2), 
  sport_plan == "Bike" ~ round(duration_min / 2.4, 1),
  sport_plan == "Run"  ~ round(duration_min / 6, 2)
)

### RPE ###
rpe <- case_when(
  tss == 0 ~ 1,
  tss < 50 ~ sample(3:5, days, replace = TRUE),
  tss < 100 ~ sample(5:7, days, replace = TRUE),
  TRUE ~ sample(7:9, days, replace = TRUE)
)

### HRV ###
hrv <- 70 +
  rnorm(days, 0, 3) -
  (tss > 120) * runif(days, 2, 6)
hrv <- round(hrv)

### Soreness ###
soreness <- pmin(10,
                 pmax(1,
                      round(
                        2 +
                          (tss / 50) +
                          (rpe / 3) +
                          rnorm(days, 0, 1)
                      )))

### Injuries removed: force zero injuries ###
injury_flag <- rep(0, days)

### Training Load Metrics ###
ATL <- rollapply(tss, 7, mean, fill = NA, align = "right")
CTL <- rollapply(tss, 28, mean, fill = NA, align = "right")
TSB <- CTL - ATL

ACWR <- rollapply(tss, 7, mean, fill = NA, align = "right") /
  rollapply(tss, 28, mean, fill = NA, align = "right")

### Combine ###
df <- tibble(
  date = date_seq,
  week,
  sport = sport_plan,
  duration_min,
  distance_km,
  tss,
  rpe,
  hrv,
  sleep_hours = round(rnorm(days, 7.5, 0.8), 1),
  soreness,
  injury_flag,
  ATL,
  CTL,
  TSB,
  ACWR
)

### Save ###
write_csv(df, "~/Documents/Work/Sport Science/Github/R/triathlon_data.csv")
