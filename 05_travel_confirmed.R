# =====================================================================
# SSAC27 · 05 · Travel / time-zone exposure + confirmed-injury sensitivity
# Venue = city from the ESPN schedule (handles Mexico City, Paris, NBA Cup
# neutral sites). City-level coordinates; standard (non-DST) UTC offsets.
# =====================================================================
suppressMessages({ library(dplyr); library(survival) })
data_dir <- Sys.getenv("SLOAN_DATA", "data")
base <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"

get_sch <- function(y) {
  f <- file.path(data_dir, sprintf("sch_%d.rds", y))
  if (!file.exists(f))
    download.file(sprintf("%s/espn_nba_schedules/nba_schedule_%d.rds", base, y), f, mode = "wb", quiet = TRUE)
  x <- as.data.frame(readRDS(f)); x$season <- y
  x |> transmute(season, game_id = as.character(id), city = venue_address_city)
}
sch <- bind_rows(lapply(2019:2026, get_sch))

cities <- read.csv(text = "
city,lat,lon,utc
Los Angeles,34.05,-118.24,-8
Inglewood,33.96,-118.35,-8
San Francisco,37.77,-122.42,-8
Oakland,37.80,-122.27,-8
Sacramento,38.58,-121.49,-8
Portland,45.52,-122.68,-8
Las Vegas,36.17,-115.14,-8
Phoenix,33.45,-112.07,-7
Denver,39.74,-104.99,-7
Salt Lake City,40.76,-111.89,-7
Dallas,32.78,-96.80,-6
Houston,29.76,-95.37,-6
San Antonio,29.42,-98.49,-6
Austin,30.27,-97.74,-6
Oklahoma City,35.47,-97.52,-6
Memphis,35.15,-90.05,-6
New Orleans,29.95,-90.07,-6
Chicago,41.88,-87.63,-6
Milwaukee,43.04,-87.91,-6
Minneapolis,44.98,-93.27,-6
Mexico City,19.43,-99.13,-6
Indianapolis,39.77,-86.16,-5
Detroit,42.33,-83.05,-5
Cleveland,41.50,-81.69,-5
Atlanta,33.75,-84.39,-5
Charlotte,35.23,-80.84,-5
Orlando,28.54,-81.38,-5
Lake Buena Vista,28.37,-81.52,-5
Tampa,27.95,-82.46,-5
Miami,25.78,-80.19,-5
Washington,38.90,-77.04,-5
Philadelphia,39.95,-75.17,-5
New York,40.75,-73.99,-5
Brooklyn,40.68,-73.98,-5
Boston,42.36,-71.06,-5
Toronto,43.65,-79.38,-5
London,51.51,-0.13,0
Paris,48.86,2.35,1
Berlin,52.52,13.40,1", strip.white = TRUE)

hav <- function(la1, lo1, la2, lo2) {
  r <- pi / 180; a <- sin((la2 - la1) * r / 2)^2 + cos(la1 * r) * cos(la2 * r) * sin((lo2 - lo1) * r / 2)^2
  2 * 6371 * asin(sqrt(a))
}

cp <- readRDS(file.path(data_dir, "cp_player_games.rds"))
# every regular-season game of each team (travel needs the full schedule, not just the risk set)
box_games <- bind_rows(lapply(2019:2026, function(y)
  as.data.frame(readRDS(file.path(data_dir, sprintf("pb_%d.rds", y)))) |>
    filter(season_type == 2) |> distinct(season, game_id, team_id, game_date))) |>
  mutate(game_id = as.character(game_id), game_date = as.Date(game_date))
tr <- box_games |>
  left_join(sch, by = c("season", "game_id")) |>
  left_join(cities, by = "city") |>
  arrange(season, team_id, game_date) |>
  group_by(season, team_id) |>
  mutate(km = coalesce(hav(lag(lat), lag(lon), lat, lon), 0),
         tz_shift = coalesce(utc - lag(utc), 0),                 # + = travelled east
         km7 = sapply(seq_along(game_date), function(i)
           sum(km[game_date >= game_date[i] - 6 & game_date <= game_date[i]]))) |>
  ungroup() |>
  select(season, team_id, game_id, city, km, km7, tz_shift)
cat("Games with unmatched city:", sum(is.na(tr$km)), "| unmatched names:",
    paste(setdiff(unique(tr$city), cities$city), collapse = ", "), "\n")

cp <- cp |> mutate(game_id = as.character(game_id)) |>
  left_join(tr, by = c("season", "team_id", "game_id"))
conf <- readRDS(file.path(data_dir, "events_confirmed.rds"))
cp <- cp |> left_join(conf, by = c("season", "team_id", "athlete_id", "tg")) |>
  mutate(event_conf = event & coalesce(confirmed, FALSE))

main <- cp |> filter(!covid_excl, !late_season, !is.na(rest_days)) |>
  mutate(rest = relevel(factor(ifelse(rest_days == 1, "B2B", ifelse(rest_days == 2, "1 day", "2+ days"))), ref = "1 day"),
         role10 = prior_avg_min / 10, dens_num = pmin(dens7, 4), km7_1000 = km7 / 1000,
         tz = relevel(factor(ifelse(tz_shift >= 1, "East", ifelse(tz_shift <= -1, "West", "None"))), ref = "None"))
cat("Unmatched travel rows in model data:", sum(is.na(main$km7)), "\n")
print(summary(main$km7)); print(table(main$tz))

fmt <- function(fit, label) {
  s <- summary(fit)$conf.int; cf <- summary(fit)$coefficients
  data.frame(model = label, term = rownames(s), HR = round(s[, 1], 2), lo = round(s[, 3], 2),
             hi = round(s[, 4], 2), p = signif(cf[rownames(s), ncol(cf)], 2), row.names = NULL)
}
# M4 · M2t + travel in last 7 days + time-zone shift since previous game
m4 <- coxph(Surv(start, stop, event) ~ dens_num + rest + away + role10 + km7_1000 + tz +
              strata(season) + cluster(athlete_id), data = main)
# S3 · M2t with confirmed-injury events only (unconfirmed absences censored as non-events)
s3 <- coxph(Surv(start, stop, event_conf) ~ dens_num + rest + away + role10 +
              strata(season) + cluster(athlete_id), data = main)
res <- bind_rows(fmt(m4, "M4 +travel"), fmt(s3, "S3 confirmed injuries"))
print(res, row.names = FALSE)
cat("Confirmed events in S3:", sum(main$event_conf), "\n")
write.csv(res, "results_hr_travel_confirmed.csv", row.names = FALSE)
