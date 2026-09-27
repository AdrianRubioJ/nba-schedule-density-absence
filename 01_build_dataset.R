# =====================================================================
# SSAC27 · Schedule density and injury-proxy absence in the NBA
# 01 · Build player-game counting-process dataset from PUBLIC data only
# Source: sportsdataverse-data releases (ESPN box scores via hoopR)
# =====================================================================
suppressMessages({ library(dplyr); library(tidyr); library(data.table) })

data_dir <- Sys.getenv("SLOAN_DATA", "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
base <- "https://github.com/sportsdataverse/sportsdataverse-data/releases/download"
seasons <- 2019:2026                     # season label = year it ends (2019 = 2018-19)

get_box <- function(y) {
  f <- file.path(data_dir, sprintf("pb_%d.rds", y))
  if (!file.exists(f))
    download.file(sprintf("%s/espn_nba_player_boxscores/player_box_%d.rds", base, y),
                  f, mode = "wb", quiet = TRUE)
  readRDS(f) |> as.data.frame()
}

box <- bind_rows(lapply(seasons, get_box)) |>
  filter(season_type == 2,                           # regular season only
         !team_abbreviation %in% c("EAST", "WEST", "WORLD", "USA", "STARS", "STRIPES", "CAN", "CHK",
                                   "DUR", "GIA", "KEN", "LEB", "SHQ")) |>   # drop All-Star weekend teams
  transmute(season, game_id, game_date = as.Date(game_date), athlete_id,
            player = athlete_display_name, team_id, team = team_abbreviation,
            away = home_away == "away",
            minutes = suppressWarnings(as.numeric(minutes)),
            did_not_play, reason = toupper(coalesce(reason, "")))

# ---------------------------------------------------------------------
# Team schedule features (known in advance -> exogenous exposure)
# ---------------------------------------------------------------------
team_games <- box |>
  distinct(season, team_id, team, game_id, game_date, away) |>
  arrange(season, team_id, game_date) |>
  group_by(season, team_id) |>
  mutate(tg = row_number(),                               # team game number
         rest_days = as.numeric(game_date - lag(game_date)),
         b2b = !is.na(rest_days) & rest_days == 1,
         dens7 = sapply(seq_along(game_date),                # games in 7 days ending today
                        function(i) sum(game_date >= game_date[i] - 6 & game_date <= game_date[i])),
         n_team_games = n()) |>
  ungroup()

# ---------------------------------------------------------------------
# Player status in every team game of each player-team-season spell
# ---------------------------------------------------------------------
non_injury <- c("COACH'S DECISION", "REST", "SUSPENDED", "PERSONAL", "NOT WITH TEAM", "")
bx <- box |>
  mutate(played = !did_not_play & !is.na(minutes) & minutes > 0,
         dnp_injury = did_not_play & !grepl(paste(non_injury[-6], collapse = "|"), reason) & reason != "") |>
  select(season, game_id, team_id, athlete_id, player, minutes, played, dnp_injury, reason)

spells <- bx |>
  inner_join(team_games |> select(season, game_id, team_id, tg), by = c("season", "game_id", "team_id")) |>
  group_by(season, team_id, athlete_id) |>
  summarise(first_tg = min(tg), last_tg = max(tg), .groups = "drop")

grid <- spells |>
  inner_join(team_games, by = c("season", "team_id"), relationship = "many-to-many") |>
  filter(tg >= first_tg, tg <= last_tg) |>
  left_join(bx, by = c("season", "game_id", "team_id", "athlete_id")) |>
  mutate(in_box = !is.na(played),
         played = coalesce(played, FALSE),
         missed = !in_box | coalesce(dnp_injury, FALSE)) |>
  arrange(season, team_id, athlete_id, tg) |>
  group_by(season, team_id, athlete_id) |>
  mutate(
    # event after game g: player played g and missed each of the next 3 team games of the spell
    nxt1 = lead(missed, 1), nxt2 = lead(missed, 2), nxt3 = lead(missed, 3),
    determinable = !is.na(nxt3),                    # < 3 games left in spell -> censor
    event = played & determinable & nxt1 & nxt2 & nxt3,
    # role: average minutes in games played BEFORE this one (no look-ahead)
    prior_gp  = lag(cumsum(played), default = 0),
    prior_min = lag(cumsum(ifelse(played, minutes, 0)), default = 0),
    prior_avg_min = ifelse(prior_gp > 0, prior_min / prior_gp, NA),
    # player minutes over the last 7 days incl. today (load, for secondary models)
    min7 = sapply(seq_along(game_date), function(i)
      sum(ifelse(played, minutes, 0)[game_date >= game_date[i] - 6 & game_date <= game_date[i]]))
  ) |>
  ungroup()
grid$player <- NULL
grid <- grid |> left_join(distinct(box, athlete_id, player) |> group_by(athlete_id) |>
                            slice(1) |> ungroup(), by = "athlete_id")

# ---------------------------------------------------------------------
# Risk set: rotation players' played games; counting-process interval (tg-1, tg]
# ---------------------------------------------------------------------
cp <- grid |>
  filter(played, determinable, prior_gp >= 5, prior_avg_min >= 15) |>
  mutate(start = tg - 1, stop = tg,
         covid_excl = season == 2021 |                                   # 2020-21 protocols season
           (game_date >= as.Date("2021-12-15") & game_date <= as.Date("2022-01-15")) | # Omicron wave
           (season == 2020 & game_date >= as.Date("2020-07-01")),        # bubble restart
         late_season = tg > n_team_games - 10,
         dens_cat = factor(pmin(dens7, 4), levels = 1:4)) |>
  select(season, athlete_id, player, team_id, team, game_id, game_date, tg, start, stop, event,
         dens7, dens_cat, b2b, rest_days, away, minutes, min7, prior_avg_min,
         covid_excl, late_season)

saveRDS(cp, file.path(data_dir, "cp_player_games.rds"))
saveRDS(grid |> select(season, team_id, athlete_id, tg, game_date, in_box, played, dnp_injury, reason),
        file.path(data_dir, "grid_player_team_games.rds"))   # for proxy validation (04)
cat("Player-games in risk set:", nrow(cp), "| events:", sum(cp$event),
    "| players:", n_distinct(cp$athlete_id), "| seasons:", paste(range(cp$season), collapse = "-"), "\n")
