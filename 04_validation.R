# =====================================================================
# SSAC27 · 04 · Validation of the injury-proxy absence
# For each event, what do the next 3 team games say when the player IS
# listed in the box score with a DNP reason? And how long is the absence?
# =====================================================================
suppressMessages({ library(dplyr) })
data_dir <- Sys.getenv("SLOAN_DATA", "data")
cp   <- readRDS(file.path(data_dir, "cp_player_games.rds")) |>
  filter(!covid_excl, !late_season, !is.na(rest_days))
grid <- readRDS(file.path(data_dir, "grid_player_team_games.rds")) |>
  arrange(season, team_id, athlete_id, tg)

ev <- cp |> filter(event) |> select(season, team_id, athlete_id, tg0 = tg)
nx <- grid |>
  inner_join(ev, by = c("season", "team_id", "athlete_id"), relationship = "many-to-many") |>
  filter(tg > tg0, tg <= tg0 + 3)

non_inj <- "COACH|REST|PERSONAL|SUSPEND|NOT WITH TEAM|G LEAGUE|G-LEAGUE|TRADE|BEREAVE|BIRTH|FAMILY"
lab <- nx |>
  group_by(season, team_id, athlete_id, tg0) |>
  summarise(any_injury = any(coalesce(dnp_injury, FALSE)),
            .groups = "drop") |>
  # NB: a listed non-injury DNP (coach's decision, rest, personal...) breaks the event by
  # construction, so visible reasons are injury/illness by design. The informative figure is
  # the share of events CONFIRMED by an explicit injury/illness reason.
  mutate(label = ifelse(any_injury, "Injury/illness reason listed", "Not listed (inactive)"))
tab <- lab |> count(label) |> mutate(pct = round(100 * n / sum(n), 1))
print(tab)
saveRDS(lab |> transmute(season, team_id, athlete_id, tg = tg0, confirmed = any_injury),
        file.path(data_dir, "events_confirmed.rds"))   # used for sensitivity model in 05

# Absence length: consecutive missed team games after the event (within the spell)
len <- grid |>
  group_by(season, team_id, athlete_id) |>
  mutate(missed = !played) |>
  ungroup() |>
  inner_join(ev, by = c("season", "team_id", "athlete_id"), relationship = "many-to-many") |>
  filter(tg > tg0) |>
  group_by(season, team_id, athlete_id, tg0) |>
  summarise(games_missed = { m <- missed[order(tg)]; r <- which(!m); if (length(r)) r[1] - 1 else length(m) },
            .groups = "drop")
print(summary(len$games_missed))
out <- tab |> mutate(n_events = sum(n), median_games_missed = median(len$games_missed),
                     iqr = paste(quantile(len$games_missed, c(.25, .75)), collapse = "-"))
write.csv(out, "results_proxy_validation.csv", row.names = FALSE)
