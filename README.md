# Schedule density and time-to-absence in the NBA (SSAC27)

Reproducible pipeline, public data only. No team-proprietary data is used.

## Run

```r
# R >= 4.2; packages: dplyr, tidyr, data.table, survival, ggplot2
Sys.setenv(SLOAN_DATA = "data")   # box scores are downloaded here on first run
source("01_build_dataset.R")      # player-game counting-process dataset
source("02_models.R")             # Cox models -> results_*.csv
source("03_figures.R")            # fig1_hazard_ratios.png, fig2_face_validity.png
source("04_validation.R")         # share of events with explicit injury reason; absence length
source("05_travel_confirmed.R")   # travel/time-zone model + confirmed-injury sensitivity
```

## Data

ESPN NBA player box scores published by sportsdataverse
(`https://github.com/sportsdataverse/sportsdataverse-data/releases`, tag
`espn_nba_player_boxscores`), the same source used by the `hoopR` package.
Seasons 2018-19 to 2025-26, regular season; All-Star weekend teams removed.
Venue city from the ESPN schedule (tag `espn_nba_schedules`); city coordinates and
standard UTC offsets are hard-coded in `05_travel_confirmed.R`.

## Design

- **Unit:** player-game for rotation players (5+ prior games, 15+ prior minutes per game).
- **Outcome (proxy):** player appears in a game, then misses the team's next 3+ games.
  Inactive players are absent from public box scores, so the outcome is an absence proxy,
  not a confirmed injury. Censored at trades and season end.
- **Exposure:** team games in the 7 days ending on the game; rest days before the game;
  home/away. Schedule is fixed in advance, so it is exogenous to player health.
- **Model:** Andersen-Gill Cox, time = team game number, strata = season,
  player-clustered robust SEs. Secondary: km travelled in 7 days, time-zone shift.
  Sensitivity: gamma frailty; no exclusions; confirmed-injury events only.
- **Main exclusions:** 2020-21 season, 15 Dec 2021 - 15 Jan 2022, 2020 bubble restart,
  last 10 games of each team's season.

## Known limitations (to address in the full paper)

1. Absence proxy includes non-injury absences (personal, suspension, G League).
   Only 27% of events carry an explicit injury/illness reason in the box score.
   Validate against official NBA injury reports.
2. No player age yet; time zones use standard offsets (DST ignored).
3. Healthy-player selection: exposure is only observed when a player is available.
