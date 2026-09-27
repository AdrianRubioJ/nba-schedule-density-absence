# =====================================================================
# SSAC27 · 02 · Survival models (Andersen–Gill counting process)
# Time scale: team game number within season; strata: season
# Robust SEs clustered by player; gamma frailty as sensitivity
# =====================================================================
suppressMessages({ library(dplyr); library(survival) })
data_dir <- Sys.getenv("SLOAN_DATA", "data")
cp <- readRDS(file.path(data_dir, "cp_player_games.rds"))

prep <- function(d) d |>
  mutate(dens = relevel(factor(ifelse(dens7 <= 2, "<=2", ifelse(dens7 == 3, "3", ">=4"))), ref = "3"),
         rest = relevel(factor(ifelse(rest_days == 1, "B2B", ifelse(rest_days == 2, "1 day", "2+ days"))), ref = "1 day"),
         role10 = prior_avg_min / 10, prev7_30 = (min7 - minutes) / 30,
         dens_num = pmin(dens7, 4))

main <- prep(cp |> filter(!covid_excl, !late_season, !is.na(rest_days)))
sens_all <- prep(cp |> filter(!is.na(rest_days)))

fmt <- function(fit, label) {
  s <- summary(fit)$conf.int
  cf <- summary(fit)$coefficients
  data.frame(model = label, term = rownames(s), HR = round(s[, 1], 2),
             lo = round(s[, 3], 2), hi = round(s[, 4], 2),
             p = signif(cf[rownames(s), ncol(cf)], 2), row.names = NULL)
}

# M1 · schedule only (the exposure as a team/league decision)
m1 <- coxph(Surv(start, stop, event) ~ dens + rest + away + strata(season) + cluster(athlete_id),
            data = main)
# M2 · + player role (prior average minutes). Same-game minutes are NOT adjusted:
#      an in-game injury truncates them (they are a consequence of the event).
m2 <- coxph(Surv(start, stop, event) ~ dens + rest + away + role10 +
              strata(season) + cluster(athlete_id), data = main)
# M2t · linear trend in schedule density (games in 7 days, 1-4)
m2t <- coxph(Surv(start, stop, event) ~ dens_num + rest + away + role10 +
              strata(season) + cluster(athlete_id), data = main)
# M3 · player minutes in the previous 6 days (excl. today) instead of schedule density
m3 <- coxph(Surv(start, stop, event) ~ prev7_30 + rest + away + role10 +
              strata(season) + cluster(athlete_id), data = main)
# S1 · M2 on all seasons/games (no COVID or late-season exclusions)
s1 <- coxph(Surv(start, stop, event) ~ dens + rest + away + role10 +
              strata(season) + cluster(athlete_id), data = sens_all)
# S2 · M2 with shared gamma frailty by player
s2 <- coxph(Surv(start, stop, event) ~ dens + rest + away + role10 +
              strata(season) + frailty(athlete_id, distribution = "gamma"), data = main)

res <- bind_rows(fmt(m1, "M1 schedule"), fmt(m2, "M2 +role"), fmt(m2t, "M2t trend"), fmt(m3, "M3 player 7-day load"),
                 fmt(s1, "S1 all games"), fmt(s2, "S2 gamma frailty"))
res <- res[!grepl("frailty", res$term), ]
print(res, row.names = FALSE)
write.csv(res, "results_hr.csv", row.names = FALSE)

desc <- main |> summarise(player_games = n(), events = sum(event), players = n_distinct(athlete_id),
                          seasons = n_distinct(season), rate_per_1000 = round(1000 * mean(event), 1),
                          pct_b2b = round(100 * mean(rest == "B2B"), 1),
                          pct_dens4 = round(100 * mean(dens == ">=4"), 1))
print(desc); write.csv(desc, "results_desc.csv", row.names = FALSE)

# Crude rates by exposure for the figure
crude <- main |> group_by(rest) |> summarise(n = n(), ev = sum(event)) |>
  mutate(rate = 1000 * ev / n, lo = 1000 * qbeta(.025, ev, n - ev + 1), hi = 1000 * qbeta(.975, ev + 1, n - ev))
print(crude); write.csv(crude, "results_crude_rest.csv", row.names = FALSE)
cat("PH test M2:\n"); print(cox.zph(m2))

# Face validity of the proxy: minutes in the game before an absence vs the player's usual role
fv <- main |> group_by(event) |> summarise(median_min = median(minutes), median_role = median(prior_avg_min),
                                           pct_under_half_role = round(100 * mean(minutes < prior_avg_min / 2), 1))
print(fv); write.csv(fv, "results_face_validity.csv", row.names = FALSE)
