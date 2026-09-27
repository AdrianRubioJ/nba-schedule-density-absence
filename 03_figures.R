# =====================================================================
# SSAC27 · 03 · Figures for the abstract (max 2)
# =====================================================================
suppressMessages({ library(dplyr); library(ggplot2) })
data_dir <- Sys.getenv("SLOAN_DATA", "data")
res <- read.csv("results_hr.csv")
blue <- "#2a78d6"; orange <- "#eb6834"; ink <- "#2b2b2b"; muted <- "#6b6b66"

# Fig 1 · Adjusted hazard ratios, model M2 (+ density trend from M2t)
lab <- c("dens<=2" = "2 or fewer games in 7 days (vs 3)", "dens>=4" = "4+ games in 7 days (vs 3)",
         "dens_num" = "Per extra game in 7 days (trend)", "restB2B" = "Back-to-back, 2nd night (vs 1 day rest)",
         "rest2+ days" = "2+ days rest (vs 1 day)", "awayTRUE" = "Away game",
         "role10" = "Role: +10 prior avg minutes")
f1 <- bind_rows(res |> filter(model == "M2 +role"), res |> filter(model == "M2t trend", term == "dens_num")) |>
  mutate(label = factor(lab[term], levels = rev(lab)))
p1 <- ggplot(f1, aes(HR, label)) +
  geom_vline(xintercept = 1, colour = muted, linewidth = .4, linetype = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .18, colour = blue, linewidth = .7) +
  geom_point(size = 2.8, colour = blue) +
  geom_text(aes(label = sprintf("%.2f (%.2f-%.2f)", HR, lo, hi)), x = log10(1.48), hjust = 0,
            size = 3.1, colour = ink) +
  scale_x_log10(limits = c(.55, 2.3), breaks = c(.6, .8, 1, 1.25, 1.5)) +
  labs(x = "Hazard ratio (log scale, 95% CI, player-clustered)", y = NULL,
       title = "Schedule density and hazard of an injury-proxy absence",
       subtitle = "NBA 2018-19 to 2025-26 (excl. 2020-21), rotation players, Cox model stratified by season") +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", colour = ink), plot.subtitle = element_text(colour = muted, size = 9),
        axis.text = element_text(colour = ink))
ggsave("fig1_hazard_ratios.png", p1, width = 8.2, height = 3.9, dpi = 200, bg = "white")

# Fig 2 · Face validity: minutes in the last game before an absence vs usual role
cp <- readRDS(file.path(data_dir, "cp_player_games.rds")) |>
  filter(!covid_excl, !late_season, !is.na(rest_days)) |>
  mutate(ratio = pmin(minutes / prior_avg_min, 2),
         grp = ifelse(event, "Last game before a 3+ game absence", "All other games"))
p2 <- ggplot(cp, aes(ratio, colour = grp)) +
  geom_density(linewidth = .9, adjust = .8) +
  geom_vline(xintercept = 1, colour = muted, linewidth = .4, linetype = 2) +
  scale_colour_manual(values = c("All other games" = blue, "Last game before a 3+ game absence" = orange), name = NULL) +
  labs(x = "Minutes played / player's prior average minutes", y = "Density",
       title = "Absences are preceded by truncated games",
       subtitle = "23% of pre-absence games fall below half the player's usual minutes vs 3.7% of other games") +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot", legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", colour = ink), plot.subtitle = element_text(colour = muted, size = 9))
ggsave("fig2_face_validity.png", p2, width = 8.2, height = 3.9, dpi = 200, bg = "white")
