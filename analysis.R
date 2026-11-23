# ---- Setup ----
library(tidyverse)
library(ggrepel)

# ---- Load data ----
mvp <- read_csv("data/mvp_2011.csv")

# ---- Clean data ----
mvp <- mvp |>
  select(-`-9999`) |>
  rename(
    pts_won = `Pts Won`,
    pts_max = `Pts Max`,
    fg_pct  = `FG%`,
    fg3_pct = `3P%`,
    ft_pct  = `FT%`,
    ws_48   = `WS/48`
  ) |>
  mutate(
    Rank    = parse_number(Rank),
    is_rose = Player == "Derrick Rose"
  )

# ---- Chart 1: Win Shares ----
ggplot(mvp, aes(x = WS, y = reorder(Player, WS), fill = is_rose)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "grey70"), guide = "none") +
  labs(
    title = "Rose ranked 4th in Win Shares among MVP vote-getters",
    x = "Win Shares",
    y = NULL
  ) +
  theme_minimal()

# ---- Chart 2: Points per game ----
ggplot(mvp, aes(x = PTS, y = reorder(Player, PTS), fill = is_rose)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "grey70"), guide = "none") +
  labs(
    title = "Rose ranked 6th in scoring among MVP vote-getters",
    x = "Points per game",
    y = NULL
  ) +
  theme_minimal()

# ---- Chart 3: Win Shares vs. vote share ----
ggplot(mvp, aes(x = WS, y = Share, color = is_rose)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Player), size = 3) +
  scale_color_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "grey50"), guide = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Rose won 98% of the vote share despite ranking 4th in Win Shares",
    x = "Win Shares",
    y = "MVP vote share"
  ) +
  theme_minimal()

# ---- Load all seasons ----
files <- list.files("data", pattern = "^mvp_\\d{4}\\.csv$", full.names = TRUE)

all_mvp <- read_csv(files, id = "file") |>
  select(-`-9999`) |>
  rename(
    pts_won = `Pts Won`,
    pts_max = `Pts Max`,
    fg_pct  = `FG%`,
    fg3_pct = `3P%`,
    ft_pct  = `FT%`,
    ws_48   = `WS/48`
  ) |>
  mutate(
    Rank   = parse_number(Rank),
    season = parse_number(file)
  )

count(all_mvp, season)

# ---- Load team records ----
team_files <- list.files("data", pattern = "^teams_\\d{4}\\.csv$", full.names = TRUE)

teams <- read_csv(team_files, id = "file") |>
  mutate(
    season = parse_number(file),
    Team   = str_remove(Team, "\\*")
  ) |>
  select(season, Team, W, L, MOV)

abbrevs <- read_csv("data/team_abbrevs.csv")

count(teams, season)

# ---- Join team records to players ----
all_mvp <- all_mvp |>
  left_join(abbrevs, by = "Tm") |>
  left_join(teams, by = c("season", "Team"))
# Drop players traded mid-season (team = "TOT"), since they have no single team record
all_mvp <- all_mvp |>
  filter(!is.na(W))
all_mvp <- all_mvp |>
  mutate(win_pct = W / (W + L))
# How many teammates also received MVP votes that season?
all_mvp <- all_mvp |>
  group_by(season, Tm) |>
  mutate(teammates_voted = n() - 1) |>
  ungroup()

# Check: any players who didn't get a team record?
all_mvp |>
  filter(is.na(W)) |>
  select(season, Player, Tm, Team)

# ---- Regression ----
train <- all_mvp |>
  filter(season != 2011)

test <- all_mvp |>
  filter(season == 2011)

# Model 1: Win Shares only
model1 <- lm(Share ~ WS, data = train)
summary(model1)

test <- test |>
  mutate(
    predicted = predict(model1, newdata = test),
    residual  = Share - predicted
  )

test |>
  select(Player, WS, Share, predicted, residual) |>
  arrange(desc(residual))

# Model 2: Win Shares + age (age was not significant)
model2 <- lm(Share ~ WS + Age, data = train)
summary(model2)

# Model 3: Win Shares + team wins
model3 <- lm(Share ~ WS + W, data = train)
summary(model3)

# Model 4: Win Shares + team win percentage
model4 <- lm(Share ~ WS + win_pct, data = train)
summary(model4)

test <- test |>
  mutate(
    predicted4 = predict(model4, newdata = test),
    residual4  = Share - predicted4
  )

test |>
  select(Player, WS, win_pct, Share, predicted4, residual4) |>
  arrange(desc(residual4))

# Model 5: Win Shares + win % + teammates who also got votes
model5 <- lm(Share ~ WS + win_pct + teammates_voted, data = train)
summary(model5)

# ---- Model comparison ----
rose <- test |> filter(Player == "Derrick Rose")

model_comparison <- tibble(
  model = c("1: WS", "2: WS + age", "3: WS + team wins",
            "4: WS + win %", "5: WS + win % + teammates"),
  adj_r2 = c(summary(model1)$adj.r.squared,
             summary(model2)$adj.r.squared,
             summary(model3)$adj.r.squared,
             summary(model4)$adj.r.squared,
             summary(model5)$adj.r.squared),
  rose_predicted = c(predict(model1, rose),
                     predict(model2, rose),
                     predict(model3, rose),
                     predict(model4, rose),
                     predict(model5, rose))
) |>
  mutate(rose_gap = rose$Share - rose_predicted)

model_comparison
# ---- Compare to other MVP winners ----
all_mvp |>
  filter(Rank == 1) |>
  select(season, Player, WS, ws_48, win_pct) |>
  arrange(WS)

# 