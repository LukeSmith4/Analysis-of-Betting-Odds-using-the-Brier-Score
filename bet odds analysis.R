library("ggplot2")
library("dplyr")

matches <- read.csv("C:/Users/luke1/Downloads/E0.csv")

# Cleaning

matches_B365 <- as_tibble(matches) %>%
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, B365H, B365D, B365A) %>%
  rename(HomeGoals= FTHG, AwayGoals = FTAG, Result = FTR, HomeOdds = B365H, DrawOdds = B365D, AwayOdds = B365A)

str(matches_B365) # Check each column has correct class
# Column names represent their data
summary(matches_B365) # 380 games is correct, and there is no NAs

matches_B365 <- mutate(matches_B365, RawHomeProb = 1/HomeOdds, RawDrawProb = 1/DrawOdds, RawAwayProb = 1/AwayOdds, Total = RawHomeProb + RawDrawProb + RawAwayProb, NormHomeProb = RawHomeProb / Total, NormDrawProb = RawDrawProb / Total, NormAwayProb = RawAwayProb / Total)

# Create normalised probabilities for home, draw and away outcomes (bookie has a margin to ensure profit)

ggplot(data = matches_B365) + 
  geom_boxplot(aes(x = factor("Home", levels = c("Home","Draw","Away")),
                   y = NormHomeProb),
               fill = "#2F5597", alpha = 0.8, width = 0.6, outlier.alpha = 0.3) +
  geom_boxplot(aes(x = factor("Draw", levels = c("Home","Draw","Away")),
                   y = NormDrawProb),
               fill = "#9E9E9E", alpha = 0.8, width = 0.6, outlier.alpha = 0.3) +
  
  geom_boxplot(aes(x = factor("Away", levels = c("Home","Draw","Away")),
                   y = NormAwayProb),
               fill = "#B04A4A", alpha = 0.8, width = 0.6, outlier.alpha = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Outcome",
       y = "Implied Probability",
       title = "Distribution of Implied Probabilities by Outcome") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))

# Draws have the highest median but the odds are far less variable than home and away. Away games are the most variable, and home games are the 'easiest' according to bet365



# Another representation of the above - Home odds far better than Draw/Away odds. Away odds marginally better than draw odds and in fact, are better over the course of almost the entire season except for the first 5 weeks - bet365 maybe struggling to accurately predict at the start of the season?

matches_B365 <- mutate(matches_B365, HW = ifelse(Result == "H", 1, 0), D = ifelse(Result == "D", 1, 0), AW = ifelse(Result == "A", 1, 0))

# Create 3 new columns, HW (Home Win), D (Draw), AW (Away Win), that take a 1 if the result is satisfied and 0 otherwise

matches_B365 <- mutate(matches_B365, Brier = (HW - NormHomeProb)^2 + (D - NormDrawProb)^2 + (AW - NormAwayProb)^2)

# Calculate Brier score for each match

print(mean(matches_B365$Brier))

# Bet365's Brier score is 0.5786638, which is not that much better than just picking randomly (0.667)

brier_rolling <- mutate(matches_B365, Group = ceiling(row_number() / 10)) %>%
  group_by(Group) %>%
  summarise(AverageBrier = mean(Brier))

ggplot(brier_rolling, aes(x = Group, y = AverageBrier)) +
  geom_line(color = "#2F5597", linewidth = 1) +
  geom_point(color = "#2F5597", size = 2) +
  geom_hline(yintercept = 0.667,
             linetype = "dashed",
             color = "#B04A4A",
             linewidth = 0.8) +
  scale_y_continuous(limits = c(0.3, 0.8)) +
  labs(title = "Rolling Mean Brier Score (per Match Week)",
       x = "Match Week",
       y = "Mean Brier Score") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Large variance in Brier score for every 10 games, although usually the models are better than random predictions (except 6/38 times). Suggests some predictability, and some weeks are more predictable then others, but some weeks are completely unpredictable.