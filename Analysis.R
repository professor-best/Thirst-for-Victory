library(tidyverse)
library("randomForest")
library(ggrepel)
library(coin)

data <- read_csv("cokevpepsi.csv") %>%
  filter(!is.na(`Coke / Pepsi`)) %>%
  select(-Season, -`Current coach`, - `Active Coaching Length`, - DFP) %>%
  mutate(Seed = as.numeric(Seed)) %>%
  mutate(Madness = as.factor(ifelse(!is.na(Seed), 1, 0)),
         Conference = as.factor(Conference),
         Seed = as.factor(Seed),
         Region = as.factor(Region),
         `Post-Season Tournament` = as.factor(`Post-Season Tournament`),
         CvP = as.factor(`Coke / Pepsi`)
         ) %>%
  select(-`Coke / Pepsi`) %>%
  rename(Team.Name = 'Team Name',
         Net_Rating_Rank = 'Net Rating Rank',
         Active_Coaching_Length_Index = 'Active Coaching Length Index',
         Off.eFG = 'Off.eFG %',
         Off.eFG_Rank = 'Off.eFG % Rank',
         Off.TO = 'Off.TO %',
         Off.TO.Rank = 'Off.TO % Rank',
         Off.OR = 'Off.OR %',
         Off.OR.Rank = 'Off.OR % Rank',
         Off.FT.Rate = 'Off.FT Rate',
         Off.FT.Rate.Rank = 'Off.FT Rate Rank',
         Def.eFG = 'Def.eFG %',
         Def.eFG.Rank = 'Def.eFG % Rank',
         Def.TO = 'Def.TO %',
         Def.TO.Rank = 'Def.TO % Rank',
         Def.OR = 'Def.OR %',
         Def.OR.Rank = 'Def.OR % Rank',
         Def.FT.Rate = 'Def.FT Rate',
         Def.FTRate.Rank = 'Def.FTRate Rank',
         Off.FT.Rank = 'Off.FT Rank',
         Off.2PT.FG = 'Off.2PT FG',
         Off.2PT.FG.Rank = 'Off.2PT FG Rank',
         Off.3PT.FG = 'Off.3PT FG',
         Off.3PT.FG.Rank = 'Off.3PT FG Rank',
         Def.FT.Rank = 'Def.FT Rank',
         Def.2PT.FG = 'Def.2PT FG',
         Def.2PT.FG.Rank = 'Def.2PT FG Rank',
         Def.3PT.FG = 'Def.3PT FG',
         Def.3PT.FG.Rank = 'Def.3PT FG Rank',
         Adjusted.Temo = 'Adjusted Temo',
         Adjusted.Tempo.Rank = 'Adjusted Tempo Rank',
         Raw.Tempo = 'Raw Tempo',
         Raw.Tempo.Rank = 'Raw Tempo Rank',
         Avg.Possession.Length.Offense = 'Avg Possession Length (Offense)',
         Avg.Possession.Length.Offense.Rank = 'Avg Possession Length (Offense) Rank',
         Avg.Possession.Length.Defense. = 'Avg Possession Length (Defense)',
         Avg.Possession.Length.Defense.Rank = 'Avg Possession Length (Defense) Rank',
         Adjusted.Offensive.Efficiency = 'Adjusted Offensive Efficiency',
         Adjusted.Offensive.Efficiency.Rank = 'Adjusted Offensive Efficiency Rank',
         Raw.Offensive.Efficiency = 'Raw Offensive Efficiency',
         Raw.Offensive.Efficiency.Rank = 'Raw Offensive Efficiency Rank',
         Adjusted.Defensive.Efficiency = 'Adjusted Defensive Efficiency',
         Adjusted.Defensive.Efficiency.Rank = 'Adjusted Defensive Efficiency Rank',
         Raw.Defensive.Efficiency = 'Raw Defensive Efficiency',
         Raw.Defensive.Efficiency.Rank = 'Raw Defensive Efficiency Rank',
         Net.Rating = 'Net Rating',
         PostSeason.Tournament = 'Post-Season Tournament',
         PostSeason.Tournament.Sorting.Index = 'Post-Season Tournament Sorting Index'
         )

# Random Forest
data_m1 <- data %>% select(-PostSeason.Tournament.Sorting.Index, -Seed, -Team.Name, -PostSeason.Tournament, -Region)

model1 <- randomForest(Madness ~ .,
                      data = data_m1,
                      importance = T
)


# Logistic Regression
filtered_data <- Filter(is.numeric, data)
as.tibble(cor(filtered_data, use = "pairwise.complete.obs"))

summary(glm(Madness ~ CvP, data = data, family = "binomial"))


# Chi Square Test of Independence
chi <- tibble(
  "Sponsor" = c("Coke", "Pepsi"),
  R64 = c(34, 30),
  R32 = c(14, 18),
  R16 = c(10, 6),
  R8 = c(8,0),
  R4 = c(4,0),
  R2 = c(2,0)
)
chi

table(chi$Sponsor)

coke <- tibble(
  "Sponsor" = c(rep("Coke", 72)),
  "Round" = c(rep("R64", 34), rep("R32", 14), rep("N16", 10), rep('N8', 8), rep("N4", 4), rep("N2", 2))
)

pepsi <- tibble(
  "Sponsor" = c(rep("Pepsi", 54)),
  "Round" = c(rep("R64", 30), rep("R32", 18), rep("N16", 6))
)

total <- bind_rows(coke, pepsi)

table(total$Sponsor, total$Round)

chisq


out <- chisq.test(table(total$Sponsor, total$Round))
out$expected
out$p.value

# Perform Fisher's exact test
fisher_result <- fisher.test(table(total$Sponsor, total$Round))

# Print the result
print(fisher_result)









