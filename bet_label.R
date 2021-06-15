library(dplyr)
library(partykit)


df = read.csv("bet_spread_df.csv")
df$X = NULL
df$season = NULL
features = c('league_id', 'rating_diff', 'home', 'match_bin', 'avg_overall_rating',
            'avg_volleys_imp', 'avg_long_passing', 'avg_finishing',
            'avg_ball_control', 'avg_dribbling', 'avg_short_passing',
            'avg_reactions', 'avg_vision_imp', 'avg_penalties', 'avg_agility_imp',
            'cumulative_result', 'match_bin')
df = df[,c(features, "target")]
df$target = as.factor(df$target)

smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train_df = df[train_ind,]
test_df = df[-train_ind,]

ctree_bet_type <- ctree(target ~ ., data = df,
                        control=ctree_control(testtype = "Bonferroni", 
                                              mincriterion = 0.999,
                                              maxdepth=4,
                                              mtry=16))
print(ctree_bet_type)
plot(ctree_bet_type)

