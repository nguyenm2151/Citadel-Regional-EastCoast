library(dplyr)
require(ggplot2)
library(GGally)
require(lme4)
library(lmerTest)
library(pbkrtest)
library(merTools)
library(MASS)


df = read.csv("stacked_df.csv")
#df = df[df$match_num>1,]
df = df[df$match_bin>=1,]
#--------------------------------------- Winners ----------------------------------------------------

win_streaks_df = df[df$cumulative_win>0,]
#win_streaks_df = win_streaks_df[win_streaks_df$match_num>4,]
win_streaks_df$cumulative_win = ifelse(win_streaks_df$cumulative_win > 9, 9, win_streaks_df$cumulative_win)
win_streaks_df$win = as.factor(win_streaks_df$win)
win_streaks_df$home = as.factor(win_streaks_df$home)

ggpairs(win_streaks_df[, c("cumulative_win", "rating_diff", "prev_opp_avg_rating","prev_opp_avg_bspread")])

#fit logistic

win_fit_lm = glm(win ~ cumulative_win + home + rating_diff + prev_opp_avg_rating,
                 data = win_streaks_df,
                 family = binomial)

summary(win_fit_lm) #19077

win_fit_lm = glm(win ~ cumulative_win + home + rating_diff + prev_opp_avg_rating + match_bin,
                 data = win_streaks_df,
                 family = binomial)

summary(win_fit_lm) #19067

win_fit_lm = glm(win ~ cumulative_win + home + rating_diff + prev_opp_avg_rating + match_num,
                 data = win_streaks_df,
                 family = binomial)

summary(win_fit_lm) #19064


win_fit_lm = glm(win ~ cumulative_win + home + rating_diff  + match_bin,
                 data = win_streaks_df,
                 family = binomial)

summary(win_fit_lm) #19062
car::vif(win_fit)


#fit mixed

win_fit <- glmer(win ~ cumulative_win + home + rating_diff + prev_opp_avg_rating 
                 + (1 | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 19075.02
AIC(win_fit)
car::vif(win_fit)
isSingular(win_fit)


win_fit <- glmer(win ~ cumulative_win + home + rating_diff + prev_opp_avg_rating 
                 + (cumulative_win | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 19074.72
AIC(win_fit)


win_fit <- glmer(win ~ cumulative_win + home + rating_diff + prev_opp_avg_bspread 
                 + (1 | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 18981.57
AIC(win_fit)
car::vif(win_fit)
isSingular(win_fit)

chq_win_fit = -2 * logLik(win_fit_lm) + 2 * logLik(win_fit)
pchisq(as.numeric(chq_win_fit), df=1, lower.tail=F) #signficant


win_fit <- glmer(win ~ cumulative_win + home + rating_diff 
                 + (1 | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 20860.7


win_fit <- glmer(win ~ cumulative_win + home + rating_diff +  
                 + (1 | season),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 20872.16
AIC(win_fit)

win_fit <- glmer(win ~ cumulative_win + home + rating_diff  + 
                   (1 | season) + (1 | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit) #AIC 20862.65
AIC(win_fit)

#fit mixed random slope
win_fit <- glmer(win ~ cumulative_win + home + rating_diff  +
                   (cumulative_win | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(win_fit)  #20865.16
AIC(win_fit)


#fit squared
win_fit2 <- glmer(win ~ cumulative_win + I(cumulative_win*cumulative_win) +
                    home + rating_diff  +
                    (1 | match_bin),
                  data = win_streaks_df,
                  , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit2) 

#fit indicator

win_fit3 <- glmer(win ~ I(cumulative_win>1) + home + rating_diff + prev_opp_avg_rating +
                    (1 | match_bin),
                 data = win_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(win_fit3) 
AIC(win_fit3) > AIC(win_fit2)


#graph best fit

#more likely to win going in the later stages

win_fit_lm = glm(win ~ cumulative_win + home + rating_diff + match_bin,
                 data = win_streaks_df,
                 family = binomial)
exp(c(-1.24654, 0.05355, 0.89943, 0.70401, 0.05325))
summary(win_fit_lm)
car::vif(win_fit_lm)
#95% CI
exp(confint(win_fit_lm))

#se <- sqrt(diag(vcov(win_fit)))
#(tab <- cbind(Est = fixef(win_fit), LL = fixef(win_fit) - 1.96 * se, UL = fixef(win_fit) + 1.96 *se))
#exp(tab)

cw_values <- with(win_streaks_df, seq(from = min(win_streaks_df$cumulative_win),
                                      to = max(win_streaks_df$cumulative_win)
                                      , length.out = 100))

biprobs <- lapply(levels(win_streaks_df$home), function(home) {
  win_streaks_df$home[] <- home
  lapply(cw_values, function(j) {
    win_streaks_df$cumulative_win <- j
    predict(win_fit, newdata = win_streaks_df, type = "response")
  })
})

plotdat2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, cw_values))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "cumulative_win")
  return(temp)
})

# collapse to one data frame
plotdat2 <- do.call(rbind, plotdat2)

plotdat2$home <- factor(rep(levels(win_streaks_df$home), each = length(cw_values)))


ggplot(plotdat2, aes(x = cumulative_win, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = home), alpha = .15) +
  geom_line(aes(colour = home), size = 2) +
  ylim(c(0, 1)) + facet_wrap(~  home)


#--------------------------------------- Losers ----------------------------------------------------


lose_streaks_df = df[df$cumulative_lose>0,]
#lose_streaks_df = lose_streaks_df[lose_streaks_df$match_num>4,]
lose_streaks_df$cumulative_lose = ifelse(lose_streaks_df$cumulative_lose > 9, 9, lose_streaks_df$cumulative_lose)
lose_streaks_df$lose = as.factor(lose_streaks_df$lose)
lose_streaks_df$home = as.factor(lose_streaks_df$home)

ggpairs(lose_streaks_df[, c("cumulative_lose", "rating_diff", "prev_opp_avg_rating", "prev_opp_avg_bspread")])

#fit logistic

lose_fit_lm = glm(lose ~ cumulative_lose + home + rating_diff + match_num,
                 data = lose_streaks_df,
                 family = binomial)

summary(lose_fit_lm) #19711

lose_fit_lm = glm(lose ~ cumulative_lose + home + rating_diff + prev_opp_avg_bspread,
                  data = lose_streaks_df,
                  family = binomial)

summary(lose_fit_lm) #19711

#fit mixed

lose_fit <- glmer(lose ~ cumulative_lose + home + rating_diff + prev_opp_avg_rating 
                 + (1 | match_bin),
                 data = lose_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(lose_fit) #AIC 21559
AIC(lose_fit)
car::vif(lose_fit)
isSingular(lose_fit)

chq_lose_fit = -2 * logLik(lose_fit_lm) + 2 * logLik(lose_fit)
pchisq(as.numeric(chq_lose_fit), df=1, lower.tail=F) #signficant

se <- sqrt(diag(vcov(lose_fit)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(lose_fit), LL = fixef(lose_fit) - 1.96 * se, UL = fixef(lose_fit) + 1.96 *se))
exp(tab)

lose_fit <- glmer(lose ~ cumulative_lose + home + rating_diff + prev_opp_avg_rating
                   + (1 | season),
                 data = lose_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(lose_fit) #AIC 21565.39
AIC(lose_fit)

lose_fit <- glmer(lose ~ cumulative_lose + home + rating_diff  + prev_opp_avg_rating +
                   (1 | season) + (1 | match_bin),
                 data = lose_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(lose_fit) #AIC 21561
AIC(lose_fit)

#fit mixed random slope
lose_fit <- glmer(lose ~ cumulative_lose + home + rating_diff  + prev_opp_avg_rating +
                    (rating_diff | match_bin),
                 data = lose_streaks_df,
                 , family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(lose_fit)  #21453.02
AIC(lose_fit)


#fit squared
lose_fit <- glmer(lose ~ cumulative_lose + I(cumulative_lose*cumulative_lose) +
                    home + rating_diff  + prev_opp_avg_rating +
                    (rating_diff | match_bin),
                  data = lose_streaks_df,
                  , family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(lose_fit)  #21454.96
AIC(lose_fit)


#fit indicator

lose_fit3 <- glmer(lose ~ I(cumulative_lose>1) + home + rating_diff + prev_opp_avg_rating + 
                     (1 | match_bin),
                  data = lose_streaks_df,
                  , family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(lose_fit3)  #21559.7


#graph best fit


lose_fit_lm = glm(lose ~ cumulative_lose + home + rating_diff + match_bin,
                  data = lose_streaks_df,
                  family = binomial)

summary(lose_fit_lm)

exp(confint(lose_fit_lm))

se <- sqrt(diag(vcov(lose_fit)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(lose_fit), LL = fixef(lose_fit) - 1.96 * se, UL = fixef(lose_fit) + 1.96 *se))
exp(tab)

cw_values <- with(lose_streaks_df, seq(from = min(lose_streaks_df$cumulative_lose),
                                      to = max(lose_streaks_df$cumulative_lose)
                                      , length.out = 100))

biprobs <- lapply(levels(lose_streaks_df$home), function(home) {
  lose_streaks_df$home[] <- home
  lapply(cw_values, function(j) {
    lose_streaks_df$cumulative_lose <- j
    predict(lose_fit, newdata = lose_streaks_df, type = "response")
  })
})

plotdat2 <- lapply(biprobs, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  temp <- as.data.frame(cbind(temp, cw_values))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "cumulative_lose")
  return(temp)
})

# collapse to one data frame
plotdat2 <- do.call(rbind, plotdat2)

plotdat2$home <- factor(rep(levels(lose_streaks_df$home), each = length(cw_values)))


ggplot(plotdat2, aes(x = cumulative_lose, y = PredictedProbability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = home), alpha = .15) +
  geom_line(aes(colour = home), size = 2) +
  ylim(c(0, 1)) + facet_wrap(~  home)


