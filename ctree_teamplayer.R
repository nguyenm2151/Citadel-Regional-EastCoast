library(dplyr)
library(partykit)

df = read.csv("ctree_teamplayer_diff_df.csv")
df$X = NULL
df$season = as.factor(df$season)
df$league_id = as.factor(df$league_id)
#df$fold = NULL
#df$home = NULL

columns = colnames(df)

top3_col = c("result", columns[grepl( "^top3", columns) | grepl( "max", columns)], columns[84:101])
avg_col = c("result", columns[grepl( "^avg", columns) | grepl( "max", columns)], columns[84:101])

top3_col = top3_col[!grepl("avg_potential", top3_col)]
avg_col = avg_col[!grepl("avg_potential", avg_col)]

#--------------------------------------------------------------------------------------------------------------

ctree_fit_list = list()
ctree_losses = rep(0,17)

for (i in 1:17){
  
  fold_i_train_df = df[df[,columns[83+i]]=="train", top3_col]
  fold_i_train_df[,columns[84:100]] = NULL
  fold_i_train_df$result = as.factor(fold_i_train_df$result)
  
  fold_i_test_df = df[df[,columns[83+i]]=="test", top3_col]
  fold_i_test_df[,columns[84:100]] = NULL
  fold_i_test_df$result = as.factor(fold_i_test_df$result)
  
  ctree_fit_fold_i_train <- ctree(result ~ ., data = fold_i_train_df, 
                                  control=ctree_control(testtype = "Bonferroni", 
                                                        mincriterion = 0.99, 
                                                        #maxdepth=6,
                                                        mtry=12))
  
  
  ctree_fit_list[[i]] = ctree_fit_fold_i_train
  
  pred = predict(ctree_fit_fold_i_train, newdata=fold_i_test_df, type = "prob")
  
  pred_df = data.frame(result=fold_i_test_df$result)
  pred_df=  cbind(pred_df, pred)
  pred_df$loss = 0
  pred_df[pred_df$result==-1,"loss"] = -1*log(pred_df[pred_df$result== -1,2])
  pred_df[pred_df$result==0,"loss"] = -1*log(pred_df[pred_df$result==- 0,3])
  pred_df[pred_df$result==1,"loss"] = -1*log(pred_df[pred_df$result== 1,4])
  
  ctree_losses[i] = mean(pred_df$loss)
  
}



#--------------------------------------------------------------------------------------------------------------



ctree_fit_2_list = list()
ctree_losses_2 = rep(0,17)


for (i in 1:17){
  
  fold_i_train_df = df[df[,columns[83+i]]=="train", avg_col]
  fold_i_train_df[,columns[84:100]] = NULL
  fold_i_train_df$result = as.factor(fold_i_train_df$result)
  
  fold_i_test_df = df[df[,columns[83+i]]=="test", avg_col]
  fold_i_test_df[,columns[84:100]] = NULL
  fold_i_test_df$result = as.factor(fold_i_test_df$result)
  
  ctree_fit_2_fold_i_train <- ctree(result ~ ., data = fold_i_train_df, 
                                  control=ctree_control(testtype = "Bonferroni", 
                                                        mincriterion = 0.99, 
                                                        #maxdepth=6,
                                                        mtry=6))
  
  
  ctree_fit_2_list[[i]] = ctree_fit_2_fold_i_train
  
  pred = predict(ctree_fit_2_fold_i_train, newdata=fold_i_test_df, type = "prob")
  
  pred_df = data.frame(result=fold_i_test_df$result)
  pred_df=  cbind(pred_df, pred)
  pred_df$loss = 0
  pred_df[pred_df$result==-1,"loss"] = -1*log(pred_df[pred_df$result== -1,2])
  pred_df[pred_df$result==0,"loss"] = -1*log(pred_df[pred_df$result==- 0,3])
  pred_df[pred_df$result==1,"loss"] = -1*log(pred_df[pred_df$result== 1,4])
  
  ctree_losses_2[i] = mean(pred_df$loss)
  
}


#--------------------------------------------------------------------------------------------------------------



ctree_fit_3_list = list()
ctree_losses_3 = rep(0,17)


for (i in 1:17){
  
  fold_i_train_df = df[df[,columns[83+i]]=="train", avg_col]
  fold_i_train_df[,columns[84:100]] = NULL
  fold_i_train_df$result = as.factor(fold_i_train_df$result)
  
  fold_i_test_df = df[df[,columns[83+i]]=="test", avg_col]
  fold_i_test_df[,columns[84:100]] = NULL
  fold_i_test_df$result = as.factor(fold_i_test_df$result)
  
  ctree_fit_3_fold_i_train <- ctree(result ~ ., data = fold_i_train_df, 
                                    control=ctree_control(testtype = "Bonferroni", 
                                                          mincriterion = 0.99, 
                                                          #maxdepth=6,
                                                          mtry=12))
  
  
  ctree_fit_3_list[[i]] = ctree_fit_3_fold_i_train
  
  pred = predict(ctree_fit_3_fold_i_train, newdata=fold_i_test_df, type = "prob")
  
  pred_df = data.frame(result=fold_i_test_df$result)
  pred_df=  cbind(pred_df, pred)
  pred_df$loss = 0
  pred_df[pred_df$result==-1,"loss"] = -1*log(pred_df[pred_df$result== -1,2])
  pred_df[pred_df$result==0,"loss"] = -1*log(pred_df[pred_df$result==- 0,3])
  pred_df[pred_df$result==1,"loss"] = -1*log(pred_df[pred_df$result== 1,4])
  
  ctree_losses_3[i] = mean(pred_df$loss)
  
}



plot(ctree_fit_3_list[[10]])

#"avg_overall_rating", "avg_volleys_imp", "avg_long_passing", "avg_finishing, "avg_ball_control
#"avg_dribbling", "avg_short_passing", "avg_reactions", "avg_dribbling", "avg_vision_imp", "avg_penalties"
#"avg_agility_imp"





