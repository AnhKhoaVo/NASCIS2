library(reshape2)
Scores_Update <- Scores_Update[c("PTNumber", "Time_update", "Lower_MS")]
Scores_Update$Time_update = factor(Scores_Update$Time_update, levels=c('ER','24h','48h','72h','6w','6m', '1y'))
Scores_Update_wide <- dcast.data.table(getanID(Scores_Update, "PTNumber"), 
                                       PTNumber ~.id, value.var="Lower_MS")
colnames(Scores_Update_wide) <- c("PTNumber", "Lower_MS_ER", "Lower_MS_24h", "Lower_MS_48h",
                                  "Lower_MS_72h", "Lower_MS_6wks", "Lower_MS_6mths", "Lower_MS_1y")



SCI <- merge(Variables,Scores_Update_wide, by = "PTNumber", all=TRUE)
SCI <- merge(SCI, Blood_work, by="PTNumber", all=TRUE)
SCI_noNA <- SCI[complete.cases(SCI), ]
SCI_blood <- merge(Blood_work, Scores_Update_wide, by ="PTNumber", all=TRUE)
SCI_blood <- SCI_blood[c("Lower_MS_1y", "AST_72h" , "ALT_72h" , "ALP_72h" , "LDH_72h" , 
                    "BILtot_72h" , "TOTPRO_72h" , "ALB_72h", "Lower_MS_72h")]


SCI_blood_scaled <- SCI_blood
SCI_blood_scaled[,2:9] <- log10(SCI_blood[,2:9])

SCI_blood_noNA<-SCI_blood[complete.cases(SCI_blood), ]




ggplot(data=SCI, aes(x=Lower_MS_1y, y=ALB_72h))+
  geom_jitter()+
  stat_smooth(method = "lm", formula = "y~x")

#Full model
lm_1 <- lm(Lower_MS_1y ~ AST_72h + ALT_72h + ALP_72h + LDH_72h + 
          BILtot_72h + TOTPRO_72h + ALB_72h + Lower_MS_72h, data = SCI_blood_noNA)
summary(lm_1)

#Model 2 based on subset regression 
lm_2 <- lm(Lower_MS_1y ~ Lower_MS_72h+AST_72h+ALT_72h+TOTPRO_72h+ALB_72h, data=SCI_blood_noNA)
summary(lm_2)

#Model 3 based on random forest 
lm_3 <- lm(Lower_MS_1y ~ LDH_72h + ALP_72h + ALB_72h +Lower_MS_72h, data=SCI_blood_noNA)
summary(lm_3)

#Model 4 based on stepwise AIC regression 
loess_4 <- loess(Lower_MS_1y ~ ALB_72h +Lower_MS_72h,data=SCI_blood_noNA,
              span=0.75, degree = 2)

lm_4 <- lm(Lower_MS_1y ~ ALB_72h +Lower_MS_72h,data=sample_train)

summary(lm_4)

#Split data in half to build predictive model
sample <- sample(seq(1, nrow(SCI_blood_noNA)), replace=FALSE)
sample_train <- SCI_blood_noNA[sample[1:250],]
sample_eval <- SCI_blood_noNA[sample[251:nrow(SCI_blood_noNA)],]

#Using non parametric test on train data and then predict it on test data 
np_1 <- npregbw(Lower_MS_1y ~ ALB_72h +Lower_MS_72h, regtype="ll",
              bwmethod="cv.aic", gradients=TRUE, data=sample_train)
npsigtest(np_1)
model_np <- npreg(bws=np_1)

predict_np<- predict(model_np, data=sample_train, newdata=sample_eval)
err_np <- mean((sample_eval$Lower_MS_1y-predict_np)^2)

plot(np_1, gradients = TRUE, plot.errors.method = "bootstrap")


#Predictive model for parametric linear 
predict_lm <- predict(lm_4, data=sample_train, newdata = sample_eval)
err_lm <- mean((sample_eval$Lower_MS_1y-predict_lm)^2)

#Plot for predictive power of the 2 models
plot(sample_train$ALB_72h, sample_train$Lower_MS_1y, xlab = "Predictor variable", ylab = "Predicted variable")
points(predict_np,sample_eval$ALB_72h, col = "red")
points(predict_np,sample_eval$Lower_MS_1y, col = "blue")

plot(sample_train$ALB_72h, sample_train$Lower_MS_1y, xlab = "Predictor variable", ylab = "Predicted variable")
points(predict_lm,sample_eval$ALB_72h, col = "red")
points(predict_lm,sample_eval$Lower_MS_1y, col = "blue")

#Comparison between actual and predicted data 
actuals_preds <- data.frame(cbind(actuals=sample_eval$Lower_MS_1y, 
                                  predicteds=predict_lm))
cor(actuals_preds)
accuracy <- data.frame(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
colnames(accuracy) <- "x"
accuracy <- subset(accuracy, x!= -Inf)
mean(accuracy$x) #only 28%!!!!!


actuals_preds_np <- data.frame(cbind(actuals=sample_eval$Lower_MS_1y, 
                                  predicteds=predict_np))
cor(actuals_preds_np)
accuracy_np <- data.frame(apply(actuals_preds_np, 1, min) / apply(actuals_preds_np, 1, max))
colnames(accuracy_np) <- "x"
accuracy_np <- subset(accuracy_np, x!= -Inf)
mean(accuracy_np$x) #only 33%!!!!!


anova(lm_1, lm_4)

library(randomForest)
library(caret)
library(leaps)

#Variable selection using random forest 
Var_select <- randomForest(Lower_MS_1y ~ AST_72h + ALT_72h + ALP_72h + LDH_72h + 
                           BILtot_72h + TOTPRO_72h + ALB_72h + Lower_MS_72h, data=SCI, na.action = na.omit)
varImpPlot(Var_select,type=2)
importance(Var_select)

#Variable selection using subset regression 
Sub <- regsubsets(Lower_MS_1y ~ AST_72h + ALT_72h + ALP_72h + LDH_72h + 
              BILtot_72h + TOTPRO_72h + ALB_72h + Lower_MS_72h, data=SCI, nbest = 3)

plot(Sub, scale="adjr2")
subsets(Sub, statistic="adjr2")

#Variable selection using stepwise regression 
drop1(lm_1, test = "Chi")
stepAIC(lm_1, direction="both")

library(reshape)
SCI_few <- head(SCI, 100)

SCI_long <- melt(SCI_few, id.vars=c("PTNumber", "SEX", "AGE"), 
                 measure.vars = c("Lower_MS_ER","Lower_MS_24h", "Lower_MS_72h","Lower_MS_6wks", 
                                  "Lower_MS_6mths", "Lower_MS_1y"), 
                 variable_name = "Time", value.name="Lower_MS")

SCI_long$Time<- gsub("Lower_MS_", "", SCI_long$Time)
SCI_long$Time = factor(SCI_long$Time, levels=c('ER','24h','72h','6wks','6mths', '1y'))


ggplot(data = SCI_long, aes(x=Time, y=value, group=PTNumber)) +
  geom_point()+
  geom_line()+
  geom_smooth(aes(group = 1, colour = "Trendline"), method = "loess", formula=y~x, size = 1, linetype = "dashed", se = FALSE)
  