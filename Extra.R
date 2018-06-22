library(reshape2)
library(data.table)
library(splitstackshape)
Scores_Update_MS <- Scores_Update[c("PTNumber", "Time_update", "Lower_MS", "Upper_MS", "Total_MS")]
Scores_Update_MS$Time_update = factor(Scores_Update_MS$Time_update, levels=c('ER','24h','48h','72h','6w','6m', '1y'))
Scores_Update_wide <- dcast.data.table(getanID(Scores_Update_MS, "PTNumber"), 
                                       PTNumber ~.id, value.var="Lower_MS")
colnames(Scores_Update_wide) <- c("PTNumber", "Lower_MS_ER", "Lower_MS_24h", "Lower_MS_48h",
                                  "Lower_MS_72h", "Lower_MS_6wks", "Lower_MS_6mths", "Lower_MS_1y")

Scores_Update_wide_upper <- dcast.data.table(getanID(Scores_Update_MS, "PTNumber"), 
                                       PTNumber ~.id, value.var="Upper_MS")
colnames(Scores_Update_wide_upper) <- c("PTNumber", "Upper_MS_ER", "Upper_MS_24h", "Upper_MS_48h",
                                  "Upper_MS_72h", "Upper_MS_6wks", "Upper_MS_6mths", "Upper_MS_1y")


Scores_Update_wide_total <- dcast.data.table(getanID(Scores_Update_MS, "PTNumber"), 
                                             PTNumber ~.id, value.var="Total_MS")
colnames(Scores_Update_wide_total) <- c("PTNumber", "Total_MS_ER", "Total_MS_24h", "Total_MS_48h",
                                        "Total_MS_72h", "Total_MS_6wks", "Total_MS_6mths", "Total_MS_1y")

Scores_Update_2_touchpin <- Scores_Update_2[c("PTNumber", "Time_update", "Total_Pin", "Total_Touch", "Lower_Pin", "Lower_Touch")]
Scores_Update_2_touchpin$Time_update = factor(Scores_Update_2_touchpin$Time_update, levels=c('ER','24h','48h','72h','6w','6m', '1y'))

Scores_Update_wide_touch <- dcast.data.table(getanID(Scores_Update_2_touchpin, "PTNumber"), 
                                       PTNumber ~.id, value.var="Lower_Touch")
colnames(Scores_Update_wide_touch) <- c("PTNumber", "Lower_Touch_ER", "Lower_Touch_24h", "Lower_Touch_48h",
                                  "Lower_Touch_72h", "Lower_Touch_6wks", "Lower_Touch_6mths", "Lower_Touch_1y")

Scores_Update_wide_pin <- dcast.data.table(getanID(Scores_Update_2_touchpin, "PTNumber"), 
                                             PTNumber ~.id, value.var="Lower_Pin")
colnames(Scores_Update_wide_pin) <- c("PTNumber", "Lower_Pin_ER", "Lower_Pin_24h", "Lower_Pin_48h",
                                        "Lower_Pin_72h", "Lower_Pin_6wks", "Lower_Pin_6mths", "Lower_Pin_1y")


Scores_Update_2_reflexes <- Scores_Update_2[c(1:3,153:168)]
Scores_Update_2_reflexes$Time_update = factor(Scores_Update_2_reflexes$Time_update, levels=c('ER','24h','48h','72h','6w','6m', '1y'))
Scores_Update_reflexes <- dcast.data.table(getanID(Scores_Update_2_reflexes, "PTNumber"), 
                                             PTNumber ~.id, value.var=c('REFLEXES - BICEPS RIGHT', 'REFLEXES - TRICEPS RIGHT',
                                                                        'REFLEXES - UPPER ABDOMINAL RIGHT', 'REFLEXES - LOWER ABDOMINAL RIGHT',
                                                                        'REFLEXES - KNEE RIGHT', 'REFLEXES - ANKLE RIGHT',
                                                                        'REFLEXES - TONE -UPPER RIGHT', 'REFLEXES - TONE -LOWER RIGHT',
                                                                        'REFLEXES - BICEPS LEFT', 'REFLEXES - TRICEPS LEFT',
                                                                        'REFLEXES - UPPER ABDOMINAL LEFT', 'REFLEXES - LOWER ABDOMINAL LEFT',
                                                                        'REFLEXES - KNEE LEFT', 'REFLEXES - ANKLE LEFT', 
                                                                        'REFLEXES - TONE -UPPER LEFT', 'REFLEXES - TONE -LOWER LEFT'))

Scores_Update_2_var <- subset(Scores_Update_2, select = -c(1:3))
names <- names(Scores_Update_2_var)
Scores_Update_2$Time_update = factor(Scores_Update_2$Time_update, levels=c('ER','24h','48h','72h','6w','6m', '1y'))
Scores_Update_2_wide <- dcast.data.table(getanID(Scores_Update_2, "PTNumber"), 
                                           PTNumber ~.id, value.var=names)

SCI_forms <- merge(F1, Scores_Update_2_wide, by="PTNumber", all=TRUE)
SCI_form <- merge(SCI_forms, Blood_work, by="PTNumber", all=TRUE)




SCI <- merge(Variables,Scores_Update_wide, by = "PTNumber", all=TRUE)
SCI <- merge(SCI, Blood_work, by="PTNumber", all=TRUE)
SCI <- merge(SCI, Scores_Update_wide_touch, by="PTNumber", all=TRUE)
SCI <- merge(SCI, Scores_Update_wide_pin, by="PTNumber", all=TRUE)


SCI_noNA <- SCI[complete.cases(SCI), ]
SCI_blood <- SCI[c("Lower_MS_1y", "AST_72h", "ALT_72h", "ALP_72h",
                         "LDH_72h", "BILtot_72h", "TOTPRO_72h", "ALB_72h","AST_10d", "ALT_10d", "ALP_10d",
                   "LDH_10d", "BILtot_10d", "TOTPRO_10d", "ALB_10d",
                         "Lower_MS_72h", "Lower_Pin_72h", "Lower_Touch_72h", "Lower_MS_ER")]
  
  
SCI_blood_scaled <- SCI_blood
SCI_blood_scaled[,2:9] <- log10(SCI_blood[,2:9])

SCI_blood_noNA<-SCI_blood[complete.cases(SCI_blood), ]

SCI_blood_noNA$ALB_72h_noOL = SCI_blood_noNA$ALB_72h
SCI_blood_noNA$ALB_72h_noOL[SCI_blood_noNA$ALB_72h_noOL>5] <- NA


ggplot(data=SCI, aes(x=Lower_MS_1y, y=ALB_72h))+
  geom_jitter()+
  stat_smooth(method = "lm", formula = "y~x")

#Full model
lm_1 <- lm(Lower_MS_1y ~ AST_72h + ALT_72h + ALP_72h + LDH_72h + 
          BILtot_72h + TOTPRO_72h + ALB_72h +  Lower_MS_72h + Lower_Pin_72h + Lower_Touch_72h, data = SCI_blood_noNA)
summary(lm_1)

#Model 2 based on subset regression 
lm_2 <- lm(Lower_MS_1y ~ Lower_MS_72h+AST_72h+ALT_72h+TOTPRO_72h+ALB_72h, data=SCI_blood_noNA)
summary(lm_2)

#Model 3 based on Boruta
lm_3 <- lm(Lower_MS_1y ~ AST_72h+ALT_72h+TOTPRO_72h+LDH_72h + ALB_72h + Lower_MS_72h + 
             Total_Pin_72h + Total_Touch_72h, data = SCI_blood_noNA)
summary(lm_3)

#Model 4 based on stepwise AIC regression 
loess_4 <- loess(Lower_MS_1y ~ ALB_72h +Lower_MS_72h,data=SCI_blood_noNA,
              span=0.75, degree = 2)

lm_4 <- lm(Lower_MS_1y ~  Lower_MS_72h + 
             Lower_Pin_72h + Lower_Touch_72h, data = SCI_blood_noNA)
summary(lm_4)

lm_5 <- lm(Lower_MS_1y ~ ALB_72h+ Lower_MS_72h + 
             Lower_Pin_72h + Lower_Touch_72h, data = SCI_blood_noNA)

#Option 1 to remove outlier?
cooks <- cooks.distance(lm_5)
plot(cooks, pch="*", main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooks, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks)+1, y=cooks, labels=ifelse(cooks>4*mean(cooks, na.rm=T),names(cooks),""), col="red") 

influential <- as.numeric(names(cooks)[(cooks > 4*mean(cooks, na.rm=T))])
SCI_blood_noOL <- SCI_blood_noNA[-influential,]

outlierTest(lm_5)

#Option 2 to remove outlier
SCI_blood_noNA$Resid<-resid(lm_5)
   #Find out what 2 standard deviation is
SD2<-2*sd(resid(lm_5))

   #If larger than 2sd, then 1, otherwise 0
SCI_blood_noNA$Outs<-ifelse(abs(SCI_blood_noNA$Resid)>SD2, 1, 0)
 
  #Remove outliers
SCI_blood_noOL<-SCI_blood_noNA[!SCI_blood_noNA$Outs,]


rlm_1 <- rlm(Lower_MS_1y ~ ALB_72h + Lower_MS_72h + Lower_Pin_72h + Lower_Touch_72h, psi=psi.huber, k=2.5, data=SCI_blood_noNA)
f.robftest(rlm_1, var="ALB_72h")

loess_1 <- loess(Lower_MS_1y ~ ALB_72h, span=0.75, degree=2, data=SCI_blood_noNA)

library(gam)
library(mgcv)
gam_1 <- mgcv::gam(Lower_MS_1y ~ ALB_72h+ s(Lower_MS_72h) + s(Lower_Pin_72h) + s(Lower_Touch_72h),data=SCI_blood_noNA)

knots <- quantile(SCI_blood_noNA$ALB_72h, p = c(0.25, 0.5, 0.75))
lm_6 <- lm(Lower_MS_1y ~ bs(ALB_72h, knots = knots), data = SCI_blood_noNA)

lm_ns <- lm(Lower_MS_1y ~ ALB_72h+ns(Lower_MS_72h) + ns(Lower_Pin_72h) + ns(Lower_Touch_72h), data=SCI_blood_noNA)


par(mfrow=c(1,4))
plot(gam_1)

ggplot(data=SCI_blood_noNA, aes(x=Lower_MS_72h, y=Lower_MS_1y))+
  geom_point()+
  geom_smooth(method="loess", colour="blue", se=FALSE)+
  geom_smooth(method="lm", colour="red", se=FALSE)+
  stat_smooth(method="gam", formula = y~s(x),colour="green", se=FALSE)+
  geom_smooth(method="rlm", colour="black", se=FALSE)+
  stat_smooth(method ="lm", formula = y ~ splines::bs(x), se=FALSE, colour="gray")

ggplot(data=SCI_blood_noNA, aes(x=ALB_72h_noOL, y=Lower_MS_1y))+
  geom_point()+
  geom_smooth(method="loess", colour="blue", se=FALSE)+
  geom_smooth(method="lm", colour="red", se=FALSE)+
  geom_smooth(method="gam", colour="green", se=FALSE)+
  geom_smooth(method="rlm", colour="black", se=FALSE)


#ctree URP
URP_1 <- ctree(Lower_MS_1y ~  ALB_24h + Lower_MS_72h + Lower_Touch_72h + Lower_Pin_72h, data = subset(SCI, !is.na(Lower_MS_1y)))
plot(URP_1)

#Plot
SCI_blood_MS <- subset(SCI_blood, Lower_MS_72h <= 55)

ggplot(data=SCI_blood_MS, aes(x=Lower_MS_1y, y=ALB_10d))+
  geom_point()+
  stat_smooth(method="lm")

#Split data in half to build predictive model
sample <- sample(seq(1, nrow(SCI_blood_noNA)), replace=FALSE)
sample_train <- SCI_blood_noNA[sample[1:285],]
sample_eval <- SCI_blood_noNA[sample[286:nrow(SCI_blood_noNA)],]

#Using non parametric test on train data and then predict it on test data 
library(np)
np_1 <- npregbw(Lower_MS_1y ~ ALB_72h +Lower_MS_72h, regtype="ll",
              bwmethod="cv.aic", gradients=TRUE, data=sample_train)
npsigtest(np_1)
model_np <- npreg(bws=np_1)

predict_np<- predict(model_np, data=sample_train, newdata=sample_eval)
err_np <- mean((sample_eval$Lower_MS_1y-predict_np)^2)

plot(np_1, gradients = TRUE, plot.errors.method = "bootstrap")


#Predictive model for parametric linear 
lm_train <- lm(Lower_MS_1y ~ ALB_72h +Lower_MS_72h + Lower_Pin_72h + Lower_Touch_72h,data=sample_train)
predict_lm <- predict(lm_train, data=sample_train, newdata = sample_eval)

#Plot for predictive power of the 2 models
plot(sample_eval$ALB_72h, sample_eval$Lower_MS_1y, xlab = "Predictor variable", ylab = "Predicted variable")
points(sample_eval$ALB_72, predict_np,col = "red")

plot(sample_eval$Lower_MS_72h, sample_eval$Lower_MS_1y)
points(sample_eval$Lower_MS_72h, predict_lm,col = "red")

#Comparison between actual and predicted data 
actuals_preds <- data.frame(cbind(actuals=sample_eval$Lower_MS_1y, 
                                  predicteds=predict_lm))

accuracy <- data.frame(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
colnames(accuracy) <- "x"
accuracy <- subset(accuracy, x!= -Inf)
mean(accuracy$x) #only 28%!!!!!

RMSE(actuals_preds$actuals, actuals_preds$predicteds)

actuals_preds_np <- data.frame(cbind(actuals=sample_eval$Lower_MS_1y, 
                                  predicteds=predict_np))
cor(actuals_preds_np)
accuracy_np <- data.frame(apply(actuals_preds_np, 1, min) / apply(actuals_preds_np, 1, max))
colnames(accuracy_np) <- "x"
accuracy_np <- subset(accuracy_np, x!= -Inf)
mean(accuracy_np$x) #only 33%!!!!!

#K-fold validation
train.control <- trainControl(method = "repeatedcv", 
                              number = 2, repeats = 3)

k_fold <- train(Lower_MS_1y ~ ALB_72h+ Lower_MS_72h + 
        Lower_Pin_72h + Lower_Touch_72h, data = SCI_blood_noNA, method = "lm",
      trControl = train.control)
predict_k <- predict(k_fold)
actuals_preds_k <- data.frame(cbind(actuals=SCI_blood_noNA$Lower_MS_1y, 
                                    predicteds=predict_k))

accuracy_k <-data.frame(apply(actuals_preds_k, 1, min) / apply(actuals_preds_k, 1, max))

colnames(accuracy_k) <- "x"
accuracy_k <- subset(accuracy_k, x!= -Inf)
mean(accuracy_k$x)

RMSE(actuals_preds_k$actuals, actuals_preds_k$predicteds)/mean(actuals_preds_k$actuals)

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

#Variable selection using Boruta
library(Boruta)
Boruta_output<-Boruta(Lower_MS_1y ~ AST_72h + ALT_72h + ALP_72h + LDH_72h + 
         BILtot_72h + TOTPRO_72h + ALB_72h + Lower_MS_72h+Lower_Pin_72h + Lower_Touch_72h, data=SCI_blood_noNA, doTrace=2)

names(Boruta_output$finalDecision[Boruta_output$finalDecision %in% c("Confirmed", "Tentative")])

library(reshape)
SCI_few <- head(SCI, 100)

SCI_long <- melt(SCI_few, id.vars=c("PTNumber", "SEX", "AGE"), 
                 measure.vars = c("Lower_MS_ER","Lower_MS_24h", "Lower_MS_72h","Lower_MS_6wks", 
                                  "Lower_MS_6mths", "Lower_MS_1y"), 
                 variable_name = "Time", value.name="Lower_MS")

SCI_long$Time<- gsub("Lower_MS_", "", SCI_long$Time)
SCI_long$Time = factor(SCI_long$Time, levels=c('ER','24h','72h','6wks','6mths', '1y'))
SCI_long$Time_numeric <-  as.numeric(SCI_long$Time)

xyplot(value~Time|PTNumber, col.line = "red", layout = c(9,5),
       grid = TRUE, type = c("p", "r"), data = SCI_long)


ggplot(data = SCI_long, aes(x=Time, y=value, group=PTNumber)) +
  geom_point()+
  geom_line()+
  geom_smooth(aes(group = 1, colour = "Trendline"), method = "loess", formula=y~x, size = 1, linetype = "dashed", se = FALSE)
  

