#### Software Lesson ####

#setup
syncmov<-read.csv(file=file.choose(), header=TRUE)

syncmov$CloseBefore.f<-as.factor(syncmov$CloseBefore)

syncmov1<-subset(syncmov, PainTolerance < 300 & PainToleranceBefore < 300)

syncmov2<-subset(syncmov1, 
                 !is.na(PainTolerance) & 
                 !is.na(PainToleranceBefore) &
                 !is.na(CloseBefore.f) & 
                 !is.na(Sex) & 
                 !is.na(Synch) &
                 !is.na(Exertion))

#correlation matrix
cor(syncmov2[,c("PainTolerance", "PainToleranceBefore","CloseDiff")], use = "complete.obs")

fullmodel<-glm(data=syncmov2, 
               PainTolerance ~ PainToleranceBefore + 
                 factor(Synch) + factor(Exertion) + 
                 factor(CloseBefore.f) + factor(Sex))

nullmodel<-glm(data=syncmov2, PainTolerance ~ 1)

library(stats)

#start with null model for fwd selection, go from lower (null) to upper (all pred)
model.forward<-step(nullmodel, direction="forward", 
                    scope=list(upper = ~ PainToleranceBefore + 
                                factor(Synch) + factor(Exertion) + 
                                 factor(CloseBefore.f) + factor(Sex),
                               lower = ~ 1))
summary(model.forward)

#bkwrd elim model from full model to null
model.backward<-step(fullmodel, 
                     direction="backward", 
                     scope=list(upper = ~ PainToleranceBefore + 
                                  factor(Synch) + factor(Exertion) + 
                                  factor(CloseBefore.f) + factor(Sex),
                                  lower = ~ 1))
summary(model.backward)

model.stepwise<-step(nullmodel, 
                     direction="both", 
                     scope=list(upper = ~ PainToleranceBefore + 
                                  factor(Synch) + 
                                  factor(Exertion) + 
                                  factor(CloseBefore.f) + 
                                  factor(Sex), lower = ~ 1))
summary(model.stepwise)

#model diagnostics
library(ggfortify);autoplot(model.stepwise)

diag.model<-ls.diag(model.stepwise)
names(diag.model)

###Plotting the standardized residuals for each observation 
plot(diag.model$std.res, xlab="Observation Number", ylab="Standardized residual")
###Adding a red horizontal line at -3 and 3 
abline(h=c(-3,3), col="red")
###Plotting the studentized residuals for each observation 
plot(diag.model$stud.res, xlab="Observation Number", ylab="Standardized residual")
###Adding a red horizontal line at -2 and 2 
abline(h=c(-2,2), col="red")
###Identifying the observation numbers have an absolute studentized residual > 2 
which(abs(diag.model$stud.res) > 2)
###Plotting the Cook's distance for each observation 
plot(diag.model$cooks, xlab="Observation Number", ylab="Cook's Distance")
###Adding a red horizontal line at 1 
abline(h=c(1), col="red")
###Plotting the DFFITS for each observation 
plot(diag.model$dfits, xlab="Observation Number", ylab="DFITS statistics")
###Adding a red horizontal line at -1 and 1 
abline(h=c(1, -1), col="red")

library(car); vif(model.forward)

#### Homework ####

#Variables to keep:
#literacy, highincome, age35plus, ageunder25, scheduled, 
#obc, meangravida, complicbefore, anemia

#Goal is to find associations with maternal morbidity (morbid)
library(tidyverse)
mat_data <- read_csv(file = file.choose())

mat_data_slim <- mat_data %>%
  select(morbid, literacy, highincome, age35plus, 
         ageunder25, scheduled, obc, meangravida, complicbefore, anemia, lucknow) %>%
  drop_na() %>%
  #high income and lucknow are factors
  mutate(highincome = as.factor(highincome),
         lucknow = as.factor(lucknow)
         )

#EDA
ggplot(data = mat_data_slim, aes(x=morbid)) + geom_histogram(bins = 10, fill = "black") +
  ggtitle("Proportions of Severe Maternal Morbidity in the BetterBirth Dataset") + 
  xlab("Morbidity Proportions") + ylab("Count")

psych::describe(mat_data_slim) %>% select(n, mean, sd, median, range)

fullmodel<-glm(data=mat_data_slim, 
               morbid ~ .)

nullmodel<-glm(data=mat_data_slim, morbid ~ 1)

library(stats)

#start with null model for fwd selection, go from lower (null) to upper (all pred)
model.forward<-step(nullmodel, direction="forward", 
                    scope=list(upper = ~ literacy + 
                                 highincome + age35plus +
                                 ageunder25+ scheduled + obc +
                                 meangravida + complicbefore + anemia + lucknow,
                               lower = ~ 1))
summary(model.forward)

#bkwrd elim model from full model to null
model.backward<-step(fullmodel, 
                     direction="backward", 
                     scope=list(upper = ~ literacy + 
                                  highincome + age35plus +
                                  ageunder25+ scheduled + obc +
                                  meangravida + complicbefore + anemia + lucknow,
                                lower = ~ 1))
summary(model.backward)

#Compare the two
summary(model.backward)
summary(model.forward)

#95% CIs for model coefficients 
confint(model.forward)
confint(model.backward)

#model diagnostics

#diagnostic plots
library(ggfortify)
autoplot(model.backward)
autoplot(model.forward)

#outliers & influential points
diag.model.bwd<-ls.diag(model.backward)
diag.model.fwd<-ls.diag(model.forward)

which(diag.model.bwd$cooks > 1)
which(diag.model.fwd$cooks > 1)

#variance inflation factor, goal is <4
car::vif(model.forward)
car::vif(model.backward)

#Re-run without influential points, keep names the same
mat_data_slim <- mat_data %>%
  select(morbid, literacy, highincome, age35plus, 
         ageunder25, scheduled, obc, meangravida, complicbefore, anemia, lucknow) %>%
  drop_na() %>%
  #high income and lucknow are factors
  mutate(highincome = as.factor(highincome),
         lucknow = as.factor(lucknow)
  )
mat_data_slim <- mat_data_slim[-c(34,36),]

fullmodel<-glm(data=mat_data_slim, 
               morbid ~ .)
nullmodel<-glm(data=mat_data_slim, morbid ~ 1)
#start with null model for fwd selection, go from lower (null) to upper (all pred)
model.forward<-step(nullmodel, direction="forward", 
                    scope=list(upper = ~ literacy + 
                                 highincome + age35plus +
                                 ageunder25+ scheduled + obc +
                                 meangravida + complicbefore + anemia + lucknow,
                               lower = ~ 1))
#bkwrd elim model from full model to null
model.backward<-step(fullmodel, 
                     direction="backward", 
                     scope=list(upper = ~ literacy + 
                                  highincome + age35plus +
                                  ageunder25+ scheduled + obc +
                                  meangravida + complicbefore + anemia + lucknow,
                                lower = ~ 1))
#Compare the two
summary(model.backward)
summary(model.forward)

autoplot(model.backward)
autoplot(model.forward)

#outliers & influential points
diag.model.bwd<-ls.diag(model.backward)
diag.model.fwd<-ls.diag(model.forward)

#Cooks Distance
which(diag.model.bwd$cooks > 1)
which(diag.model.fwd$cooks > 1)
