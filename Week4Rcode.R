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
# Prop of female lit in district, literacy
# higher-income district, highincome (1=high)
# Geo location in central study hub, lucknow (1=in central hub)
# Prop pts. >= 35 years, age35plus
# Mean age birth attendants, baage
# Years since last training, balasttrained
# Years of exp., baexperience
# Prev neonatal mortality at the facility level, died0sbrbaseline
# Prop c-section, csection
# >=50% deliveries attended by >=1 aux nurse midwife, anmonlybin (1=yes)

#List of variables:
#literacy, highincome, lucknow, age35plus, baage, balasttrained, baexperience, 
#died0sbrbaseline, csection, anmonlybin
#Goal is to find associations with maternal morbidity (morbid)
library(tidyverse)
mat_data <- read_csv(file = file.choose())

mat_data_slim <- mat_data %>%
  select(morbid, literacy, highincome, lucknow, 
         age35plus, baage, balasttrainedln, baexperienceln,
         died0sbrbaseline, csectionln, anmonlybin) %>%
  drop_na() %>%
  #highincome, lucknow, anmonlybin are factors
  mutate(highincome = as.factor(highincome),
   lucknow = as.factor(lucknow),
   anmonlybin = as.factor(anmonlybin))

#correlation matrix
#cor(syncmov2[,c("PainTolerance", "PainToleranceBefore","CloseDiff")], use = "complete.obs")
#Add correlation matrix here

#For now, run these all without interactions.
#Consider running a model with all interactions first and then adding those interaction terms
#to the fwd/backward elim models

fullmodel<-glm(data=mat_data_slim, 
               morbid ~ .)

nullmodel<-glm(data=mat_data_slim, morbid ~ 1)

library(stats)

#start with null model for fwd selection, go from lower (null) to upper (all pred)
model.forward<-step(nullmodel, direction="forward", 
                    scope=list(upper = ~ literacy + highincome + lucknow + age35plus + 
                                 baage + balasttrainedln + baexperienceln + died0sbrbaseline +
                                 csectionln + anmonlybin,
                               lower = ~ 1))
summary(model.forward)

#bkwrd elim model from full model to null
model.backward<-step(fullmodel, 
                     direction="backward", 
                     scope=list(upper = ~ literacy + highincome + lucknow + age35plus + 
                                  baage + balasttrainedln + baexperienceln + died0sbrbaseline +
                                  csectionln + anmonlybin,
                                lower = ~ 1))
summary(model.backward)

#Compare the two
summary(model.backward)
summary(model.forward)

#model diagnostics
library(ggfortify);autoplot(model.backward)

diag.model<-ls.diag(model.backward)
names(diag.model)
