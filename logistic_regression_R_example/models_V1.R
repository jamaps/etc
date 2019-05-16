# this is some code used to run binary and ordered logistic regression models
# it was applied to analyzing activity participation by post-secondary students
# for more info on this project, read the paper here https://doi.org/10.1016/j.tbs.2018.08.003 
# or email jeff.allen@mail.utoronto.ca for mor info




# load packages

library(pscl)
library(ggplot2)
library(MASS)
require(foreign)
require(Hmisc)
require(reshape2)
library(erer)

setwd("~")

# some data prep stuff

df <- read.csv("ready_data_v3.csv", na.strings=c("","NA"))
df <- subset(df, df$ps_commute_main_cat != "Other")

df$t_mode_travel_time[df$t_mode_travel_time > 120] <- 120
df <- na.omit(df)

df$ps_status_paste <- paste(df$ps_ersonstatustime,df$ps_personstatusgrad)
df$ps_status_paste[df$ps_status_paste == "FT Grad"] <- "C FT Grad"
df$ps_status_paste[df$ps_status_paste == "FT UG"] <- "A FT UG"
df$ps_status_paste[df$ps_status_paste == "Other Other"] <- "E Other"
df$ps_status_paste[df$ps_status_paste == "PT Grad"] <- "D PT Grad"
df$ps_status_paste[df$ps_status_paste == "PT UG"] <- "B PT UG"
df <- subset(df, df$ps_status_paste != "E Other")

df$t_r_walk_ratio <- (df$t_transit_walk_time / df$t_total_transit_time)
df$t_r_wait_ratio <- (df$t_transit_initial_wait_time + df$t_transfer_time) / df$t_total_transit_time

df$d_sum_3 <- df$d_commutediscouragecomingcampus + df$d_commutediscparticipunivacts + df$d_pickupcoursesaccordtocommute

df$ps_com_freq_5 <- 0
df$ps_com_freq_5[df$ps_frequencycommute < 3] <- 2
df$ps_com_freq_5[df$ps_frequencycommute == 3] <- 3
df$ps_com_freq_5[df$ps_frequencycommute == 4] <- 4
df$ps_com_freq_5[df$ps_frequencycommute == 5] <- 5
df$ps_com_freq_5[df$ps_frequencycommute > 5] <- 6

df$ps_commute_main_cat_reorder <- ""
df$ps_commute_main_cat_reorder[df$ps_commute_main_cat == "Walk"] <- "aWalk"
df$ps_commute_main_cat_reorder[df$ps_commute_main_cat == "Bike"] <- "bBike"
df$ps_commute_main_cat_reorder[df$ps_commute_main_cat == "Transit"] <- "cTransit"
df$ps_commute_main_cat_reorder[df$ps_commute_main_cat == "Rideshare"] <- "dRideshare"
df$ps_commute_main_cat_reorder[df$ps_commute_main_cat == "Solo Driver"] <- "eDrive"


df$t_transit_walk_cat <- 0
df$t_transit_walk_cat[df$t_transit_walk_time > 30] <- 30
table(df$t_transit_walk_cat)
df$t_transit_iwait_cat <- 0
df$t_transit_iwait_cat[df$t_transit_initial_wait_time > 10] <- 10
table(df$t_transit_iwait_cat)

df$constant <- 1

df$t_transfers_new <- df$t_transfers + 1
df$t_transfers_new[df$ps_commute_main_cat == "Walk"] <- 0
df$t_transfers_new[df$ps_commute_main_cat == "Bike"] <- 1
df$t_transfers_new[df$ps_commute_main_cat == "Rideshare"] <- 1
df$t_transfers_new[df$ps_commute_main_cat == "Solo Driver"] <- 1

df$t_bike_faster_than_transit <- 0
df$t_bike_faster_than_transit[(df$t_bike_time + 2) < df$t_total_transit_time] <- 1



#################################
# binary logistic regression
#################################

# Y1 - Does your commute sometimes discourage you from coming to campus? 

table(df$d_commutediscouragecomingcampus)
m <- glm(df$d_commutediscouragecomingcampus~
           df$ps_cmpgendershortname+
           df$ps_age+
           #df$ps_status_paste+
           df$ps_campus_main+
           df$ps_work_3_cat+
           df$ps_commute_main_cat_reorder+
           #df$ntrips+
           df$t_transfers_new+
           #df$ps_transitpassownerflag+
           #df$hh_depchild_cat3.1+
           df$t_mode_travel_time,
         family=binomial(link='logit'))
summary(m)
Lb <- logLik(m)

exp(cbind(coef(m), confint(m))) # odds rataios and conf int
o <- cbind(exp(coef(m)),summary(m)$coefficients[,4])
write.csv(o, "m1_dis_to_campus.csv")


mc <- glm(df$d_commutediscouragecomingcampus~
            df$constant
          , 
          family=binomial(link='logit'))
summary(mc)
Lc <- logLik(mc)

pho <- 1 - Lb / Lc
phobar <- 1 - (Lb - 16) / Lc
Lb
pho
phobar




# Y2
table(df$d_pickupcoursesaccordtocommute)
m <- glm(df$d_pickupcoursesaccordtocommute~
           df$ps_cmpgendershortname+
           df$ps_age+
           df$ps_status_paste+
           df$ps_campus_main+
           df$ps_work_3_cat+
           df$ps_commute_main_cat_reorder+
           #df$ntrips+
           #df$t_transfers_new+
           #df$ps_transitpassownerflag+
           #df$hh_numdependentchildren+
           df$t_mode_travel_time,
         family=binomial(link='logit'))
summary(m)
Lb <- logLik(m)

exp(cbind(coef(m), confint(m))) # odds rataios and conf int
o <- cbind(exp(coef(m)),summary(m)$coefficients[,4])
write.csv(o, "m2_pick_courses.csv")


mc <- glm(df$d_pickupcoursesaccordtocommute~
            df$constant
          , 
          family=binomial(link='logit'))
summary(mc)
Lc <- logLik(mc)

pho <- 1 - Lb / Lc
phobar <- 1 - (Lb - 18) / Lc
Lb
pho
phobar



#Y3
table(df$d_commutediscparticipunivacts)
m <- glm(df$d_commutediscparticipunivacts~
           df$ps_cmpgendershortname+
           df$ps_age+
           #df$ps_status_paste+
           #df$ps_campus_main+
           #df$ps_work_3_cat+
           df$ps_commute_main_cat_reorder+
           #df$ntrips+
           #df$t_transfers_new+
           #df$ps_transitpassownerflag+
           #df$hh_numdependentchildren+
           df$t_mode_travel_time,
         family=binomial(link='logit'))
summary(m)
Lb <- logLik(m)
coef(summary(m))

exp(cbind(coef(m), confint(m))) # odds rataios and conf int
o <- cbind(exp(coef(m)),summary(m)$coefficients[,4])
write.csv(o, "m3_oncamp_act_dis.csv")



mc <- glm(df$d_commutediscparticipunivacts~
            df$constant
          , 
          family=binomial(link='logit'))
summary(mc)
Lc <- logLik(mc)

pho <- 1 - Lb / Lc
phobar <- 1 - (Lb - 7) / Lc
Lb
pho
phobar




#############################
# ordred logistic regression
#############################


# for number of days students commute to campus per week
table(df$ps_com_freq_5) / 2011
crapo <- df
m <- polr(as.factor(crapo$ps_com_freq_5)~
            crapo$ps_cmpgendershortname+
            crapo$ps_age+
            crapo$ps_status_paste+
            crapo$ps_campus_main+
            crapo$ps_work_3_cat+
            crapo$ps_commute_main_cat_reorder+
            #crapo$aHome+
            crapo$ntrips+
            #df$aOther+
            #df$aVist+
            #df$t_transfers_new+
            crapo$ps_transitpassownerflag+
            #df$hh_numdependentchildren+
            crapo$hh_livingsituation+
            crapo$t_mode_travel_time
          , 
          Hess=TRUE, method = "logistic")
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
Lb <- logLik(m)
ctable <- ctable[,c("Value","p value")]
write.csv(ctable, file="Yt2.csv")
mef <- ocME(m)
write.csv(mef$out$ME.all, file="Yt2mef.csv")


mc <- polr(as.factor(df$ps_com_freq_5)~
             df$constant
           , 
           Hess=TRUE, method = "logistic")
Lc <- logLik(mc)

pho <- 1 - Lb / Lc
phobar <- 1 - (Lb - 21) / Lc
Lb
pho
phobar
#1 - Lb / Lc



###########################################
# OLS model for time spent time on campus
##########################################

dfs <- subset(df, df$aSchool > 0)
# dfs <- subset(dfs, dfs$aWork > 0)
hist(dfs$aSchool / 60)
m <- lm(dfs$aSchool ~ 
          #dfs$ps_work_3_cat + 
          dfs$ps_commute_main_cat_reorder +
          dfs$t_mode_travel_time+
          #dfs$ps_cmpgendershortname+
          dfs$aWork+
          dfs$ntrips
)
summary(m)
plot(dfs$aSchool ~ dfs$t_mode_travel_time)




### some additional stats needed for the paepr
# desc transprotation
dfs <- subset(df, df$ps_commute_main_cat == "Transit")
dfs <- subset(dfs, dfs$t_transfers_new > 2)

dfs <- subset(df, df$aSchool > 0)
dfs <- subset(dfs, dfs$aWork > 0)

dfs <- subset(df, df$ps_commute_main_cat == "Bike")
mean(dfs$t_bike_time)




