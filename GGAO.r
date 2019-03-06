library(data.table)
library(ggplot2)
dtAttacker <- fread("C:/Users/Guest User/Desktop/New folder (2)/attackers_model_data.csv")
touch<-dtAttacker[!(Position %in% c("Centre Forward","Second Striker")),.(firstName,lastName,team,Position,Goal,IntentAst,`TakeOn%`,`OnTarget%`,`Performance Winger nxt`),by=.(TchsA3)][order(TchsA3)]

dtAttackerWing <- dtAttacker[, - c('Rank','Salary','playerId', 'key','Salary', 'optaPersonId', 
                                   'firstName', 'lastName', 'player', 'team', 'teamId',
                                   'optaTeamId', 'leagueId', 'league', 'Normalised Goals',
                                   'Crosses Normalised', 'Performance Winger', 'Goals nxt', 'flag')][, - (1:3)]
dtAttacker[,`Performance Winger nxt`,by=.(TchsA3)]

dtAttackerWing = dtAttackerWing[!(Position %in% c("Centre Forward","Second Striker"))]
# for attackers only Center Forward and Second Striker are measured by expected goal



dtAttackerWing$Position =factor(dtAttackerWing$Position)
dtAttackerWing[, `Ps%InA3rd`:= as.numeric(substr(`Ps%InA3rd`, 1, nchar(`Ps%InA3rd`) - 1)) / 100]
dtAttackerWing[, `TakeOn%` := as.numeric(substr(`TakeOn%`, 1, nchar(`TakeOn%`) - 1)) / 100]
dtAttackerWing[, `TakeOn%A3` := as.numeric(substr(`TakeOn%A3`, 1, nchar(`TakeOn%A3`) - 1)) / 100]
dtAttackerWing[, `OnTarget%` := as.numeric(substr(`OnTarget%`, 1, nchar(`OnTarget%`) - 1)) / 100]
dtAttackerWing[, `Aerial%A3` := as.numeric(substr(`Aerial%A3`, 1, nchar(`Aerial%A3`) - 1)) / 100]
dtAttackerWing[, `GrndDuelOppHalfSuccess%` := as.numeric(substr(`GrndDuelOppHalfSuccess%`, 1, nchar(`GrndDuelOppHalfSuccess%`) - 1)) / 100]

# impute missing value
#colnames(dtAttackerWing)[sapply(1:ncol(dtAttackerWing), function(x) ifelse(sum(is.na(dtAttackerWing[, ..x]) > 0), x, NA))]

dtAttackerWing[, `Ps%InA3rd` := ifelse(is.na(`Ps%InA3rd`), median(`Ps%InA3rd`, na.rm = TRUE), `Ps%InA3rd`)]
dtAttackerWing[, `TakeOn%` := ifelse(is.na(`TakeOn%`), median(`TakeOn%`, na.rm = TRUE), `TakeOn%`)]
dtAttackerWing[, `TakeOn%A3` := ifelse(is.na(`TakeOn%A3`), median(`TakeOn%A3`, na.rm = TRUE), `TakeOn%A3`)]
dtAttackerWing[, `OnTarget%` := ifelse(is.na(`OnTarget%`), median(`OnTarget%`, na.rm = TRUE), `OnTarget%`)]
dtAttackerWing[, `Aerial%A3` := ifelse(is.na(`Aerial%A3`), median(`Aerial%A3`, na.rm = TRUE), `Aerial%A3`)]
dtAttackerWing[, `GrndDuelOppHalfSuccess%` := ifelse(is.na(`GrndDuelOppHalfSuccess%`), median(`GrndDuelOppHalfSuccess%`, na.rm = TRUE), `GrndDuelOppHalfSuccess%`)]
apply(dtAttackerWing[,2:29],2,as.numeric)

#get correlation and find high-correlated pairs
cor <- as.dataframe((cor(dtAttackerWing[,2:29])))
cool<-apply(cor,2,function(x) ifelse(x>0.85&x<1,"A",x))
index <- which(cool=="A", arr.ind=TRUE)

# we decided to put away xA, ShotExcBlk, xG, Position, IntentAst and Goal 
lmAttackerWing <- lm(`Performance Winger nxt` ~ .-xA -ShotExcBlk -xG -Position -IntentAst -Goal ,data = dtAttackerWing)

stepAttackerWing <- step(lmAttackerWing)


summary(stepAttackerWing)

# What's the relationship between the touches and predicted performance on wingers 
ggplot(dtAttackerWing, aes(TchsA3, `Performance Winger nxt`)) +
  geom_point(aes(color = Position)) +
  geom_smooth(method ="lm") +
  theme_bw()

