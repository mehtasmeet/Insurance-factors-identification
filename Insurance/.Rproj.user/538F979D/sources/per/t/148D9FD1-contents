df <- read.csv("data.csv")
View(df)

dim(df)

#--------------------------------------------

summary(df)
#--------------------------------------------

cor(df$Claims,df$Payment) #--high +ve correlation

cor(df$Insured,df$Payment) #--high +ve correlation

plot(df$Insured,df$Payment) 

plot(df$Claims,df$Payment) 

#----------------------------------------------

lineModel <- lm(Payment ~ ., data = df)
summary(lineModel)

#----------------------------------------------

?apply
ZoneResult <- apply(df[,c(5,6,7)],2, function(x)tapply(x, df$Zone, mean))
ZoneResult

KmResult <- apply(df[,c(5,6,7)],2, function(x)tapply(x, df$Kilometres, mean))
KmResult

BonusResult <- apply(df[,c(5,6,7)],2, function(x)tapply(x, df$Bonus, mean))
BonusResult

#----------------------------------------------------

md <- lm(df$Claims ~ df$Kilometres + df$Zone + df$Bonus + df$Make + df$Insured) 

summary(md)
