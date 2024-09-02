library(readxl)
library(MASS)
library(AER)

data <- read_excel("C:/Users/kkagd/OneDrive/Desktop/Project Moljut/international_matches.xlsx",sheet="Used Data")
View(data)
summary(data)

#Uji Poisson
poisson<- glm(Goals~Region+Rank_Diff+Rank_Avg+Netral+Penalty, data=data, family=poisson(link="log"))
summary(poisson)
vif(poisson)

#Uji overdispersi
mean(data$Goals)
var(data$Goals)
chisq.test(data)

#Uji excess zero
hist(data$Goals, main="Histogram Variabel Respon Gol", xlab="Jumlah Gol pada Pertandingan", ylab="Frekuensi")

#Uji Negative Binomial
nb1<-glm.nb(Goals~Region+Rank_Diff+Rank_Avg+Netral+Penalty,data=data)
summary(nb1)
vif(nb1)
BIC(nb1)

nb2<-glm.nb(Goals~Region+Rank_Diff+Rank_Avg+Penalty,data=data)
summary(nb2)
BIC(nb2)

nb3<-glm.nb(Goals~Region+Rank_Diff+Netral+Penalty,data=data)
summary(nb3)
BIC(nb3)

nb4<-glm.nb(Goals~Rank_Diff+Rank_Avg+Netral+Penalty,data=data)
summary(nb4)
BIC(nb4)

nb5<-glm.nb(Goals~Region+Rank_Diff+Penalty,data=data)
summary(nb5)
BIC(nb5)

nb6<-glm.nb(Goals~Rank_Diff+Rank_Avg+Penalty,data=data)
summary(nb6)
BIC(nb6)

nb7<-glm.nb(Goals~Rank_Diff+Netral+Penalty,data=data)
summary(nb7)
BIC(nb7)

nb8<-glm.nb(Goals~Rank_Diff+Penalty,data=data)
summary(nb8)
BIC(nb8)

#Residual plot 
nb_res <- resid(nb8)
plot(fitted(nb8), nb_res, col='steelblue', pch=16,
     xlab='Predicted Goals', ylab='Standardized Residuals', main='Negative Binomial', xlim=c(1,4), ylim=c(-4,4))
abline(0,0)

#Confidence Interval
exp(cbind(Estimate=coef(nb8),confint(nb8)))