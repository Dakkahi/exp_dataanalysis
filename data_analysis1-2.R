#重みの変化でどのような結果の違いが出るか検証
library(reshape)
library(MASS)
data = read.csv("result_edit_1-2.csv")
#比率を文字列に変換
#for (i in 1:30){
#  data$rate_hmd1_x[i] = as.character(data$rate_hmd1_x[i])
#}
boxplot(average ~ rate_hmd1_x, data = data, xlab = "rate of HMD1 in x", ylab = "average of time", main = "boxplot")

attach(data)
tapply(average, rate_hmd1_x, summary)
data_aov = aov(average ~ rate_hmd1_x, data = data)
summary(data_aov)
#仮説検定の諸々の結果を出力
alpha = 0.05
qf(alpha, df1=4, df2=55, lower.tail=F)
pf(8.311, df1=4, df2=55, lower.tail=F)

#モデル診断
par(mfrow = c(2,2))
plot(data_aov)
par(mfrow = c(1,1))

#TukeyHSD
data_tukey = TukeyHSD(data_aov)
data_tukey
plot(data_tukey, cex.axis = 0.7)

#boxcox
bxcx = boxcox((average + 1) ~ rate_hmd1_x, data=data, lambda=seq(-0.1, 0.7, len=20))
lambda = bxcx$x[which.max(bxcx$y)]

#boxcox後のANOVA
data_aov2 <- aov(((average + 1)^lambda - 1)/lambda ~ rate_hmd1_x, data=data)
summary(data_aov2)

#モデル診断
par(mfrow = c(2,2))
plot(data_aov2)
par(mfrow = c(1,1))