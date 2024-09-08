#Rotationの効果があるか検証
library(MASS)
data = read.csv("result_survey.csv")
#比率を文字列に変換
for (i in 1:120){
  data$rate_hmd1_x[i] = as.character(data$rate_hmd1_x[i])
}

attach(data)
tapply(feel, rate_hmd1_x, summary)
#ANOVA
boxplot(feel ~ rate_hmd1_x, data = data, xlab = "rate of HMD1 in x", ylab = "feeling", main = "boxplot")
data_aov = aov(feel ~ rate_hmd1_x, data = data)
summary(data_aov)

alpha = 0.05
qf(alpha, df1=4, df2=115, lower.tail=F)
pf(3.982, df1=4, df2=115, lower.tail=F)

#モデル診断
par(mfrow = c(2,2))
plot(data_aov)
par(mfrow = c(1,1))

#TukeyHSD
data_tukey = TukeyHSD(data_aov)
data_tukey
plot(data_tukey, cex.axis = 0.7)
