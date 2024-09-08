#重みの変化でリーチングタイムにどのような結果の違いが出るか2-way ANOVAおよびpairwise検定で検証
library(MASS)
data = read.csv("result_edit_1.csv")

#独立変数の数字を文字列に変換(これを行わないと検定結果がおかしくなる)
for (i in 1:60){
  data$rate_hmd1_x[i] = as.character(data$rate_hmd1_x[i])
  data$Rotate_of_other_hmd[i] = as.character(data$Rotate_of_other_hmd[i])
}

#箱ひげ図
par(mfrow=c(1,2))
boxplot(average_of_time ~ rate_hmd1_x, data = data, xlab = "rate of HMD1 in horizontal direction", ylab = "average of time")
boxplot(average_of_time ~ Rotate_of_other_hmd, data = data, xlab = "visual condition", ylab = "average of time")
par(mfrow=c(1,1))

#相互作用プロット
attach(data)
par(mfrow=c(1,2))
interaction.plot(rate_hmd1_x, Rotate_of_other_hmd, average_of_time, xlab = "rate of HMD1 in horizontal direction", ylab = "average of time")
interaction.plot(Rotate_of_other_hmd, rate_hmd1_x, average_of_time, xlab = "visual condition", ylab = "average of time")
par(mfrow=c(1,1))

#多次元ANOVA
aov(average_of_time ~ rate_hmd1_x * Rotate_of_other_hmd, data=data)
data.aov <- aov(average_of_time ~ rate_hmd1_x * Rotate_of_other_hmd, data=data)
summary(data.aov)

#交互作用のない分散分析
aov(average_of_time ~ rate_hmd1_x + Rotate_of_other_hmd, data=data)
data.aov2 = aov(average_of_time ~ rate_hmd1_x + Rotate_of_other_hmd, data=data)
summary(data.aov2)

#モデル診断(解析の妥当性を確認)
par(mfrow=c(2,2))
plot(data.aov2)
par(mfrow=c(1,1))

#pairwise検定
pairwise.t.test(average_of_time, rate_hmd1_x, p.adj = "none")

#boxcox変換
bxcx = boxcox(average_of_time+1 ~ rate_hmd1_x * Rotate_of_other_hmd, data=data, lambda=seq(-0.05, 0.45, len=20))
lambda = bxcx$x[which.max(bxcx$y)]
lambda

#boxcox変換後の分散分析
data.aov3 = aov(((average_of_time+1)^lambda-1)/lambda ~ rate_hmd1_x * Rotate_of_other_hmd, data=data)
summary(data.aov3)

#boxcox変換後のモデル診断
par(mfrow=c(2,2))
plot(data.aov3)
par(mfrow=c(1,1))

#知覚についての検定