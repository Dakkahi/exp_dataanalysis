#重みの変化で知覚にどのような結果の違いが出るか2-way ANOVAおよびpairwise検定で検証
library(MASS)
data = read.csv("result_survey.csv")

#独立変数の数字を文字列に変換(これを行わないと検定結果がおかしくなる)
for (i in 1:60){
  data$rate_hmd1_x[i] = as.character(data$rate_hmd1_x[i])
}

#箱ひげ図
attach(data)
boxplot(feel ~ rate_hmd1_x, data = data, xlab = "rate of HMD1 in horizontal direction", ylab = "feel")


#1次元ANOVA
aov(feel ~ rate_hmd1_x)
data.aov <- aov(feel ~ rate_hmd1_x, data =data)
summary(data.aov)

#モデル診断(解析の妥当性を確認)
par(mfrow=c(2,2))
plot(data.aov)
par(mfrow=c(1,1))

#pairwise検定
pairwise.t.test(feel, rate_hmd1_x, p.adj = "none")

#boxcox変換
bxcx = boxcox(feel+1 ~ rate_hmd1_x, lambda=seq(-0.05, 10, len=100))
lambda = bxcx$x[which.max(bxcx$y)]
lambda

#boxcox変換後の分散分析
data.aov2 = aov(((feel+1)^lambda-1)/lambda ~ rate_hmd1_x)
summary(data.aov2)

#boxcox変換後のモデル診断
par(mfrow=c(2,2))
plot(data.aov2)
par(mfrow=c(1,1))

#boxcox後のpairwise検定
pairwise.t.test(((feel+1)^lambda-1)/lambda, rate_hmd1_x, p.adj = "none")
