#Rotationの効果があるか検証
library(reshape)
library(MASS)
data = read.csv("result_edit_1.csv")
#比率を文字列に変換
for (i in 1:60){
  data$rate_hmd1_x[i] = as.character(data$rate_hmd1_x[i])
}

for (j in 1:5){
  horizonal = data$average[(12*(j-1) + 1) : (12*j - 6)]
  vertical = data$average[(12*(j-1) + 7) : (12*j)]
  
  test = t.test(horizonal, vertical, alternative = "two.sided")
  print(test)
}