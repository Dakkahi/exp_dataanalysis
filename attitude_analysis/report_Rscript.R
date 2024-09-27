#ggmを使う準備
library("ggm")

#データの読み込み
data <-read.csv("attitude_R.csv",header=T)
data

#基本統計量
summary(data)

#相関行列
R <- round(cor(data),3)

#散布図
pairs(data)

#偏相関行列を出力するプログラム
X <- solve(R)
d <- sqrt(diag(X))
P <- -X / (d %*% t(d))
diag(P) <- 1
P 

round(P,3)

#fitConGraphによる相関,AICの計算
#options(digits=3)
X <- data
n <- nrow(X); p <- ncol(X)
R <- cor(X)
amat <- matrix(1,p,p)-diag(p)

#グラフの辺のうち偏相関の絶対値が最小となる辺を求める
select.ij <- function(P,amat){
  p <- nrow(P); minabsP <- Inf
  for(i in (2:p)){
    for(j in (1:(i-1))){
      if(amat[i,j] == 1 && abs(P[i,j]) < minabsP){
        minabsP <- abs(P[i,j]); i0 <- i; j0 <- j
      }}}
  c(i0,j0) }

#最小を求める
select.ij(P,amat)

#変数名をコピー
dimnames(amat) <- dimnames(R)

#辺(2,4)を削除
amat[4,2] <- amat[2,4] <- 0
amat

#最尤法
f <- fitConGraph(amat,R,n)
f


#AIC
aic <- f$dev - 2*f$df
aic


#共分散選択アルゴリズム
graph <- function(R){

X <-data
n <- nrow(X); p <- ncol(X)
R <-cor(X)
amat <- matrix(1,p,p)-diag(p);
result <- 1
aic <- 0
print_mat <- diag(n)
M <- round(R,3)
while(aic < result){
  result <- aic
  M_inv <- solve(M)#ここで逆行列が存在しないとエラーを吐かれる
  d <- sqrt(diag(M_inv))
  P <- -M_inv / (d %*% t(d))
  diag(P) <- 1
  i <- select.ij(P,amat)[1]
  j <- select.ij(P,amat)[2]
  dimnames(amat) <- dimnames(R)
  amat[i,j] <- amat[j,i] <- 0
  f <- fitConGraph(amat,R,n)
  M <- f$Shat
  aic <- f$dev - 2*f$df
  if(aic < result){
    show(i)
    show(j)
    show(aic)
    print_mat <- M
  }
  else{
    amat[i,j] <- amat[j,i] <- 1
    M_inv_print <- solve(print_mat)
    d <- sqrt(diag(M_inv_print))
    P_print <- -M_inv_print / (d %*% t(d))
    diag(P_print) <- 1
    show(print_mat)
    show(P_print)
  }
}
}

drawGraph(amat,adjust=FALSE)