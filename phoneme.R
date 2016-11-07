library(splines)
library(MASS)

data = read.table("C:\\Users\\Shuaiwen\\Desktop\\phoneme.data", sep=",", header=T)[, -1]

n = dim(data)[1]
p = dim(data)[2] - 2

set.seed(124)
index = sample(n, (n * 3) %/% 4, replace = F)
tr_X = as.matrix(data[index, 1 : 256])
te_X = as.matrix(data[-index, 1 : 256])
tr_y = as.vector(data[index, 257])
te_y = as.vector(data[-index, 257])

M = 12
knots = as.integer(seq(1, 256, length.out = M + 2))

H = ns(x = 1:256, knots = knots[2 : (M + 1)])

tr_X_smooth = tr_X %*% H
te_X_smooth = te_X %*% H

pca_viz <- function(X, label, save = NA){
  pca_X = princomp(X)  # pca of X
  X_std = t((t(X) - pca_X$center) / pca_X$scale)  # standardize X
  coordinate = X_std[, ] %*% pca_X$loadings[, 1:2]
  
  label = factor(label)
  label_level = levels(label)  # find all the labels
  k = length(label_level)

  plot(1, 1, type="n", xlim=range(coordinate[, 1]), ylim=range(coordinate[, 2]))  # start a new plot
  col = rainbow(k)
  for (i in 1 : k){
    lvl = label_level[i]
    temp_index = which(label == lvl)
    lines(coordinate[temp_index, 1], coordinate[temp_index, 2], type="p", col=col[i])    
  }
  legend("topright", legend = label_level, col = col, pch=1)
}

run_qda <- function(tr_X, tr_y, te_X, te_y){
  
  fit <- qda(x = tr_X, grouping = tr_y, method = "moment")
  te_predict = predict(fit, newdata = te_X)
  te_y_predict = te_predict$class
  
  err_rate = sum(te_y_predict != te_y) / length(te_y)
  return(err_rate)
}

qda_original_fit = run_qda(tr_X, tr_y, te_X, te_y)
qda_smooth_fit = run_qda(tr_X_smooth, tr_y, te_X_smooth, te_y)

