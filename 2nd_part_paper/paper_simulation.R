# 11 20 cate simulation main
library(MASS)
library(CBPS)
library(ggplot2)
library(dplyr)
library(PSweight)
m = 2 # the number of covariance

# plz change alpha0 according to the value of gama and the following dict
alpha0 = 0.32

# alpha0 0.4 {1:-0.095, 2:0.13, 3：0.32 4：0.5}  
#        0.1 {1: -2, 2:-2.3 3: -2.75, 4:-3.25}  

alpha = matrix(c(-6, 1.5, -2), ncol = 1)
#alpha = matrix(c(0.15, 0.3, 0.3, -0.2, -0.25, -0.25), ncol = 1)

## Plz change alpha0 when u wanna change the value of gama
## 

alpha0_list_0.4 = c(-0.3, -0.4, -0.3, 1.7)
alpha0_list_0.2 = c(-1.205, -1.3, -1.53, -1.82)
alpha0_list_0.1 = c(-2, -2.3, -2.75, -3.25)
gama= 1

prob = 0.4

#beta = matrix(c(-0.5, -0.5, -1.5, 0.8, 0.8, 1), ncol = 1)
beta = matrix(c(-0.5, -1.5, -0.5), ncol = 1)
n <- 2000 # total size of the dataset 500 or 2000
n_list = c(500, 2000)

k= 2
a = 0.5
h = a * n ^(-1/4)
a1 = 0.2
h1 = a1 * n ^(-1/9)
l = k
item = 10000


for (n in c(500, 1000)){
for (gama in 1:3){
  alpha0.4=alpha0_list_0.4[gama]
  alpha0.2=alpha0_list_0.2[gama]
  alpha0.1=alpha0_list_0.1[gama]
  alpha0 = alpha0.2
  if (prob == 0.4)
    alpha0 = alpha0.4

  bias_logit = matrix(nrow = item, ncol = 5)
  bias_cbps1 = matrix(nrow = item, ncol = 5)
  bias_cbps2 = matrix(nrow = item, ncol = 5)
  bias_overlap = matrix(nrow = item, ncol = 5)
  bias_logit_m = matrix(nrow = item, ncol = 5)
  bias_cbps1_m = matrix(nrow = item, ncol = 5)
  bias_cbps2_m = matrix(nrow = item, ncol = 5)
  CATE = matrix(nrow = item, ncol = 5)
  for (i in 1: item){
    print(i)
    source('cate_dgp.R')
    tao_logit = EstCate(x = X, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "glm")
    tao_CBPS1 = EstCate(x = X, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "CBPS1")
    tao_CBPS2 = EstCate(x = X, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "CBPS2")
    tao_overlap = EstCate(x = X, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "OVERLAP")
    
    tao_logit_m = EstCate(x = Z, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "glm")
    tao_CBPS1_m = EstCate(x = Z, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "CBPS1")
    tao_CBPS2_m = EstCate(x = Z, y = Y, D=D, h=h, h1=h1, k=k, l=l, target_x = x1, ps_method = "CBPS2")
    CATE[i,] = cate
    
    bias_logit[i,] = tao_logit
    bias_cbps1[i,] = tao_CBPS1
    bias_cbps2[i,] = tao_CBPS2
    bias_overlap[i,] = tao_overlap
    
    bias_logit_m [i,] = tao_logit_m 
    bias_cbps1_m [i,] = tao_CBPS1_m 
    bias_cbps2_m [i,] = tao_CBPS2_m   
    }
  data = data.frame(bias_logit, bias_cbps1, bias_cbps2)
  logit_sd = (diag(t(as.matrix(bias_logit - colMeans(bias_logit))) %*% as.matrix(bias_logit - colMeans(bias_logit)))/item)^0.5
  cbps1_sd = (diag(t(as.matrix(bias_cbps1 - colMeans(bias_cbps1))) %*% as.matrix(bias_cbps1 - colMeans(bias_cbps1)))/item)^0.5
  cbps2_sd = (diag(t(as.matrix(bias_cbps2 - colMeans(bias_cbps2))) %*% as.matrix(bias_cbps2 - colMeans(bias_cbps2)))/item)^0.5
  #overlap_sd = (diag(t(as.matrix(overlap - colMeans(overlap))) %*% as.matrix(overlap - colMeans(overlap)))/item)^0.5
  sd = rbind(logit_sd,cbps1_sd,cbps2_sd)
  name_list = c('logit', 'cbps_over', 'cbps_exact')
  bias = data.frame(colMeans(bias_logit - CATE), colMeans(bias_cbps1 - CATE), colMeans(bias_cbps2 - CATE))
  names(bias) = name_list
  bias = t(as.matrix(bias))
  #std = diag((as.matrix(data-colMeans(data)) %*% t(as.matrix(data-colMeans(data)))  ) ** 0.5) / (item ** 0.5)
  rmse_logit = diag((t(as.matrix(bias_logit-CATE)) %*% as.matrix(bias_logit-CATE)) ** 0.5) / (item ** 0.5)
  rmse_over = diag((t(as.matrix(bias_cbps1-CATE)) %*% as.matrix(bias_cbps1-CATE)) ** 0.5) / (item ** 0.5)
  rmse_exact = diag((t(as.matrix(bias_cbps2-CATE)) %*% as.matrix(bias_cbps2-CATE)) ** 0.5) / (item ** 0.5)
  rmse_overlap = diag((t(as.matrix(bias_overlap-CATE)) %*% as.matrix(bias_overlap-CATE)) ** 0.5) / (item ** 0.5)
  rmse = rbind(rmse_logit, rmse_over, rmse_exact)
  
  
  data_m = data.frame(bias_logit_m, bias_cbps1_m, bias_cbps2_m)
  logit_sd = (diag(t(as.matrix(bias_logit_m - colMeans(bias_logit_m))) %*% as.matrix(bias_logit_m - colMeans(bias_logit_m)))/item)^0.5
  cbps1_sd = (diag(t(as.matrix(bias_cbps1_m - colMeans(bias_cbps1_m))) %*% as.matrix(bias_cbps1_m - colMeans(bias_cbps1_m)))/item)^0.5
  cbps2_sd = (diag(t(as.matrix(bias_cbps2_m - colMeans(bias_cbps2_m))) %*% as.matrix(bias_cbps2_m - colMeans(bias_cbps2_m)))/item)^0.5
  #overlap_sd = (diag(t(as.matrix(overlap - colMeans(overlap))) %*% as.matrix(overlap - colMeans(overlap)))/item)^0.5
  sd_m = rbind(logit_sd,cbps1_sd,cbps2_sd)
  name_list_m = c('logit_m', 'cbps_over_m', 'cbps_exact_m')
  bias_m = data.frame(colMeans(bias_logit_m - CATE), colMeans(bias_cbps1_m - CATE), colMeans(bias_cbps2_m - CATE))
  names(bias_m) = name_list_m
  bias_m = t(as.matrix(bias_m))
  #std = diag((as.matrix(data-colMeans(data)) %*% t(as.matrix(data-colMeans(data)))  ) ** 0.5) / (item ** 0.5)
  rmse_logit_m = diag((t(as.matrix(bias_logit_m-CATE)) %*% as.matrix(bias_logit_m-CATE)) ** 0.5) / (item ** 0.5)
  rmse_over_m = diag((t(as.matrix(bias_cbps1_m-CATE)) %*% as.matrix(bias_cbps1_m-CATE)) ** 0.5) / (item ** 0.5)
  rmse_exact_m = diag((t(as.matrix(bias_cbps2_m-CATE)) %*% as.matrix(bias_cbps2_m-CATE)) ** 0.5) / (item ** 0.5)
  #rmse_overlap = diag((t(as.matrix(bias_overlap-CATE)) %*% as.matrix(bias_overlap-CATE)) ** 0.5) / (item ** 0.5)
  rmse_m = rbind(rmse_logit_m, rmse_over_m, rmse_exact_m)
  
  filename = paste('fanLi_', as.character(item),'gama = ', as.character(gama), 'alpha0 = ', as.character(alpha0),
                   '_num=', as.character(n),'.csv',sep = '')
  ans = rbind(as.matrix(bias),as.matrix(sd),as.matrix(rmse),as.matrix(bias_m),as.matrix(sd_m),as.matrix(rmse_m))
 # ans_m = rbind(as.matrix(bias_m),as.matrix(sd_m),as.matrix(rmse_m))
  ans = data.frame(ans)
  #ans_m = data.frame(ans_m)
  View(ans)
  print(filename)
  write.csv(ans, filename)
}}
