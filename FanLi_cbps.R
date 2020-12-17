library(grf)
library(CBPS)
library(PSweight)

## Not run:
# Save the plot of a tree in the causal forest.
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
library(DiagrammeR)
library(DiagrammeRsvg)
####generate simulational data#####
# maybe we can use the data structure in susan(2017)
# so the susan's simulation data structure are copied at following:

library(MASS)
m = 6 # the number of covariance

# plz change alpha0 according to the value of gama and the following dict
alpha0 = 0.32

alpha0_list_0.4 = c(-0.095, 0.13, 0.32, 0.5)
alpha0_list_0.2 = c(-1.205, -1.3, -1.53, -1.82)
alpha0_list_0.1 = c(-2, -2.3, -2.75, -3.25)
# alpha0 0.4 {1:-0.095, 2:0.13, 3：0.32 4：0.5}  
#        0.1 {1: -2, 2:-2.3 3: -2.75, 4:-3.25}  
heteo = TRUE
heteo_list = c(F, T)
treat_size = 0.5
alpha = matrix(c(0.15, 0.3, 0.3, -0.2, -0.25, -0.25), ncol = 1)

## Plz change alpha0 when u wanna change the value of gama
## 
gama=3
prob = 0.2
alpha0.4=alpha0_list_0.4[gama]
alpha0.2=alpha0_list_0.2[gama]
alpha0.1=alpha0_list_0.1[gama]
alpha0 = alpha0.2
if (prob == 0.4)
  alpha0 = alpha0.4
beta = matrix(c(-0.5, -0.5, -1.5, 0.8, 0.8, 1), ncol = 1)
nitem = 100
A = matrix(rep(0.5, 36),nrow=m,ncol=m)
for (i in 1: 6)
  A[i,i] = 1
covariance = A 
n <- 2000 # total size of the dataset 500 or 2000
n_list = c(500, 2000)
set.seed(123)
for (gama in 1:4){
  alpha0.4=alpha0_list_0.4[gama]
  alpha0.2=alpha0_list_0.2[gama]
  alpha0.1=alpha0_list_0.1[gama]
  alpha0 = alpha0.2
  if (prob == 0.4)
    alpha0 = alpha0.4
  if (prob == 0.1)
    alpha0 = alpha0.1
#for (heteo in heteo_list){
#for (n in n_list){
  
  filename = paste(sep = '', Sys.Date(), '_', 'Fanli_', 'item_num=', as.character(nitem), '_num=', as.character(n), '_gama=', as.character(gama),
                   '_alpha0=', as.character(alpha0), '_heterog=', as.character(heteo), '.csv')
  print(filename)
  ave_bias = list()
  ave_bias_weight = list()
  ave_bias_cbps = list()
  ave_bias_cbps_overlap = list()
  ave_bias_m = list()
  ave_bias_weight_m = list()
  ave_bias_cbps_m = list()
  ave_bias_cbps_overlap_m = list()
  
  bias = list()
  bias_weight = list()
  bias_cbps = list()
  bias_cbps_overlap = list()
  bias_m = list()
  bias_weight_m = list()
  bias_cbps_m = list()
  bias_cbps_overlap_m = list()
  
  mse = list()
  mse_weight = list()
  mse_cbps = list()
  mse_cbps_overlap = list()
  mse_m = list()
  mse_weight_m = list()
  mse_cbps_m = list()
  mse_cbps_overlap_m = list()
  for (item in 1:nitem){
    print(item)
    source('FanLiSimulation.R')
    n_train = n * 0.8
    test_treat_effect_ave = ifelse(heteo, mean(tau_true[(n_train + 1):n]), treat_size)
    x = X[1:n_train,]
    x_test = X[(n_train + 1):n,]
    z = Z[1:n_train,]
    z_test = Z[(n_train + 1):n,]
    y_train = y[1:n_train]
    y_test = y[(n_train + 1):n]
    ########
    
    ###### build model ########
    ### parameter-setting ###
    f = 'X1+X2+X3+X4+X5+X6'
    tune_list = c("sample.fraction","min.node.size", "alpha", "imbalance.penalty")
    # 1st get ps
    ps = getprob(df = data.frame(x, w = w[1:n_train]), f = f)
    ps.formula = paste(sep = '~', 'w', f)
    data = data.frame(w = w[1:n_train] , x, y_train)
    df.name = names(data)
    iptw_ow = PSweight(ps.formula = ps.formula, yname = 'y_train', data = data.frame(w = w[1:n_train] , x, y_train))
    ps_ow = iptw_ow$propensity[,2]
    cbps_model = CBPS(formula = paste(sep = '~', 'w', f), data = data.frame(w = w[1:n_train] , x), ATT = 0)
    ps_cbps = cbps_model$fitted.values
    # 2nd get overlap weight
    weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps), method = "Overlap")
    cbps_weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps), method = "Combined")
    
    grct = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL)
    grct_overlap = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, W.hat = ps_ow)
    grct_cbps = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, W.hat = ps_cbps)
    cbps_weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps), method = "Overlap")
    grct_cbps_overlap = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, W.hat = ps_cbps,sample.weights = cbps_weight)
    #########
    
    #### ps-misspecified model#
    name = names(X)
    dfname = c(name, 'w')
    getprob_df = data.frame(x = z, w = w[1:n_train])
    names(getprob_df) = dfname
    ps_m = getprob(df = getprob_df, f = f)
    ps.formula = paste(sep = '~', 'w', f)
    dfm = data.frame(z, w = w[1:n_train], y_train)
    names(dfm)=df.name
    iptw_ow_m = PSweight(ps.formula = ps.formula, yname = 'y_train', data = data.frame(z, w = w[1:n_train], y_train))
    ps_ow_m = iptw_ow_m$propensity[,2]
    cbps_model_m = CBPS(formula = paste(sep = '~', 'w', f), data = getprob_df, ATT = 0)
    ps_cbps_m = cbps_model_m$fitted.values
    # 2nd get overlap weight
    weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_m), method = "Overlap")
    cbps_weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps_m), method = "Combined")
    grct_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL)
    grct_overlap_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL, W.hat = ps_ow_m)
    grct_cbps_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL,  W.hat = ps_cbps_m)
    cbps_weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps_m), method = "Overlap")
    grct_cbps_overlap_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL, W.hat = ps_cbps_m, sample.weights = cbps_weight_m)
    ############
    
    #####plot#######
    
    # summary(grct)
    trees = get_tree(grct,2)
    tree.plot = plot(get_tree(grct,1))
    # cat(DiagrammeRsvg::export_svg(tree.plot), file='plot.svg')
    
    # summary(grct_overlap)
    trees = get_tree(grct_overlap,5)
    tree.plot = plot(get_tree(grct_overlap,1))
    ###############
    
    
    ##### predict&test #######
    tau_hat = predict(grct, x_test, estimate.variance = F)
    tau_hat_weight = predict(grct_overlap, x_test, estimate.variance = F)
    tau_hat_cbps = predict(grct_cbps, x_test, estimate.variance = F)
    tau_hat_cbps_weight = predict(grct_cbps_overlap, x_test, estimate.variance = F)
    mse[item] = (mean((tau_hat$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_weight[item] = (mean((tau_hat_weight$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_cbps [item] = (mean((tau_hat_cbps$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_cbps_overlap[item] = (mean((tau_hat_cbps_weight$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    
    bias[item] = mean(abs(tau_hat$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_weight[item] = mean(abs(tau_hat_weight$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_cbps[item] = mean(abs(tau_hat_cbps$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_cbps_overlap[item] = mean(abs(tau_hat_cbps_weight$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    
    ave_bias[item] = mean((tau_hat$predictions - test_treat_effect_ave))
    ave_bias_weight[item] = mean((tau_hat_weight$predictions - test_treat_effect_ave))
    ave_bias_cbps[item] = mean((tau_hat_cbps$predictions - test_treat_effect_ave))
    ave_bias_cbps_overlap[item] = mean((tau_hat_cbps_weight$predictions - test_treat_effect_ave))
    ###
    
    ##
    tau_hat_m = predict(grct_m, z_test, estimate.variance = F)
    tau_hat_weight_m = predict(grct_overlap_m, z_test, estimate.variance = F)
    tau_hat_cbps_m = predict(grct_cbps_m, z_test, estimate.variance = F)
    tau_hat_cbps_weight_m = predict(grct_cbps_overlap_m, z_test, estimate.variance = F)
    mse_m[item] = (mean((tau_hat_m$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_weight_m[item] = (mean((tau_hat_weight_m$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_cbps_m [item] = (mean((tau_hat_cbps_m$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    mse_cbps_overlap_m[item] = (mean((tau_hat_cbps_weight_m$predictions - tau_true[(n_train + 1):n]))^2)^0.5
    
    bias_m[item] = mean(abs(tau_hat_m$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_weight_m[item] = mean(abs(tau_hat_weight_m$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_cbps_m[item] = mean(abs(tau_hat_cbps_m$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    bias_cbps_overlap_m[item] = mean(abs(tau_hat_cbps_weight_m$predictions - tau_true[(n_train + 1):n]/tau_true[(n_train + 1):n]))
    
    ave_bias_m[item] = mean((tau_hat_m$predictions - test_treat_effect_ave))
    ave_bias_weight_m[item] = mean((tau_hat_weight_m$predictions - test_treat_effect_ave))
    ave_bias_cbps_m[item] = mean((tau_hat_cbps_m$predictions - test_treat_effect_ave))
    ave_bias_cbps_overlap_m[item] = mean((tau_hat_cbps_weight_m$predictions - test_treat_effect_ave))
    #######################
  }
  
  rmse = mean(as.numeric(mse))
  abs_bias = mean(as.numeric(bias))
  std = sd(as.numeric(bias))
  avebias = mean(as.numeric(ave_bias))/test_treat_effect_ave
  
  bias_w = mean(as.numeric(ave_bias_weight))/test_treat_effect_ave
  rmse_w = mean(as.numeric(mse_weight))
  std_w = sd(as.numeric(bias_weight))
  abs_bias_w =  mean(as.numeric(bias_weight))
  
  avebias_cbps = mean(as.numeric(ave_bias_cbps))/test_treat_effect_ave
  rmse_cbps = mean(as.numeric(mse_cbps))
  std_cbps = sd(as.numeric(bias_cbps))
  abs_bias_cbps =  mean(as.numeric(bias_cbps))
  
  bias_cbps_weight = mean(as.numeric(ave_bias_cbps_overlap))/test_treat_effect_ave
  rmse_cbps_weight = mean(as.numeric(mse_cbps_overlap))
  std_cbps_weight = sd(as.numeric(bias_cbps_overlap))
  abs_bias_cbps_weight =  mean(as.numeric(bias_cbps_overlap))
  
  
  
  bias_w_mis = mean(as.numeric(ave_bias_weight_m))/test_treat_effect_ave
  bias_cbps_mis = mean(as.numeric(ave_bias_cbps_m))/test_treat_effect_ave
  bias_cbps_weight_mis = mean(as.numeric(ave_bias_cbps_overlap_m))/test_treat_effect_ave
  
  rmse_m = mean(as.numeric(mse_m))
  abs_bias_m = mean(as.numeric(bias_m))
  std_m = sd(as.numeric(bias_m))
  bias_mis = mean(as.numeric(ave_bias_m))/test_treat_effect_ave
  
  rmse_w_m = mean(as.numeric(mse_weight_m))
  std_w_m = sd(as.numeric(bias_weight_m))
  abs_bias_w_m =  mean(as.numeric(bias_weight_m))
  
  rmse_cbps_m = mean(as.numeric(mse_cbps_m))
  std_cbps_m = sd(as.numeric(bias_cbps_m))
  abs_bias_cbps_m =  mean(as.numeric(bias_cbps_m))
  
  rmse_cbps_weights_m = mean(as.numeric(mse_cbps_overlap_m))
  std_cbps_weight_m = sd(as.numeric(bias_cbps_overlap_m))
  abs_bias_cbps_weight_m =  mean(as.numeric(bias_cbps_overlap_m))
  
  
  ans1_rmse = data.frame(normal = rmse, overlap = rmse_w, cbps = rmse_cbps, cbps_overlap = rmse_cbps_weight)* 10
  ans1_absbias = data.frame(normal = abs_bias, overlap = abs_bias_w, cbps = abs_bias_cbps, cbps_overlap = abs_bias_cbps_weight) * 10
  ans1_std = data.frame(normal = std, overlap = std_w, cbps = std_cbps, cbps_overlap = std_cbps_weight) * 10 
  ans1_bias = data.frame(normal = avebias, overlap = bias_w, cbps = avebias_cbps, cbps_overlap = bias_cbps_weight) * 10
  
  ans2_rmse = data.frame(normal = rmse_m, overlap = rmse_w_m, cbps = rmse_cbps_m, cbps_overlap = rmse_cbps_weights_m)* 10
  ans2_absbias = data.frame(normal = abs_bias_m, overlap = abs_bias_w_m, cbps = abs_bias_cbps_m, cbps_overlap = abs_bias_cbps_weight_m) * 10
  ans2_std = data.frame(normal = std_m, overlap = std_w_m, cbps = std_cbps_m, cbps_overlap = std_cbps_weight_m) * 10
  ans2_bias = data.frame(normal = bias_mis, overlap = bias_w_mis, cbps = bias_cbps_mis, cbps_overlap = bias_cbps_weight_mis) * 10
  
  ans0 = rbind(ans1_bias, ans1_absbias, ans1_std, ans1_rmse, ans2_bias, ans2_absbias, ans2_std, ans2_rmse)
  
  View(ans0)
  write.csv(ans0, filename)
}
}