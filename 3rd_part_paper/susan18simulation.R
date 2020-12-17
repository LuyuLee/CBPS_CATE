library(grf)
library(CBPS)
## Not run:
# Save the plot of a tree in the causal forest.
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
library(DiagrammeR)
library(DiagrammeRsvg)
####generate simulational data#####
# maybe we can use the data structure in susan(2017)
# so the susan's simulation data structure are copied at following:

# Generate data 
# parameters for data generating
#10 2   or 20 4 
p <- 20 # number of total covariates
# please keep pt equals to 4 in this R scrip
pt <- 2 # number of covariates affecting treatment effects    
py <- 4 # number of covariates affecting outcomes but not treatment effects
asym <- .5 # whether treatment effects are distributed asymmetrically across treated and control
n <- 2600 # total size of the dataset 1800 or 2600
n_list = c(1800, 2600)
p_list = c(10, 20)

heteo = T
conf = T

# seed 12345
set.seed(seed = 12345)
nitem = 60

for (p in p_list)
  for (n in n_list){
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
  # draw X
  X = matrix(nrow = n, ncol = p)
  for (i in 1 : p)
    X[,i] = runif(n)
  # X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  ############# obser covs   kang, 2007
  Z1 = exp(X[,1]/2)
  Z2 = X[,2]/(1 + exp(X[,1]))+5
  # Z3 = (X[,1] * X[,3] / 25 + 0.6)**3
  # Z4 = (X[,1] + X[,4] + 5)**2
  ####N
  # generate treatment effects as function o  f X
  if (p<pt+py) print("error: p>=pt+py required")
  tau <- 0
  if (heteo == T){
    tau = 1 + (1 + exp(20 * (X[, 1] - 1/3)))**-1
    # pt > 1
    for (iii in 2:pt) {
      f1 = 1 + (1 + exp(-20 * (X[, iii] - 1/3)))**-1
      tau <- tau * f1
    }
  } else if (heteo == F)
    tau = rep(0, n)
  Z = X
  Z[, 1] = Z1
  Z[, 2] = Z2
  # Z[, 3] = Z3 
  # Z[, 4] = Z4
  # generate average value of outcomes
  
  # mu <- treatsize*rowSums(X[,1:pt])+levsize*rowSums(X[,(pt+1):(pt+py)])
  
  # we need to change the propens into another ways  such as logit model 
  # maybe a ex-function.r is suitable for propens
  # the propens could be (0.3-0.5)
  # propens <- Treat_generate(X, pt = pt, py = py, n = n) #treatment probability
  
  # ex is  the prob of w=1   and w~ bernoulli(ex)
  ex = rep(0.5, n)
  if (conf == T){
    #   CHANGED
    rda = dbeta( X[, 3], 2, 4, ncp = 0)
    ex = 0.25*(1 + rda)
    #
    mu = 2 * X[, 3] - 1
  } else if(conf == F)
    mu = rep(0, n)
  # draw W
  w <- rbinom(n, 1, ex)
  w_ <- 1-w
  ##########divided line##################
  
  # generate outcomes as function of treatment status, mu, tau, and noise
  # y ~ norm(mx + (w-0.5)tao, 1)
  y = rnorm(mu + asym*w*tau + (asym-1)*(1-w)*tau, 1)
  y_ = rnorm(mu + asym*w_*tau + (asym-1)*(1-w_)*tau, 1)
  
  # create formulas for estimation
  # if modifying code for another dataset, need to simply name outcome variable y, treatment variable w, and
  # create a formula such as f with the list of x variables separated by "+"
  name = ''
  f <- ""
  nextx <- ""
  if (p>1) {
    for (ii in 1:(p-1)) {
      nextx <- paste("X",ii, sep="")
      if (ii==1) {name <- nextx}
      if (ii>1) {name <- c(name, nextx)}
      f <- paste(f, nextx, "+", sep="")
    }
    f <- paste(f, "X", ii+1, sep="")
  } else if (p==1) {
    f <- "X1"
  }
  
  for (ii in 1:p) {
    nextx <- paste("X",ii, sep="")
    if (ii==1) {name <- nextx}
    if (ii>1) {name <- c(name, nextx)}
  }
  
  # get true tau
  tau_true <- (1-2*w)*(y_ - y)
  
  n_train = n - 1000
  x = X[1:n_train,]
  x_test = X[(n_train + 1):n,]
  z = Z[1:n_train,]
  z_test = Z[(n_train + 1):n,]
  y_train = y[1:n_train]
  y_test = y[(n_train + 1):n]
  ########
  
  ###### build model ########
  ### parameter-setting ###
  tune_list = c("sample.fraction","min.node.size", "alpha", "imbalance.penalty")
  # 1st get ps
  ps = getprob(df = data.frame(x, w = w[1:n_train]), f = f)
  cbps_model = CBPS(formula = paste(sep = '~', 'w', f), data = data.frame(w = w[1:n_train] , x), ATT = 1)
  ps_cbps = cbps_model$fitted.values
  # 2nd get overlap weight
  weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps), method = "Overlap")
  cbps_weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps), method = "Combined")
  
  grct = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL)
  grct_overlap = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = weight)
  grct_cbps = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = cbps_weight)
  cbps_weight = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps), method = "Overlap")
  grct_cbps_overlap = causal_forest(x, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = cbps_weight)
  #########
  
  #### ps-misspecified model#
  dfname = c(name, 'w')
  getprob_df = data.frame(x = z, w = w[1:n_train])
  names(getprob_df) = dfname
  ps_m = getprob(df = getprob_df, f = f)
  cbps_model_m = CBPS(formula = paste(sep = '~', 'w', f), data = getprob_df, ATT = 1)
  ps_cbps_m = cbps_model_m$fitted.values
  # 2nd get overlap weight
  weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_m), method = "Overlap")
  cbps_weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps_m), method = "Combined")
  grct_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL)
  grct_overlap_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = weight_m)
  grct_cbps_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = cbps_weight_m)
  cbps_weight_m = get_weight(df = data.frame(w = w[1:n_train], prob = ps_cbps_m), method = "Overlap")
  grct_cbps_overlap_m = causal_forest(z, y_train, w[1:n_train], tune.parameters = NULL, sample.weights = cbps_weight)
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
  
  #######################
}

rmse = mean(as.numeric(mse))
std = sd(as.numeric(mse))
rmse_w = mean(as.numeric(mse_weight))
std_w = sd(as.numeric(mse))
rmse_cbps = mean(as.numeric(mse_cbps))
std_cbps = sd(as.numeric(mse_cbps))
rmse_cbps_weight = mean(as.numeric(mse_cbps_overlap))
std_cbps_weight = sd(as.numeric(mse_cbps_overlap))




rmse_m = mean(as.numeric(mse_m))
std_m = sd(as.numeric(mse_m))
rmse_w_m = mean(as.numeric(mse_weight_m))
std_w_m = sd(as.numeric(mse_m))
rmse_cbps_m = mean(as.numeric(mse_cbps_m))
std_cbps_m = sd(as.numeric(mse_cbps_m))
rmse_cbps_weights_m = mean(as.numeric(mse_cbps_overlap_m))
std_cbps_weights_m = sd(as.numeric(mse_cbps_overlap_m))



ans1 = data.frame(normal = rmse, overlap = rmse_w, cbps = rmse_cbps, cbps_overlap = rmse_cbps_weight)
ans2 = data.frame(normal = rmse_m, overlap = rmse_w_m, cbps = rmse_cbps_m, cbps_overlap = rmse_cbps_weights_m)
ans0 = rbind(ans1, ans2)
ans = data.frame(normal = t(as.matrix(ans1)), misspecified = t(as.matrix(ans2)))

filename = paste(sep = '', 'miscbps_', as.character(nitem), 'item_num=', as.character(n), '_q=', as.character(p),
                 '_heterog=', as.character(heteo), '_conf=',as.character(conf),'.csv')

write.csv(ans0, filename)
}
# after finished above requirement of main structure of Rcode 
# you should finish the part of iteration in the simulation and get final ans 
