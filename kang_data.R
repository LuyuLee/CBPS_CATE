cbps_simulation = function(n = 500, heteo = T,
                           alpha = alpha, beta = beta,
                           delta = rep(1,n), psgen = 'logit', trsd = c(-0.8, 0.3, 0, 0, 0))
{
  # psgen = c('logit', 'probit', 'tree')
  # Z are observed covariances and non-linear transformed from X which are variance of true model
  # set.seed(11545)
  library(MASS)
  covariance = diag(length(beta) - 1)  
  data <- mvrnorm(n=n, mu=rep(0, length(beta) - 1), covariance)
  X = data.frame(data)
  x = as.matrix(cbind(rep(1, n), X))
  Z1 = exp(X$X1/2)
  Z2 = X$X2/(1 + exp(X$X1)) + 10
  Z3 = (X$X1 * X$X3 / 25 + 0.6)**3
  Z4 = (X$X1 + X$X4 + 20)**2
  Z = data.frame(Z1, Z2, Z3, Z4)
  
  ## get Y_true and logit(pi)
  if (psgen == 'logit'){
    ps = 1 / (exp(-(x %*% alpha)) + 1)
    D = rbinom(n, 1, ps)
  }
  if (psgen == 'probit'){
    ps = pnorm(x %*% alpha)
    D = rbinom(n, 1, ps)
  }
  # if (psgen == 'tree')
  #   D = tree_ps(X, trsd = trsd, bnprob = 0.8)
  summary(D)
  ##
  tau = matrix(nrow = n, ncol = 4)
  for (i in 1 : 4)
    tau[,i] = 1 + (1 + exp(2 * (X[, i] - 1/3)))**-1
  delta = tau[,1] * tau[,2] + tau[,3] * tau[,4]
  if (heteo == F)
    delta = rep(mean(delta), n)
  
  Y = x %*% beta + D * delta + rnorm(n)
  summary(Y)
  return(data.frame(Z, X, Y = Y, treat = D, tau_true = delta))
}