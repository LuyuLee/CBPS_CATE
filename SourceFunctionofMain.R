Treat_generate = function(X, beta = rep(1/py, py), pt,  py, n = 1000)
{
  Z = matrix(X[, (pt+1):(pt + py)], n, py) %*% as.numeric(beta) + rnorm (n, -0.4, 1)
  A = exp(Z) / (1 + exp(Z))
  return(A)
}



getprob = function(df, f)
{
  formula = paste("w","~", f, sep="")
  logsti = glm(formula, binomial(link = 'logit'), data = df)
  summary(logsti)
  # data.glm<- step(logsti)
  prediction = logsti$linear.predictors
  all.predict = predict(logsti)
  all.prob = 1/(1+exp(-all.predict))
  all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
  performance = length(which((all.predict==df$w)==TRUE))/nrow(df)
  prob = all.prob
  return(prob)
}


get_weight = function(df, method ='Truncated', alpha = 0.05)
{
  #df = probdata
  #method = 'Truncated'
  #alpha = 0.05
  
  id0 = which(df$w == 0)
  id1 = which(df$w == 1)
  if (method == "Control")
  {
    method = 'Combined'
  }
  if (method == 'Truncated')
  {
    weight0 = 1 / (1 - df$prob)
    weight1 = 1 / df$prob
    weight0[which(((df$prob < alpha) == TRUE) | (df$prob > (1 - alpha)) == TRUE)] = 0
    weight1[which(((df$prob < alpha) == TRUE) | (df$prob > (1 - alpha)) == TRUE)] = 0
  }
  if (method == 'Overlap')
  {
    weight0 = df$prob
    weight1 = 1 - df$prob
  }
  
  if (method == 'Combined')
  {
    weight0 = 1 / (1 - df$prob)
    weight1 = 1 / df$prob
  }
  
  if (method == 'Matching')
  {
    weight0[which(df$prob > (1 - df$prob))] = 1 
    weight0[which(df$prob <= (1 - df$prob))] = df$prob / (1 - df$prob)
    weight1[which(df$prob > (1 - df$prob))] = (1 - df$prob) / df$prob
    weight1[which(df$prob <= (1 -df$prob))] = 1
  }
  
  if (method == 'Treated')
  {
    weight0 = df$prob / (1 - df$prob)
    Lis = array()
    for (i in 1:length.POSIXlt(df))
      Lis[i] = 1
    weight1 = Lis
  }
  
  if (method == 'Control')
  {
    weight0 = array()
    for (i in 1:length.POSIXlt(df))
      weight0[i] = 1
    weight1 = (1 - df$prob) / df$prob
  }
  weight = array()
  for (i in 1:nrow(df))
  {
    if (df$w[i] == 1)
      weight[i] = weight1[i]
    if (df$w[i] == 0)
      weight[i] = weight0[i]
  }
  # weight = c(weight0[id0],weight1[id1])
  #weightt = data.frame(weight0[id0], weight1[id1])
  return(weight)
}
