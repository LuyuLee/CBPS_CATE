
data <- mvrnorm(n=n, mu=rep(0, m), covariance)
X = data
indicator<-function(x, condition_value=0) ifelse(x<condition_value,1,0)
X[,4:6] = indicator(data[,4:6])
X = data.frame(X)
names(X) = c('X1','X2','X3','X4','X5','X6')
 
ex = (1 + exp(-(alpha0 + as.matrix(X) %*% (alpha*gama)))) ** -1
mean(ex)
D = rbinom(n, 1, ex)

treat_effect = (D-0.5) * treat_size
if (heteo)
  treat_effect = (D-0.5) * (treat_size + X$X3)
Ey = as.matrix(X) %*% beta + D * treat_effect + (1 - D) * treat_effect
Ey_ = as.matrix(X) %*% beta - (1 - D) * treat_effect - D * treat_effect
y = rnorm(n, mean=Ey, sd=1.5)
y_minus = rnorm(n, mean=Ey_, sd=1.5)
tau_true = (1-2*D) * (y_minus - y)

Z1 = exp(X[,1]/2)
Z2 = X[,2]/(1 + exp(X[,1]))+5
Z3 = (X[,1] * X[,3] / 25 + 0.6)**3

Z = X
Z[, 1] = Z1
Z[, 2] = Z2
Z[, 3] = Z3 
w = D
