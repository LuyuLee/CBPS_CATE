#dgp cate simulation
X1 = runif(n, -0.5, 0.5)
X2 = (1 + 2*X1)^2 + runif(n, -0.5, 0.5)
X3 = (-1 + X1)^2  + runif(n, -0.5, 0.5)
#names(X) = c('X1','X2','X3','X4','X5','X6')
X = data.frame(X1, X2, X3)
names(X) = c('X1','X2','X3')
ex = (1 + exp(-(alpha0 + as.matrix(X) %*% (alpha*gama)))) ** -1
summary(ex)

D = rbinom(n, 1, ex)
error = rnorm(n, 0, 0.25^2)
Y1 = X1 * X2 * X3 + 1
Y = Y1 * D
x1 = c(-0.3, -0.1, 0, 0.1, 0.3)
cate = x1 * (1 + 2*x1)^2 * (-1 + x1)^2 + 1

# Z1 = exp(X1/2)
Z2 = X2/(1 + exp(X1))+10
Z3 = (X1*X3/25 + 0.6)^3
Z = data.frame(X1=X1, X2=Z2, X3=Z3)

