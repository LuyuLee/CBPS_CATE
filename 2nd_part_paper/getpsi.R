get.psi.dr <- function(Y, D, X, stype, cbps=FALSE, cbps_method = 'over' , Ey.Ps.Estimat = F){
	# library(CBPS)
	n <- nrow(Y)  ## number of observation
	k <- ncol(X)  ## number of covariate
  # ps
	if (stype == 1 | stype ==2){
  	pi_hat<-fitted(glm(D~X, family=binomial("logit")))
  	if (cbps)
  	  pi_hat = CBPS(formula = D~X, ATT = 0, method = cbps_method)$fitted.values
	}
	if (stype == 3 | stype ==4){
	  pi_hat<-fitted(glm(D~X[,1:(k/2)], family=binomial("logit")))
	  if (cbps)
	    pi_hat = CBPS(formula = D~X[,1:(k/2)], ATT = 0, method = cbps_method)$fitted.values
	}
	
	Ytreated <- Y[D==1]
	Xtreated <- X[D==1,]
	Yuntreated <- Y[D==0]
	Xuntreated <- X[D==0,]
	piTreated = pi_hat[D==1]
	piUntreated = pi_hat[D==0]

	# outcome
	
	if (stype == 1 | stype ==3){
	  cov_treat = Xtreated
	  cov_untreat = Xuntreated
	  if (Ey.Ps.Estimat){
	    cov_treat = piTreated
      cov_untreat = piUntreated	  
	    }
		miu1_hat<-cbind(1,X)%*%as.matrix(lm(Ytreated~cov_treat)$coef)
		miu0_hat<-cbind(1,X)%*%as.matrix(lm(Yuntreated~cov_untreat)$coef)
}
	if (stype == 2 | stype ==4){
	  cov_treat = Xtreated[,1:(k/2)]
	  cov_untreat = Xuntreated[,1:(k/2)]
	  if (Ey.Ps.Estimat){
	    cov_treat = piTreated
	    cov_untreat = piUntreated	  
	  }
		miu1_hat<-cbind(1,X[,1:(k/2)])%*%as.matrix(lm(Ytreated~cov_treat)$coef)
		miu0_hat<-cbind(1,X[,1:(k/2)])%*%as.matrix(lm(Yuntreated~cov_untreat)$coef)
}

	
	psi1<-D*Y/pi_hat-(D-pi_hat)*miu1_hat/pi_hat
	psi0<-(1-D)*Y/(1-pi_hat)+(D-pi_hat)*miu0_hat/(1-pi_hat)

	psi1-psi0

}

get.psi.ra <- function(Y, D, X){

	Ytreated<-Y[D==1]
	Xtreated<-X[D==1,]
	Yuntreated<-Y[D==0]
	Xuntreated<-X[D==0,]

	miu1_hat<-cbind(1,X)%*%as.matrix(lm(Ytreated~Xtreated)$coef)
	miu0_hat<-cbind(1,X)%*%as.matrix(lm(Yuntreated~Xuntreated)$coef)

	miu1_hat - miu0_hat
	
}

get.psi.ipw <- function(Y, D, X){
	
	pi_hat<-fitted(glm(D~X, family=binomial("logit")))

	psi1 <- D*Y/pi_hat
	psi0 <- (1-D)*Y/(1-pi_hat)
	psi1-psi0

}



##-- model specification --##
## here, we specify four models:two for regression and another two for propensity score model
## model_1 : specified regression model
## model_2 : misspecified regression model
## model_3 : specified propensity score model
## model_4 : misspecified propensity score model



get.psi.dr2 <- function(Y, D, X, stype, cbps=FALSE, cbps_method = 'over' , order = 2){
  # library(CBPS)
  n <- nrow(Y)  ## number of observation
  k <- ncol(X)  ## number of covariate
  # ps
  if (stype == 1 | stype ==2){
    glm_model = glm(D~X, family=binomial("logit"))
    pi_hat<-fitted(glm_model)
    alpha = glm_model$coefficients
    if (cbps)
      pi_hat = CBPS(formula = D~X, ATT = 0, method = cbps_method)$fitted.values
  }
  if (stype == 3 | stype ==4){
    glm_model = glm(D~X[,1:(k/2)], family=binomial("logit"))
    pi_hat<-fitted(glm_model)
    alpha = glm_model$coefficients
    if (cbps)
      pi_hat = CBPS(formula = D~X[,1:(k/2)], ATT = 0, method = cbps_method)$fitted.values
  }
  
  Ytreated <- Y[D==1]
  Xtreated <- X[D==1,]
  Yuntreated <- Y[D==0]
  Xuntreated <- X[D==0,]
  piTreated = pi_hat[D==1]
  piUntreated = pi_hat[D==0]
  

  # outcome
  
  if (stype == 1 | stype ==3){
    dytreated = as.matrix(cbind(rep(1, length(X[, 1])), X)) %*% alpha
    dyuntreated = as.matrix(cbind(rep(1, length(X[, 1])), X)) %*% alpha
    gama_tr = getgama(Y=Ytreated, X=Xtreated, alpha = alpha, q=order, weight = NULL)
    gama_un = getgama(Y=Yuntreated, X=Xuntreated, alpha = alpha, q=order, weight = NULL)
    miu1_hat = EstimatingEy(gama=gama_tr, dy = dytreated)
    miu0_hat = EstimatingEy(gama=gama_un, dy = dyuntreated)
  }
  if (stype == 2 | stype ==4){
    dytreated = as.matrix(cbind(rep(1, length(X[, 1])), X[,1:(k/2)])) %*% alpha
    dyuntreated = as.matrix(cbind(rep(1, length(X[, 1])), X[,1:(k/2)])) %*% alpha
    gama_tr = getgama(Y=Ytreated, X=Xtreated[,1:(k/2)], alpha = alpha, q=order, weight = NULL)
    gama_un = getgama(Y=Yuntreated, X=Xuntreated[,1:(k/2)], alpha = alpha, q=order, weight = NULL)
    miu1_hat = EstimatingEy(gama=gama_tr, dy = dytreated)
    miu0_hat = EstimatingEy(gama=gama_un, dy = dyuntreated)
  }
  
  
  psi1<-D*Y/pi_hat-(D-pi_hat)*miu1_hat/pi_hat
  psi0<-(1-D)*Y/(1-pi_hat)+(D-pi_hat)*miu0_hat/(1-pi_hat)
  
  psi1-psi0
  
}

EstimatingEy=function (gama, dy)
{
  sum = gama[1]
  if (length(gama) > 1)
    for (i in 1 : (length(gama) - 1))
      sum = sum + gama[i + 1] * dy ** i
  return(sum)
}


get_NWps = function(x, y, bandwith = 0.5, k, x.points = x)
{
  NW_model = ksmooth(x, y, kernel = 'normal', bandwidth = bandwith, x.points = x.points)
  kernel_value = NW_model$y
  return(kernel_value)
}

EstCate = function(x, y, D, h, h1, k, l, target_x, ps_method='NW', X1=X1)
{	# library(CBPS)
  if (ps_method == 'glm')
  {
    glm_model = glm(D~X1 + X2 + X3, data = x, family = binomial(link = 'logit'))
    ps = glm_model$fitted.values
  }
  if (ps_method == 'NW')
    #confusing
    ps = get_NWps(x, D, bandwith = h, k=k)
  if (ps_method == 'CBPS1')
    ps = CBPS(formula = D~X1 + X2 + X3, data = x, ATT=0, method = 'over')$fitted.values
  if (ps_method == 'CBPS2')
    ps = CBPS(formula =D~X1 + X2 + X3, data = x, ATT = 0, method = 'exact')$fitted.values
  if (ps_method == 'OVERLAP')
  {
    iptw.overlap = PSweight(ps.formula = D~X1 + X2 + X3, yname="Y", data=data.frame(X1, X2, X3, D, Y))
    ps = iptw.overlap$propensity
  }
  iptw = D * y / ps - (1 - D) * y / (1 - ps)
  tao = get_NWps(X1, iptw, bandwith = h1, k=l, x.points = target_x)
  tao
}


