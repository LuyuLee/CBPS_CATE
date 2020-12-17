# the .R includes 2 goals: get the average treat effect and plot in mother-smoker-baby-weight data (cate) 

library(CBPS)
## Not run:
# Save the plot of a tree in the causal forest.
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
library(DiagrammeR)
library(DiagrammeRsvg)
library("glmnet")
library(DMwR)
h_num=3
para = 1
white = T
#loadding data#
#data = read.table('cate-birthdata-cleaned-1stkid-white-detailed.txt', head=F, sep = '\t')
data = read.table('cate-birthdata-cleaned-1stkid-black-detailed.txt', head=F, sep = '\t')
if (white)
  data = read.table('cate-birthdata-cleaned-1stkid-white-detailed.txt', head=F, sep = '\t')

#copy from robot's code  Robert P. Lieli
byear=data[,1];
fage=data[,2];
mage=data[,3];
feduc=data[,4];
meduc=data[,5];
terms=data[,6];
gestation=data[,7];
prenatal=data[,8];
prenatal_visits=data[,9];
mom_zip=data[,10];
wtgain=data[,11];
anemia=data[,12];
diabetes=data[,13];
hyperpr=data[,14];
amnio=data[,15];
ultra=data[,16];
male=data[,17];
feducmiss=data[,18];
fagemiss=data[,19];
married=data[,20];
bweight=data[,21];
smoke=data[,22];
drink=data[,23];
kessner_ad=data[,24];
kessner_inad=data[,25];
med_inc=data[,26];
pc_inc=data[,27];
long=data[,28];
lat=data[,29];
popdens=data[,30];

# make mother's age
mage = mage + rnorm(length(mage)) - 0.5

##chose variable according to (Lieli2019. Estimation of Conditional Average Treatment Effects with High-Dimensional Data)
df = data.frame(bweight, smoke, mage, meduc, fage, feduc, fagemiss, married, popdens, prenatal, prenatal_visits,
                terms, amnio, anemia, diabetes, hyperpr, ultra, male, drink )

## get PS and Weight 
# we need change the way to get ps   >  cbps ? 
##  high dimension cbps  

# dmwr 
# perc.over = xx 表示 少样本变成原来的（1+xx/100）倍
# perc.under=yy 表示多样本变成少样本的 yy/100 *(xx/100)倍
table(smoke)
df$smoke = as.factor(smoke)
set.seed(12345)
#under.sample = SMOTE(smoke ~., data = df, perc.under = 110,perc.over = 100 )
#table(under.sample$smoke)


formulation = 'smoke~.-bweight'
logsti = glm(formulation, binomial(link = 'logit'), data = df)
x = as.matrix(df[,-1][,-1])


cv.fit<-cv.glmnet(x, as.matrix(smoke), family="binomial", type.measure = 'auc' )
coefficients<-coef(cv.fit,s=cv.fit$lambda.min)
all.predict = predict(cv.fit, newx = x, s = cv.fit$lambda.min)

## cbps
CBPS.fit = CBPS(formulation, data = df, ATT=0)
ps.cbps = CBPS.fit$fitted.values
  
  
  # summary(logsti)
  # data.glm<- step(logsti)
prediction = logsti$linear.predictors
all.predict = predict(logsti, newdata = df)
all.prob = 1/(1+exp(-all.predict))
all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
performance = length(which((all.predict==smoke)==TRUE))/nrow(df)
PS = as.numeric(all.prob)
# weight = get_weight(df = data.frame(w = smoke, prob = PS), method = "Overlap")
# cbps.weight = get_weight(df = data.frame(w = smoke, prob = ps.cbps), method = "Overlap")

# condition x
X = as.matrix(df[, 3])
# building model with overlap ps
# truncated: [(weight>0.01)&(weight<0.99)]
#cate
sigma = sd(X)
k = l = 2
h1_list = c(0.25, 0.5, 1, 2) 
x1 = X
#target_x = seq(17.4, 26, length.out = 50)
first_qua = summary(mage)
target_x = seq(first_qua[2], 33, length.out = 100)
if (white)
  target_x = seq(first_qua[2], 30, length.out = 100)

# h1 <==== (map)  h1_list
h1 = h1_list[h_num]* sigma 
iptw = smoke * bweight / PS - (1 - smoke) * bweight / (1 - PS)
tao_logit = get_NWps(x1, iptw, bandwith = h1, k=1, x.points = target_x)
sd(tao_logit)

iptw_cbps = smoke * bweight / ps.cbps - (1 - smoke) * bweight / (1 - ps.cbps)
tao_CBPS1 = get_NWps(x1, iptw_cbps, bandwith = h1, k=1, x.points = target_x)
sd(tao_CBPS1)
'
# grf
tune_list = c("sample.fraction","min.node.size", "alpha", "imbalance.penalty")
grf = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list)
grf.m = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list, sample.weights = weight )
grf.cbps = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list, sample.weights = cbps.weight )
'
#saveRDS(grf.m, file = 'grf_smoke.RData')
#saveRDS(grf.cbps, file = 'grf_smoke_cbps.RData')'

#plot subtrees
# plot(get_tree(grf.m,110))

### all covas
'
w = as.matrix(smoke)
w = as.matrix(bweight)

predictions.m = as.numeric(grf.m$predictions)
mean(predictions.m)
sd(predictions.m)

predictions = as.numeric(grf$predictions)
mean(predictions)
sd(predictions)

predictions.cbps = as.numeric(grf.cbps$predictions)
mean(predictions.cbps)
sd(predictions.cbps)
'

plotx = target_x
item = 0
ploty = numeric()
ploty.m = numeric()
# ploty.cbps = list()
for (i in plotx){
  item = item + 1
  ploty.m[item] = (tao_CBPS1[item])
  ploty[item] = (tao_logit[item])
  #ploty.cbps[item] = (tao_CBPS2[item])
}

#plot 
library(ggplot2)
library(dplyr)


mytheme <- theme_bw() + 
  theme(plot.title=element_text(size=rel(2),hjust=0.5),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.5)),
        panel.grid.major=element_line(color="white"),
        panel.grid.minor=element_line(color="white"),
        panel.border=element_rect(color="white"),
        axis.line=element_line(color="gray",size=1))


norm_sd = sd(as.numeric(ploty))
norm_sd_cbps = sd(as.numeric(ploty.m))
plot_control = data.frame(x = plotx, y = ploty, up = ploty + para * norm_sd, down = ploty - para *norm_sd)

plot_cbps = data.frame(x = plotx, y = ploty.m, up = ploty.m + para * norm_sd_cbps, down = ploty.m - para *norm_sd_cbps )


name = paste(sep = '', Sys.Date(),'_ifwhite=',as.character(white), '_control_', '_', 'h1=', as.character(h1_list[h_num]))

png_name = paste(sep='_', name, '.png')
p = ggplot(data = plot_control, aes(x=x, y=y)) 
title_name = paste(sep='', "CATE with h=",as.character(h1_list[h_num]),'σ,±2s.e.')
png(file=png_name)
with(p, 
       p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"), 
                        axis.title.x = element_text(vjust=0))+
       
      geom_smooth(aes(y=y), se=FALSE)+geom_line(aes(y=up), linetype="dotted")+geom_line(aes(y=down), linetype="dotted")+
       xlab("Mother's Age(year)") + ylab("Effect on birthweight (grams)") + 
       ggtitle(title_name) + mytheme
     
     )
dev.off()


name = paste(sep = '', Sys.Date(),'_ifwhite=',as.character(white), '_cbps_', '_', 'h1=', as.character(h1_list[h_num]))

png_name = paste(sep='_', name, '.png')
p = ggplot(data = plot_cbps, aes(x=x, y=y)) 
png(file=png_name)
with(p, 
     p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"), 
                        axis.title.x = element_text(vjust=0))+
       
       geom_smooth(aes(y=y), se=FALSE)+geom_line(aes(y=up), linetype="dotted")+geom_line(aes(y=down), linetype="dotted")+
       xlab("Mother's Age(year)") + ylab("Effect on birthweight (grams)") + 
       ggtitle(title_name) + mytheme
     
     
)
dev.off()



plot(plotx, ploty, col='red', type = 'l', ylab = 'CATE', xlab = 'age')
lines(plotx, ploty+2*norm_sd, col='black', type = 'c')
lines(plotx, ploty-2*norm_sd, col='black', type = 'c')


plot(plotx, ploty.m, type = 'l')
lines(plotx, ploty.cbps, col='blue', type = 'l')
