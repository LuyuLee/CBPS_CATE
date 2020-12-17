# the .R includes 2 goals: get the average treat effect and plot in mother-smoker-baby-weight data (cate) 

library(grf)
library(CBPS)
## Not run:
# Save the plot of a tree in the causal forest.
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
library(DiagrammeR)
library(DiagrammeRsvg)
library("glmnet")
library(DMwR)


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
under.sample = SMOTE(smoke ~., data = df, perc.under = 110,perc.over = 100 )
table(under.sample$smoke)


formulation = 'smoke~.-bweight'
logsti = glm(formulation, binomial(link = 'logit'), data = df)
x = as.matrix(df[,-1][,-1])


cv.fit<-cv.glmnet(x, as.matrix(smoke), family="binomial", type.measure = 'auc' )
coefficients<-coef(cv.fit,s=cv.fit$lambda.min)
all.predict = predict(cv.fit, newx = x, s = cv.fit$lambda.min)

## cbps



# summary(logsti)
# data.glm<- step(logsti)
prediction = logsti$linear.predictors
all.predict = predict(logsti, newdata = df)
all.prob = 1/(1+exp(-all.predict))
all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
performance = length(which((all.predict==smoke)==TRUE))/nrow(df)
PS = as.numeric(all.prob)
weight = get_weight(df = data.frame(w = smoke, prob = PS), method = "Overlap")
PS.cbps = CBPS(formulation, data=df, ATT = 0)
PS.cbps = PS.cbps$fitted.values
cbps.weight = get_weight(df = data.frame(w = smoke, prob = PS.cbps), method = "Overlap")

# building model with overlap ps
# truncated: [(weight>0.01)&(weight<0.99)]
tune_list = c("sample.fraction","min.node.size", "alpha", "imbalance.penalty")
grf = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list)
saveRDS(grf, file = 'w_grf.RData')
grf.m = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list, sample.weights = weight )
saveRDS(grf.m, file = 'w_grf_w.RData')
grf.cbps = causal_forest(X = df[,3:length(df[1,])], Y =  bweight, W = smoke, tune.parameters = tune_list, sample.weights = cbps.weight )
saveRDS(grf.cbps, file = 'w_grf_cbps.RData')
#plot subtrees
# plot(get_tree(grf.m,110))
grf = readRDS('w_grf.RData')
grf.m = readRDS('w_grf_w.RData')
grf.cbps = readRDS('w_grf_cbps.RData')


### all covas
X = as.matrix(df[, -21][, -21])
w = as.matrix(smoke)
w = as.matrix(bweight)

predictions.m = as.numeric(grf.m$predictions)
mean(predictions.m)
sd(predictions.m)

predictions = as.numeric(grf$predictions)
mean(predictions)
sd(predictions)

predictions.cbps = as.numeric(grf.cbps$predictions)
mean(predictions)
sd(predictions)
first_qua = quantile(mage, c(0.2, 0.8))
plotx = seq(first_qua[1], first_qua[2])+3
if (white)
  plotx = seq(first_qua[1], first_qua[2]) + 3
#plotx = seq(17.4, 30, length.out = 100)
#plotx =  seq(19.6, 29.9, length.out = 100)
item = 0
ploty = numeric()
ploty.m = numeric()
ploty.cbps = numeric()
for (i in plotx){
  item = item + 1
  ploty.m[item] = mean(predictions.m[which(mage>=i-0.5&mage<(i+0.5))])
  ploty[item] = mean(predictions[which(mage>=i-0.5&mage<(i+0.5))])
  ploty.cbps[item] = mean(predictions.cbps[which(mage>=i-0.5&mage<(i+0.5))])
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
norm_sd_cbps = sd(as.numeric(ploty.cbps))
norm_sd_w = sd(as.numeric(ploty.m))
plot_control = data.frame(x = plotx, y = ploty, up = ploty + para * norm_sd, down = ploty - para *norm_sd)

plot_cbps = data.frame(x = plotx, y = ploty.cbps, up = ploty.cbps + para * norm_sd_cbps, down = ploty.cbps - para *norm_sd_cbps)
plot_w = data.frame(x = plotx, y = ploty.m, up = ploty.m + para * norm_sd_w, down = ploty.m - para *norm_sd_w )


name = paste(sep = '', Sys.Date(),'_ifwhite=',as.character(white), '_control_')

png_name = paste(sep='_', name, '.png')
p = ggplot(data = plot_control, aes(x=x, y=y)) 
title_name = paste(sep='', "CATE,±2s.e.")
png(file=png_name)
with(p, 
     p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"), 
                        axis.title.x = element_text(vjust=0))+
       
       geom_smooth(aes(y=y), se=FALSE)+geom_smooth(aes(y=up), linetype="dotted", se=FALSE, color=1)+
       geom_smooth(aes(y=down), linetype="dotted", se=FALSE,color=1)+
       xlab("Mother's Age(year)") + ylab("Effect on birthweight (grams)") + 
       ggtitle(title_name) + mytheme
     
)
dev.off()


name = paste(sep = '', Sys.Date(),'_ifwhite=',as.character(white), '_cbps_')

png_name = paste(sep='_', name, '.png')
p = ggplot(data = plot_cbps, aes(x=x, y=y)) 
png(file=png_name)
with(p, 
     p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"), 
                        axis.title.x = element_text(vjust=0))+
       
       geom_smooth(aes(y=y), se=FALSE)+geom_smooth(aes(y=up), linetype="dotted", se=FALSE, color=1)+
       geom_smooth(aes(y=down), linetype="dotted", se=FALSE,color=1)+xlab("Mother's Age(year)") + ylab("Effect on birthweight (grams)") + 
       ggtitle(title_name) + mytheme
     
     
)
dev.off()



name = paste(sep = '', Sys.Date(),'_ifwhite=',as.character(white), '_w_' )

png_name = paste(sep='_', name, '.png')
p = ggplot(data = plot_w, aes(x=x, y=y)) 
png(file=png_name)
with(p, 
     p+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"), 
                        axis.title.x = element_text(vjust=0))+
       
       geom_smooth(aes(y=y), se=FALSE)+geom_smooth(aes(y=up), linetype="dotted", se=FALSE, color=1)+
       geom_smooth(aes(y=down), linetype="dotted", se=FALSE,color=1)+xlab("Mother's Age(year)") + ylab("Effect on birthweight (grams)") + 
       ggtitle(title_name) + mytheme
     
     
)
dev.off()
plot(plotx, ploty, type = 'l')
lines(plotx, ploty.m, col='red', type = 'l')
lines(plotx, ploty.cbps, col='blue', type = 'c')
