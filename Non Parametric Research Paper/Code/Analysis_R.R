##### Non-parametric : Yield and Fertilizer Use ########### 

library(np)
#install.packages("haven")
library(haven)
library(dplyr)
library(tidyr)

setwd("C:/Users/monika/Desktop/UCR/Research/Fertilizer_paper/data")
df=read.csv('master_wht.csv')
df=subset(df, NPK_per_hect<1000)

View(df)
attach(df)


v_wht=df$wht_yield_value
y=c(df$wht_yield)
#y_avg_yield_value=c(df$avg_yield_value)
x=c(df$NPK_per_hect)
df$NPK_per_hect_sq=(df$NPK_per_hect)^2
x2=x^2
x3=x^3
x4=x^4
x5=x^5
x6=x^6
z1=df$ANNUAL  #annual rainfall as control variable 
z2=df$wht_area_irrg_per
z3=df$land_nonagri_per





# Parametric method
par_lin1=lm(y~x, data=df)
summary(par_lin1)
par_quad1=lm(y_wht~x+x2, data=df)
summary(par_quad1)
par_cubic=lm(y_wht~x+x2+x3+x4+x5+x6, data=df)
summary(par_cubic)


##Test if Parametric models are true using Li-Wang non-parametric test 
#Null: relationship is linear 
lin_model=lm(y~ x , x=TRUE, y=TRUE) 
XX=data.frame(x)
npcmstest(model=lin_model, xdat=XX, ydat=y)

#Null: relationship is quadratic  
quad_model=lm(y~ x +x2 , x=TRUE, y=TRUE) 
XX=data.frame(x)
npcmstest(model=quad_model, xdat=XX, ydat=y)

#Null: relationship is Fixed effect model 
library(plm)
df <- transform(df, id2=apply(df[c("DIST", "YEAR")], 1, paste, collapse="."))
fixed_eff = lm(wht_yield ~ NPK_per_hect + NPK_per_hect_sq+ANNUAL+wht_area_irrg_per+land_nonagri_per +factor(DIST), data=df, x=TRUE, y=TRUE)
summary(fixed_eff)

XX=data.frame(x+x2+z1+z2+z3+df$DIST)
npcmstest(model=fixed_eff, xdat=XX, ydat=y)





###### Nonparametric method
bw_cv = npregbw(y~x, tol=.1,ftol=.1, regtype = 'll')
model.np_cv = npreg(bws = bw_cv, gradients = TRUE)

model.np.ll<-npreg(y~x, regtype="ll", bwmethod="cv.ls",gradients=TRUE,residuals=TRUE)
fit.np<-fitted(model.np.ll)
se.model.np.ll<-se(model.np.ll)
upper.ll<-fit.np+1.96*se.model.np.ll
lower.ll<-fit.np-1.96*se.model.np.ll

npplot(bws=bw_cv, plot.errors.method="bootstrap",plot.errors.boot.num=100,
       xlab="Fertilizer (tons/hect)", ylab="Average Yield Value (INR/hect)", main = "Fertilizer Regression on Average Yield Value (LLLS)")
npplot(bws=bw_cv, plot.errors.method = "bootstrap", plot.errors.boot.num = 100,   
       plot.errors.quantiles = c(0.025,0.975), plot.errors.style = "band", gradients = TRUE, col="red",
       xlab="Fertilizer (kg/hect)", ylab="Marginal Effects (beta)", main = "Marginal Effect (beta) of Fertilizer on Wheat Yield (LLLS)")
abline(h=0, col="yellow")
abline(v=c(325,425), col=c("blue","blue"))

summary(model.np_cv)





### Rice Yield and Fertilizer use #########

setwd("C:/Users/rajve/Desktop/UCR/Research/Fertilizer_paper/data")
df=read.csv('master_rice.csv')
df=subset(df, NPK_per_hect<700)

View(df)
attach(df)

v_rice=df$rice_yield_value
y_rice=c(df$rice_yield)
x=c(df$NPK_per_hect)


# Parametric method
par_1<-lm(y_rice~x, data=df)
summary(par_1)
pred_1<-predict(par_1,newdata=df,interval='confidence')


###### Nonparametric method
bw_cv = npregbw(y_rice~x, tol=.1,ftol=.1, regtype = 'll')
model.np_cv = npreg(bws = bw_cv, gradients = TRUE)

model.np.ll<-npreg(y_rice~x, regtype="ll",bwmethod="cv.ls",gradients=TRUE,residuals=TRUE)
fit.np<-fitted(model.np.ll)
se.model.np.ll<-se(model.np.ll)
upper.ll<-fit.np+1.96*se.model.np.ll
lower.ll<-fit.np-1.96*se.model.np.ll

npplot(bws=bw_cv, plot.errors.method="bootstrap",plot.errors.boot.num=100,
       xlab="Fertilizer (tons/hect)", ylab="Average Yield Value (INR/hect)", main = "Fertilizer Regression on Average Yield Value (LLLS)")
npplot(bws=bw_cv, plot.errors.method = "bootstrap", plot.errors.boot.num = 100,   
       plot.errors.quantiles = c(0.025,0.975), plot.errors.style = "band", gradients = TRUE, col="red",
       xlab="Fertilizer (kg/hect)", ylab="Average Wheat Yield (kg/hect)", main = "Marginal Effect (beta) of Fertilizer on Average Wheat Yield (LLLS)")
abline(h=0, col="yellow")
summary(model.np_cv)
