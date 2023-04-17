library(ggplot2)
library(gridExtra)
library(tidyr)
library(car)
library(arm)
library(dplyr)
data<-read.csv("heart.csv")
View(data)
summary(data)
#Treating categorical predictors
data1<-data
data1$sex<-as.factor(data1$sex)
data1$employment<-as.factor(data1$employment)
data1$edu_level<-as.factor(data1$edu_level)
data1$chest_pain<-as.factor(data1$chest_pain)
data1$fgl<-as.factor(data1$fgl)
data1$rest_ecg<-as.factor(data1$rest_ecg)
data1$ex_ang<-as.factor(data1$ex_ang)
data1$slope<-as.factor(data1$slope)
data1$diab_num<-as.factor(data1$diab_num)
data1$thalass<-as.factor(data1$thalass)
data1$cvd<-as.factor(data1$cvd)

#Exploratory analysis
#Age
age.plot<-ggplot(data1, aes(x=cvd, y=age))+geom_boxplot()
age.plot
#ages have similar means, no direct indication beyond the general 45-low60s age range. Cvd patients have a greater range

#sex
sex.plot<-ggplot(data1, aes(x=sex, fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
sex.plot
#Seams that similar levels of men and women are affected by cvd, with women having the slight edge in this sample.

#Employment
employment.plot<-ggplot(data1, aes(x=employment, fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
employment.plot
#Large variation, unskilled being highest, decreasing with an inverse proportion the level of skill.

#edu_level
edu.plot<-ggplot(data1, aes(x=edu_level, fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
edu.plot
#Shows inverse proportion with the highest education level reached

#chest pain
chest.plot<-ggplot(data1, aes(x=chest_pain, fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name = "Within group Percentage", labels = scales::percent)
chest.plot
#atypical angina shows highest proportion however not angina and no symptoms have similarly high proportions. May not be completely angina related

#rest bps
bps.plot<-ggplot(data1, aes(x=cvd, y=rest_bps))+geom_boxplot()
bps.plot
#Tose with cvd seem to have a much tigheter range in their rest bps. Means are similar though

#cholesterol
cholesterol.plot<-ggplot(data1, aes(x=cvd, y=chol))+geom_boxplot()
cholesterol.plot
#Those with cvd seem to have a smaller range and smaller mean cholesterol concentration. CvD has a hogher peak cholesterol

#fgl
fgl.plot<-ggplot(data1, aes(x=fgl,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
fgl.plot
#Very similar proportions in either group

#rest ecg
ecg.plot<-ggplot(data1, aes(x=rest_ecg,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
ecg.plot
#Those with abnormal st-t experienced the highest cvd contraction. lEft ventricle hypertrophy recorded lower cvd levels than those with normal rest ecg

#max heart rate
maxhr.plot<-ggplot(data1, aes(x=cvd, y=max_r))+geom_boxplot()
maxhr.plot
#Those with cvd have a much higher max hr

#Angina pectoris
ang.plot<-ggplot(data1, aes(x=ex_ang,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
ang.plot
#Those without ex angina pectoris saw a much higher proprotion of cvd patients.

#ST difference
st.plot<-ggplot(data1, aes(x=cvd, y=ST_diff))+geom_boxplot()
st.plot
#ST difference was much shorter in those with cvd than those without.

#Slope
slope.plot<-ggplot(data1, aes(x=slope,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
slope.plot
#Those with an upslope or horizontal had similar levels of those afflicted however downslope showed a much higher within group percentage.

#diab_num
diab.plot<-ggplot(data1, aes(x=diab_num,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
diab.plot
#Those with no fluorescent colours and hogh levels of fluorescent colours had the most cvd patients

#thalass
thalass.plot<-ggplot(data1, aes(x=thalass,fill=cvd))+geom_bar(position="fill")+scale_y_continuous(name="Within group percentage", labels = scales::percent)
thalass.plot
#fixed defect had the highest proportion, reversible and no defect had similar lower levels.

#Treating NAs and levels
data2<-data1
#Age
data2.age<-na.omit(data1$age)

#Sex
summary(data1$sex)
data2<-subset(data1,(sex!="NA's"))
data2$sex<-relevel(data2$sex,ref = "1")

#Employment
summary(data1$employment)
data2$employment<-relevel(data2$employment, ref="1")

#edu_level
summary(data1$edu_level)
data2$edu_level<-relevel(data2$edu_level,ref = "1")

#chest pain
summary(data1$chest_pain)
data2$chest_pain<-relevel(data2$chest_pain,ref = "0")

#rest_bps
summary(data1$rest_bps)

#Cholesterol
summary(data1$chol)

#fgl
summary(data1$fgl)
data2$fgl<-relevel(data2$fgl,ref = "0")

#rest ecg
summary(data1$rest_ecg)
data2$rest_ecg<-relevel(data2$rest_ecg,ref = "0")

#Max HR
summary(data1$max_r)

#Ex ang
summary(data1$ex_ang)
data2$ex_ang<-relevel(data2$ex_ang,ref = "0")

#St diff
summary(data1$ST_diff)

#slope
summary(data1$slope)
data2$slope<-relevel(data2$slope,ref = "1")

#diab_num
summary(data1$diab_num)
data2$diab_num<-relevel(data2$diab_num,ref = "0")

#Thalass
summary(data1$thalass)
data2$thalass<-relevel(data2$thalass,ref = "2")

#Centering age, Max hr, rest bps, cholesterol as none make sense at 0 as patient would be dead.
center.pred<-function(v){
  center.v<-v-mean(v)
  center.v}
cent.age<-center.pred(data2$age)
cent.max<-center.pred(data2$max_r)
cent.bps<-center.pred(data2$rest_bps)
cent.chol<-center.pred(data2$chol)


#Model building
model.1<-glm(cvd~., data= data2, family=binomial(link="logit"))
summary(model.1)
Anova(model.1)
#employment, chest pain, rest ecg, ex ang, diab_num, thalass show levels of significancy
#Themes
demo<-glm(cvd~ cent.age +sex+employment+edu_level, data= data2, family=binomial(link="logit"))
summary(demo)
Anova(demo)
#Employment is the only significant predictor here

physic<-glm(cvd~chest_pain+chol+rest_ecg+rest_bps+fgl+max_r+ex_ang+ST_diff+slope+diab_num+thalass, data= data2, family=binomial(link="logit"))
summary(physic)
Anova(physic)
#Chest pain St diff slope diab_num slope thalass show significancy

#Model consisiting of significant predictors

model.2<-glm(cvd~employment+chest_pain+ST_diff+slope+diab_num+thalass, data= data2, family=binomial(link="logit"))
summary(model.2)
Anova(model.2)

#model eval
k<-134.67-113.35
qchisq(0.95,9)
#15-6=9 degrees of freedom, k=26>16.91 therefore model.2 is a better model than the full unnested model.
#Classification table approach
pred.glm<-as.numeric(model.2$fitted.values>0.5)
glm.dat<-data.frame(predicted=pred.glm,observed=data2$cvd)
table(glm.dat)
ct.op<-function(predicted,observed){
  df.op<-data.frame(predicted=predicted,observed=observed)
  op.tab<-table(df.op)
  op.tab<-rbind(op.tab,c(round(prop.table(op.tab,2)[1,1],2),
                         round((prop.table(op.tab,2)[2,2]),2)))
  rownames(op.tab)<-c("pred=0","pred=1","%corr")
  colnames(op.tab)<-c("obs=0","obs=1")
  op.tab
}
ct.op(pred.glm,data2$cvd) 
pred.1<-as.numeric(model.1$fitted.values>0.5)
ct.op(pred.1, data2$cvd)
#As per the classification table the full model predicts cvd with slightly greater accuracy as well as not having cvd

#Using centered variables in physical model
physic.cent<-glm(cvd~chest_pain+cent.chol+rest_ecg+cent.bps+fgl+cent.max+ex_ang+ST_diff+slope+diab_num+thalass, data= data2, family=binomial(link="logit"))
summary(physic.cent)
Anova(physic.cent)
#using centered variables doesnt change significancy levels

#Model using factors I suggested could be significant from explanatory analysis plots
model.3<-glm(cvd~employment+edu_level+chest_pain+rest_bps+rest_ecg+max_r+ex_ang+ST_diff+diab_num+slope+thalass,data= data2, family=binomial(link="logit"))
summary(model.3)
Anova(model.3)
pred.3<-as.numeric(model.3$fitted.values>0.5)
ct.op(pred.3, data2$cvd)
#Predicts as accurately as model 1
k<-116.02-113.35
qchisq(0.95,4)
#Deviance test shows model.3 is not as good as model 1.
#Adapting model.3 to only include significant predictors
model.4<-glm(cvd~employment+chest_pain+rest_ecg+ex_ang+ST_diff+diab_num+thalass,data= data2, family=binomial(link="logit"))
summary(model.4)
Anova(model.4)
k<-119.5-113.35
qchisq(0.95,8)
pred.4<-as.numeric(model.4$fitted.values>0.5)
ct.op(pred.4, data2$cvd)
#Model 4 from a deviance stand point isn't significantly better than the full unnested model however it predicts cvd with slightly better accuracy in this sample

#Interactions
model.5<-glm(cvd~employment+chest_pain+rest_ecg+ex_ang+ST_diff+diab_num+thalass+chest_pain*ex_ang+employment*chest_pain,data= data2, family=binomial(link="logit"))
summary(model.5)
display(model.5)
Anova(model.5)
anova(model.1,model.5, test="Chisq") #using this anova comparison method we see that model 5 is better than model.1
pred.5<-as.numeric(model.5$fitted.values>0.5)
ct.op(pred.5,data2$cvd)

#chestpain*ex_ang leads to 92,95% accuracy 
#"" plus rest_ecg*ST_diff leads to 93,95
#93,94 for rest_ecg*ST_diff
#94,95 for chest_pain*ex_ang and chest_pain*employment

#Trying out other possible interaction combinations
model.6<-glm(cvd~employment+chest_pain+rest_ecg+ex_ang+ST_diff+diab_num+thalass+rest_ecg*employment+chest_pain*ex_ang,data= data2, family=binomial(link="logit"))
summary(model.6)
anova(model.1,model.4, test="Chisq") #using this anova comparison method we see that model 5 is better than model.1 #Choosing model.5 as it is the best compared to the unnested model
ct.op(pred.5,data2$cvd)
plot(model.5)

#Outliers
show_outliers<-function(the.linear.model,topN){
  
  n=length(fitted(the.linear.model))
  
  p=length(coef(the.linear.model))
  
  res.out<-which(abs(rstandard(the.linear.model))>3) #sometimes >2
  
  res.top<-head(rev(sort(abs(rstandard(the.linear.model)))),topN)
  
  lev.out<-which(lm.influence(the.linear.model)$hat>2*p/n)
  
  lev.top<-head(rev(sort(lm.influence(the.linear.model)$hat)),topN)
  
  dffits.out<-which(dffits(the.linear.model)>2*sqrt(p/n))
  
  dffits.top<-head(rev(sort(dffits(the.linear.model))),topN)
  
  cooks.out<-which(cooks.distance(the.linear.model)>1)
  
  cooks.top<-head(rev(sort(cooks.distance(the.linear.model))),topN)
  list.of.stats<-list(Std.res=res.out,Std.res.top=res.top, Leverage=lev.out, Leverage.top=lev.top, DFFITS=dffits.out, DFFITS.top=dffits.top, Cooks=cooks.out,Cooks.top=cooks.top)
  
  list.of.stats}
#look at the top 5 value in each statistic
outliers<-show_outliers(model.5, 5)
outliers

common.out<-intersect(intersect(outliers$Std.res,outliers$DFFITS),outliers$Leverage)
common.out

#No outliers

qchisq(0.95,15)
corr(model.5)

max(data2$age
    )
