getwd()
df<-read.csv("House_Price.csv")
df

#viewing the data
View(df)

#viewing the structure of data
str(df)
summary(df)


hist(df$crime_rate)

pairs(~price+crime_rate+n_hot_rooms+rainfall+n_hos_beds,data=df)

barplot(table(df$airport))



barplot(table(df$waterbody))

barplot(table(df$bus_ter))





#Observations

#n_hot_rooms and rainfall has outliers
#n_hos_beds has missing values
#bus_term is a useless variable
#crime_rate has some other functional relationship with price


quantile(df$n_hot_rooms,0.99)
uv=3*quantile(df$n_hot_rooms,0.99)
uv
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)


lv=0.3*quantile(df$rainfall,0.01)
lv
df$rainfall[df$rainfall<lv]<-lv 
summary(df$rainfall)

mean(df$n_hos_beds)


mean(df$n_hos_beds,na.rm=TRUE)





which(is.na(df$n_hos_beds))


df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm=TRUE)
summary(df$n_hos_beds)

which(is.na(df$n_hos_beds))


pairs(~price+crime_rate,data=df)

plot(df$price,df$crime_rate)


df$crime_rate=log(1+df$crime_rate)
plot(df$price,df$crime_rate)


df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
View(df)



df2<-df[,-7:-10]
View(df2)


df<-df[,-7:-10]
df
rm(df2)

#install.packages("fastDummies")
library(fastDummies)

cols_to_convert <- "airport"

df <- fastDummies::dummy_cols(df, select_columns = cols_to_convert, remove_first_dummy = FALSE)
df <- df[, -which(names(df) == "airport")]
df <- df[, -which(names(df) == "airport_NO")]


cols_to_convert <- "waterbody"

df <- fastDummies::dummy_cols(df, select_columns = cols_to_convert, remove_first_dummy = FALSE)
df <- df[, -which(names(df) == "waterbody")]
df <- df[, -which(names(df) == "waterbody_None")]
df <- df[, -which(names(df) == "bus_ter")]

cor(df)

round(cor(df),2)


simple_model<-lm(price~room_num,data=df)
summary(simple_model)

plot(df$room_num,df$price)
abline(simple_model)


multiple_model<-lm(price~.,data=df)
summary(multiple_model)


#install.packages("caTools")
library("caTools")

set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
training_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)


lm_a=lm(price~.,data=training_set)
summary(lm_a)

train_a=predict(lm_a,training_set)
test_a=predict(lm_a,test_set)

mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)


#install.packages("leaps")
library("leaps")

lm_best=regsubsets(price~.,data=df,nvmax = 16)
summary(lm_best)

summary(lm_best)$adjr2


which.max(summary(lm_best)$adjr2)


coef(lm_best,9)

lm_forward=regsubsets(price~.,data=df,nvmax = 16,method="forward")
summary(lm_forward)
summary(lm_forward)$adjr2

lm_backward=regsubsets(price~.,data=df,nvmax = 16,method="backward")
summary(lm_backward)
summary(lm_backward)$adjr2


x=model.matrix(price~.,data=df)[,-1]
y=df$price
#install.packages("glmnet")
library("glmnet")
grid=10^seq(10,-2,length=100)
grid



lm_ridge=glmnet(x,y,alpha=0,lambda=grid)

summary(lm_ridge)

cv_fir=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv_fir)

opt_lambda=cv_fir$lambda.min
opt_lambda
tss=sum((y-mean(y))^2)
tss


y_a=predict(lm_ridge,s=opt_lambda,newx=x)
rss=sum((y_a-y)^2)
rss


rsp=1-rss/tss
rsp


lm_lasso=glmnet(x,y,alpha=1,lambda=grid)
lm_lasso

