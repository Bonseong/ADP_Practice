library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(ROCR)
library(caret)
library(tm)
library(rJava)
library(KoNLP)
library(wordcloud)
library(plyr)
library(stringr)
setwd('D:/ADP/모의고사 4회')

AUS<-read.csv('weatherAUS.csv')
bike<-read.csv('bike_marketing.csv')
set.seed(6789)

summary(AUS)

AUS_1<-AUS[,-c(8,14,15,16,17)]
AUS_1
AUS_1$Date<-as.Date(AUS_1$Date)
AUS_1<-na.omit(AUS_1)
sum(is.na(AUS_1))
nrow(AUS_1)

idx<-sample(1:nrow(AUS_1),size=nrow(AUS_1)*0.7,replace=FALSE)
AUS_train<-AUS_1[idx,]
AUS_test<-AUS_1[-idx,]

nrow(AUS_train)
nrow(AUS_test)

## 의사결정나무
tree.fit<-rpart(RainTomorrow~., data=AUS_train, method="class", control = rpart.control(maxdepth=5, minsplit=15))
tree.fit

prp(tree.fit,type=4,extra = 2)


pred_tree<-predict(tree.fit,AUS_test[,-length(AUS_test)],type='prob')
pred_tree2<-predict(tree.fit,AUS_test[,-length(AUS_test)],type='class')

write.csv(cbind(pred_tree,pred_tree2),'decisiontree.csv')

confusionMatrix(data=pred_tree2,reference=AUS_test[,16], positive="Yes")

pred.dt.roc<-prediction(as.numeric(pred_tree2),as.numeric(AUS_test[,16]))
plot(performance(pred.dt.roc,"tpr","fpr"))
abline(a=0,b=1,lty=2,col="black")

performance(pred.dt.roc,"auc")@y.values

## 랜덤포레스트
rf.fit<-randomForest(RainTomorrow~., data=AUS_train)
rf.fit

pred_rf<-predict(rf.fit, AUS_test[,-16], type='prob')
pred_rf2<-predict(rf.fit, AUS_test[,-16], type='class')

write.csv(cbind(pred_rf,pred_rf2), 'randomforest.csv')

confusionMatrix(data=pred_rf2,reference=AUS_test[,16], positive="Yes")

pred.rf.roc<-prediction(as.numeric(pred_rf2),as.numeric(AUS_test[,16]))
plot(performance(pred.rf.roc,"tpr","fpr"))
abline(a=0,b=1,lty=2,col="black")

performance(pred.rf.roc,"auc")@y.values

## 나이브베이즈
nv.fit<-naiveBayes(RainTomorrow~., data=AUS_train)
nv.fit

pred_nv<-predict(nv.fit, AUS_test[,-16], type='raw')
pred_nv2<-predict(nv.fit, AUS_test[,-16], type='class')
head(pred_nv)

write.csv(cbind(pred_nv,pred_nv2), 'naivebayes.csv')

confusionMatrix(data=pred_nv2,reference=AUS_test[,16], positive="Yes")

pred.nv.roc<-prediction(as.numeric(pred_nv2),as.numeric(AUS_test[,16]))
plot(performance(pred.nv.roc,"tpr","fpr"))
abline(a=0,b=1,lty=2,col="black")

performance(pred.nv.roc,"auc")@y.values


## 통계분석
sum(is.na(bike))

bike$pop_density<-as.factor(bike$pop_density)

summary(aov(revenues~pop_density, data=bike))

bike_lm<-lm(revenues~google_adwords+facebook+twitter+marketing_total+employees, data=bike)
summary(bike_lm)

formula_low<-lm(revenues~1, data=bike)
formula_high<-bike_lm

bike_lm2<-step(formula_low, scope=list(upper=formula_high), direction='forward')
summary(bike_lm2)


## 잔차분석
library(lmtest)

dwtest(bike_lm2)

shapiro.test(resid(bike_lm2))

par(mfrow=c(2,2))
plot(bike_lm2)

par(mfrow=c(1,1))
instagram_tour<-readLines("instagram_태교여행.txt")

#데이터 전처리
clean_txt<-function(txt){
  txt<-tolower(txt)             # 대, 소문자 변환
  txt<-removePunctuation(txt)   # 구두점 제거
  txt<-removeNumbers(txt)       # 숫자 제거
  txt<-stripWhitespace(txt)     # 공백제거
  
  return(txt)
}
tour_1<-clean_txt(instagram_tour)
tour_1
#---------------------------------------------------------------------------
# Q2) 전처리된 데이터에서 “태교여행”이란 단어를 사전에 추가하고 명사를 추출해 
#     출현빈도 10위까지 막대그래프로 시각화하시오. 
#---------------------------------------------------------------------------

#buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame(c("태교여행"),"ncn"),replace_usr_dic = T)

tour1<-sapply(tour_1,extractNoun)

table.cnoun<-head(sort(table(unlist(tour1)),decreasing=T),10)

barplot(table.cnoun, main="tour 데이터 빈출 명사", 
        xlab="단어",
        ylab="빈도")

# 3) 전처리된 데이터를 이용해 워드클라우드를 작성하고 인사이트를 추출하시오.

result<-data.frame(sort(table(unlist(tour1)),decreasing=T))

t<-wordcloud(result$Var1,result$Freq,color=brewer.pal(6,"Dark2"),min.freq=20)