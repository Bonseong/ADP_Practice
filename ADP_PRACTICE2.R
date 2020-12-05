setwd('D:/ADP/모의고사 2회')
library(corrplot)
library(gvlma)
library(lmtest)
library(DMwR)
library(rpart)
set.seed(12345)

# 통계분석
adm<-read.csv('Admission.csv')
sum(is.na(adm))

## 상관분석
cor(adm[-6])

cor.test(adm$Chance_of_Admit, adm$GRE)
cor.test(adm$Chance_of_Admit, adm$TOEFL)
cor.test(adm$Chance_of_Admit, adm$Univ_Rating)
cor.test(adm$Chance_of_Admit, adm$SOP)
cor.test(adm$Chance_of_Admit, adm$LOR)
cor.test(adm$Chance_of_Admit, adm$CGPA)

corrplot(cor(adm[,-6]),method="number")

## 회귀분석 + 단계적 선택법
str(adm)

adm_lm<-lm(Chance_of_Admit~.,data=adm)
summary(adm_lm)

step(adm_lm, direction='both')

adm_lm2<-lm(Chance_of_Admit ~ GRE + TOEFL + LOR + CGPA + Research, data = adm)
summary(adm_lm2)            

dwtest(adm_lm2)

shapiro.test(resid(adm_lm2))

par(mfrow=c(2,2))
plot(adm_lm2)

# 정형 데이터 마이닝

titan<-read.csv("titanic.csv")
summary(titan)

#cabin, embarked의 "" -> NA 바꾸기
levels(titan$embarked)

levels(titan$embarked)[1]<-NA
table(titan$embarked,useNA="always") #useNA="always"는 NA 개수도 출력

titan$cabin<-ifelse(titan$cabin=="",NA,titan$cabin)
summary(titan)

#데이터 형태 변환
str(titan)

titan$pclass<-as.factor(titan$pclass)
titan$name<-as.character(titan$name)
titan$ticket<-as.character(titan$ticket)
titan$cabin<-as.character(titan$cabin)
titan$survived<-factor(titan$survived,levels=c(0,1),labels=c("dead","survived"))

#결측치 대치


sum(complete.cases(titan))
summary(titan)
titan2<-centralImputation(titan)
summary(titan2)
sum(is.na(titan2))

#age_1생성 및 데이터 추가
titan2<-within(titan2,
                 {
                   age_1=integer(0)
                   age_1[age>=0 & age<10]=0
                   age_1[age>=10 & age<20]=1
                   age_1[age>=20 & age<30]=2
                   age_1[age>=30 & age<40]=3
                   age_1[age>=40 & age<50]=4
                   age_1[age>=50 & age<60]=5
                   age_1[age>=60 & age<70]=6
                   age_1[age>=70 & age<80]=7
                   age_1[age>=80 & age<90]=8
                 })

titan2$age_1<-factor(titan2$age_1,levels=c(0,1,2,3,4,5,6,7,8),
                       labels=c("10세 미만","10대","20대","30대","40대","50대","60대","70대","80대"))

str(titan2)



#---------------------------------------------------------------------------------------
# Q2) 전처리가 완료된 titan 데이터를 train(70%), test(30%) 데이터로 분할하시오.
#    (set.seed(12345)를 실행한 후 데이터를 분할하시오.) 
#    또, train 데이터로 종속변수인 survived(생존 여부)를 독립변수 pclass, sex, sibsp, parch, 
#    fare, embarked로 지정하여 예측하는 분류모델을 3개 이상 생성하고 test 데이터에 대한 
#    예측값을 csv파일로 각각 제출하시오.
#---------------------------------------------------------------------------------------


## < Solution >

#데이터 분할
set.seed(12345)
idx<-sample(1:nrow(titan2),size=nrow(titan2)*0.7,replace=FALSE)
train<-titan2[idx,]
test<-titan2[-idx,]
nrow(train)
nrow(test)

str(train)

#모델링 하기 전 분석에 사용할 변수만 추출
train_1<-train[,c("pclass","survived", "sex", "sibsp", "parch", "fare", "embarked")]
test_1<-test[,c("pclass","survived", "sex", "sibsp", "parch", "fare", "embarked")]

str(train_1)


#모델링 (1) 의사결정나무(깊이를 최대 5개까지, 최소 관측치는 15개 이상)
#install.packages(c("rpart","rpart.plot"))
library(rpart)
library(rpart.plot)

dt.model <- rpart(survived~., method="class", data=train_1, control = rpart.control(maxdepth=5, minsplit=15))

dt.model

prp(dt.model,type=4,extra = 2)

pred<-predict(dt.model,test_1[,-2],type="prob")

write.csv(pred,"decisionTree predict.csv")


library(randomForest)

rf.model<-randomForest(train_1$survived~.,data=train_1)
rf.model
pred<-predict(rf.model, test_1[,2],type='prob')
