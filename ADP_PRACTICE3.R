setwd('D:/ADP/모의고사 3회')

library(ISLR)
library(corrplot)
library(caret)
library(dplyr)

car=Carseats

sum(is.na(car))

var.test(data=car, Sales~Urban, alternative='two.sided')

t.test(data=car, Sales~Urban, alternative='two.sided', var.equal=TRUE)


plot(car[,-c(7,10,11)])

cor(car)


#1. Sales 와 CompPrice 간의 상관분석

cor(Sales, CompPrice)         #피어슨 상관계수 산출
cor.test(Sales, CompPrice)    #피어슨 상관계수 검정


#2. Sales 와 Income 간의 상관분석

cor(Sales, Income)         #피어슨 상관계수 산출
cor.test(Sales, Income)    #피어슨 상관계수 검정


#3. Sales 와 Advertising 간의 상관분석

cor(Sales, Advertising)       #피어슨 상관계수 산출
cor.test(Sales, Advertising)  #피어슨 상관계수 검정


#4. Sales 와 Population 간의 상관분석

cor(Sales, Population)         #피어슨 상관계수 산출
cor.test(Sales, Population)    #피어슨 상관계수 검정


#5. Sales 와 Price 간의 상관분석

cor(Sales, Price)         #피어슨 상관계수 산출
cor.test(Sales, Price)    #피어슨 상관계수 검정


#6. Sales 와 Age 간의 상관분석

cor(Sales, Age)         #피어슨 상관계수 산출
cor.test(Sales, Age)    #피어슨 상관계수 검정


#7. Sales 와 Education 간의 상관분석

cor(Sales, Education)         #피어슨 상관계수 산출
cor.test(Sales, Education)    #피어슨 상관계수 검정


#8. 상관계수 행렬 생성 및 시각화
str(car)
cor(car[,-c(7,10,11)])
plot(car[,-c(7,10,11)])

corrplot(cor(car[,-c(7,10,11)]), method='number')

car[,-c(7,10,11)]



car_lm<-lm(data=car, Sales~CompPrice+Income+Advertising+Population+Price+Age+Education)
step(car_lm, direction = 'backward')

car_lm2<-lm(Sales ~ CompPrice + Income + Advertising + Price + Age, data = car)
summary(car_lm2)



bf<-read.csv('BlackFriday.csv')

bf$Product_Category_2<-ifelse(is.na(bf$Product_Category_2)==TRUE,0,bf$Product_Category_2)
bf$Product_Category_3<-ifelse(is.na(bf$Product_Category_3)==TRUE,0,bf$Product_Category_3)

bf$Product_all=bf$Product_Category_1+bf$Product_Category_2+bf$Product_Category_3

bf$User_ID<-as.character(bf$User_ID)
bf$Occupation<-as.factor(bf$Occupation)
bf$Marital_Status<-as.factor(bf$Marital_Status)
bf$Product_Category_1<-as.factor(bf$Product_Category_1)
bf$Product_Category_2<-as.factor(bf$Product_Category_2)
bf$Product_Category_3<-as.factor(bf$Product_Category_3)

bf_1<-bf %>%
  mutate(gender_num=as.numeric(Gender),
         age_num=as.numeric(Age),
         city_num=as.numeric(City_Category),
         stay_num=as.numeric(Stay_In_Current_City_Years))

dummy<-dummyVars(data=bf_1, "~Gender+Age+City_Category+Stay_In_Current_City_Years")

new_df<-data.frame(predict(dummy, newdata=bf_1))
bf_2<-cbind(bf, new_df)
str(bf_2)

bf_cluster<-bf_2 %>%
  select(-User_ID,-Product_ID,-Gender,-Age,-City_Category,-Stay_In_Current_City_Years,-Product_all)

bf_cluster$Occupation<-as.numeric(bf_cluster$Occupation)
bf_cluster$Marital_Status<-as.numeric(bf_cluster$Marital_Status)
bf_cluster$Product_Category_1<-as.numeric(bf_cluster$Product_Category_1)
bf_cluster$Product_Category_2<-as.numeric(bf_cluster$Product_Category_2)
bf_cluster$Product_Category_3<-as.numeric(bf_cluster$Product_Category_3)

str(bf_cluster)

set.seed(1234)



wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")}

wssplot(bf_cluster)

bf_kmeans_4<-kmeans(bf_cluster,4)
bf_kmeans_4
clust<-bf_kmeans_4$cluster
bf_full<-cbind(bf,clust)
bf_full


table(bf_full$clust)

#Clust별 Gender 요약
xtabs(bf_full$clust ~ bf_full$Gender)

xtabs(~bf_full$clust + bf_full$Gender)


#Clust별 Age 요약
xtabs(bf_full$clust ~ bf_full$Age)

xtabs(~bf_full$clust + bf_full$Age)

aggregate(Purchase~clust, bf_full, mean) 

aggregate(Purchase~Age, bf_full, mean) 

aggregate(Purchase~Gender, bf_full, mean) 

library(tm)
library(rJava)
library(KoNLP)
library(wordcloud)
library(plyr)
library(stringr)


blog<-read.csv("공구 블로그 댓글.txt",sep="\t")

##데이터 전처리
#"\t" 없애기

blog$Date<-as.character(blog$Date)

blog$Date<-substr(blog$Date,1,10)

blog

#데이터 전처리
clean_txt<-function(txt){
  txt<-tolower(txt)             # 대, 소문자 변환
  txt<-removePunctuation(txt)   # 구두점 제거
  txt<-removeNumbers(txt)       # 숫자 제거
  txt<-stripWhitespace(txt)     # 공백제거
  
  return(txt)
}
blog$Content<-clean_txt(blog$Content)


#---------------------------------------------------------------------------
# Q2) ‘사전.txt’를 사전에 추가하고 문서에서 형용사를 추출하여라. 
#---------------------------------------------------------------------------

#사전 추가하기
buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame(readLines("사전.txt"),"ncn"),replace_usr_dic = T)

#형용사 추출하기
doc<-as.character(blog$Content)
pos<-paste(SimplePos09(doc))
extracted<-str_match(pos,'([가-힣]+)/[P]')
keyword<-extracted[,2]
keyword[!is.na(keyword)]

#---------------------------------------------------------------------------
# Q3) 2월에 게시된 댓글의 명사를 추출하고 빈도수를 시각화하시오.
#---------------------------------------------------------------------------

#2월 추출하기
blog$Date<-as.Date(blog$Date, format="%Y.%m.%d")
blog$month<-as.numeric(format(blog$Date, "%m"))
blog$month
blog_2<-subset(blog,blog$month==2)

#2월 댓글의 명사추출
blog2_exN<-sapply(blog_2$Content,extractNoun)
Noun<-as.vector(unlist(blog2_exN))
Noun_2<-Noun[nchar(Noun)>=2]
table.cnoun<-head(sort(table(Noun_2),decreasing=T),5)

#시각화
colors<-rainbow(5)
pie(table.cnoun, main="2월 댓글 빈출 명사",col=colors)
legend("right",names(table.cnoun),fill=colors)




