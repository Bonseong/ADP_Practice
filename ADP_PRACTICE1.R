setwd('d:ADP/모의고사 1회')
library(reshape2)
library(arules)
library(dplyr)
# 정형 데이터 마이닝
##1

lotto<-read.csv('lotto.csv')

sum(is.na(lotto))


lot_melt<-melt(lotto, id.vars=1)
lot_melt2<-lot_melt[-2]

lot_sp<-split(lot_melt2$value,lot_melt2$time_id)
lot_ts<-as(lot_sp,"transactions")

inspect(lot_ts[1:5])

itemFrequencyPlot(lot_ts,topN=10,type='absolute')
itemFrequencyPlot(lot_ts,topN=10)

##2
metric.params <- list(supp=0.002, conf=0.8, minlen=2, maxlen=6)
rules_1<-apriori(lot_ts, metric.params)
rules_2<-sort(rules_1, by='lift', decreasing = TRUE)
rules_3<-inspect(rules_2[1:30])
rules_4<-as(rules_3, 'data.frame')
write.csv(rules_4, 'lotto_rules.csv')

##3
summary(rules_1)

'''총 679개의 연관규칙이 도출되었으며, 그중 632개의 규칙은 4개의 번호로, 47개의 규칙은 5개의 번호로 구성되었다.
규칙에 대한 향상도에서 최솟값이 6.410으로 높게 나왔으며, 추첨번호들의 교집합 확률을 의미하는 지지도의 평균은 0.002364로 나왔다.
트랜잭션 데이터의 수는 859개이고, 트랜잭션 데이너 859회 동안의 로또 당첨번호들을 의미한다.'''

rules_most_freq<-subset(rules_1, rhs %in% '34')
inspect(rules_most_freq)

'''총 19개의 규칙이 도출되었으며, 1번규칙에서 {7,22,31}번과 34번이 같이 추첨될 확률은 0.2%이이다.
향상도는 6.42로 이는 34번만 추첨됐을 때 보다 {7,22,31}번이 뽑히고, {34}도 뽑힐 확률이 약 6배 높다는 것을 의미한다.
하지만 이러한 규칙들은 로또번호가 추첨되는 순서를 고려하지 않고 단순히 조합에 대한 확률만을 고려한 규칙이므로, 향상도가 높은 숫자들의 조합이 로또 추첨번호가 될 가능성이 높은 것은 아니다.'''

# 통계분석
##1
fifa<-read.csv('FIFA.csv', encoding='UTF-8')

fifa$Height_cm<-as.numeric(substr(fifa$Height,1,regexpr("'",fifa$Height)-1))*30+as.numeric(substr(fifa$Height,regexpr("'",fifa$Height)+1,nchar(as.character(fifa$Height))))*2.5

##2

fifa<-within(fifa, {
  Position_Class=character(0)
  Position_Class[Position %in% c('LS','ST','RS','LW','LF','CF','RF','RW')]='Forward'
  Position_Class[Position %in% c('LAM','CAM','RAM','LM','LCM','CM','RCM','RM')]='Midfielder'
  Position_Class[Position %in% c('LWB','LDM','CDM','RDM','RWB','LB','LCB','CB','RCB','RB')]='Defender'
  Position_Class[Position %in% c('GK')]='GoalKeeper'
})

fifa$Position_Class<-factor(fifa$Position_Class, levels=c('Forward','Midfielder','Defender','GoalKeeper'), labels=c('Forward','Midfielder','Defender','GoalKeeper'))

##3

fifa_anova<-aov(fifa$Value~fifa$Position_Class)
summary(fifa_anova)

'''
귀무가설 : 네 포지션의 시장가치는 모두 동일하다
대립가설 : 네 포지션 중 적어도 한 시장가치는 동일하지 않다.

분산분석표를 확인한 결과, p-values가 <2e-16이므로 대립가설을 기각하고 귀무가설을 채택한다.
따라서, 네 포지션 중 적어도 한 가치는 동일하지 않다.
'''

TukeyHSD(fifa_anova)

'''
귀무가설 : 집단들 사이 평균은 같다.
대립가설 : 집단들 사이 평균은 같지 않다.

사후 분석 결과, 미드필더와 공격수 사이에서는 p-values>0.05이므로 가치가 동일하다고 할 수 있다.
그러나 나머지 포지션 사이에서는 <0.05이므로 가치가 동일하지 않다고 할 수 있다.
diff 변수는 '-' 앞 변수와 뒷 변수 간 반응값의 차이를 보여주는데, 일반적으로 뒷 변수가 앞 변수보다 값이 큰 것을 확인할 수 있다.
'''

##4

fifa_twoway_anova<-aov(fifa$Value~fifa$Preferred_Foot+fifa$Position_Class+fifa$Preferred_Foot:fifa$Position_Class)
summary(fifa_twoway_anova)

'''
귀무가설 : 주발과 가치에는 차이가 없다, 포지션과 가치에는 차이가 없다, 주발과 포지션에 상호작용이 없다.
대립가설 : 주발과 가치에는 차이가 있다, 포지션과 가치에는 차이가 있다, 주발과 포지션에 상호작용이 있다.

분산분석표를 확인한 결과, 주발과 가치에 대해서 p-values<0.05 이므로 차이가 있다고 할 수 있다.
포지션에 대해서는 p-values<0.05이므로 차이가 있다고 할 수 있다.
주발과 포지션에 대해서 p-values<0.05이므로 상호작용 효과가 있다고 할 수 있다.
'''

##5
fifa_lm<-lm(Value~Age+Overall+Wage+Height_cm+Weight_lb, data=fifa)
step(fifa_lm,direction='both')
#step(lm(Value~1, data=fifa), scope=list(lower=~1,upper=~Age + Overall + Wage + Height_cm + Weight_lb), direction="both")

'''
첫번째 모형인 Age+Overall+Wage+Height_cm+Weight_lb에 대해서 Weight_lb 변수를 제거했을 때 AIC가 가장 작은 것을 확인할 수 있었다.
그러므로 두번째 모형에서는 Weight_lb를 빼고 Age + Overall + Wage + Height_cm만을 확인한 결과, 이 모형이 다른 변수를 추가하거나 제거하는 것보다
AIC가 가장 낮을 것을 확인할 수 있었다.

따라서 최종모형으로 Value ~ Age + Overall + Wage + Height_cm를 사용한다.

최종적으로 추정된 회귀식은
y = -8690.818 - 202.160 * Age + 241.345 * Overall + 184.184 * Wage - 8.445 * Height_cm 이다.
'''

fifa_lm2<-lm(Value ~ Age + Overall + Wage + Height_cm, data = fifa)
summary(fifa_lm2)

'''
stepwise 방법을 이용해 변수선택 후 회귀분석을 시행할 결과, Age, Overall, Wage, Height_cm 변수는 유의수준 0.05 하에서 통계적으로 유의하다.
모형의 결정계수는 0.7908, 수정된 결정계수는 0.7908로 다변량 회귀식은 데이터의 약 80%를 설명하고 있다.
이 모형에 대한 F 통계량에 대한 p-value는 < 0.05이므로 통계적으로 유의하다고 할 수 있다.
'''

# 비정형 텍스트 마이닝

##1
movie<-readLines('영화 기생충_review.txt')
dic<-readLines('영화 기생충_사전.txt')

#buildDictionary(ext_dic = "woorimalsam", user_dic=data.frame(readLines("영화 기생충_사전.txt"),"ncn"),replace_usr_dic = T)




movie[1:10]

clean_txt<-function(txt){
  txt<-tolower(txt)             # 대, 소문자 변환
  txt<-removePunctuation(txt)   # 구두점 제거
  txt<-removeNumbers(txt)       # 숫자 제거
  txt<-stripWhitespace(txt)     # 공백제거
  
  return(txt)
}

movie_clean<-clean_txt(movie)

movie_clean[1:10]


##2

b<-VCorpus(VectorSource(movie))

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}

c<-clean_corpus(b)

dtm<-TermDocumentMatrix(c,control=list(dictionary=dic))

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE) 
d <- data.frame(word = names(v),freq=v)
head(d, 10)


colors<-rainbow(nrow(d))

barplot(v[1:10], main="기생충 review 빈출 명사",col=colors)
legend("right",names(v[1:10]),fill=colors)


##3
movie_exN<-sapply(movie_clean,extractNoun)
Noun<-as.vector(unlist(movie_exN))
Noun_2<-Noun[nchar(Noun)>=2]

result<-data.frame(sort(table(Noun_2),decreasing=T))
t<-wordcloud(result$Noun_2,result$Freq,color=brewer.pal(8,"Dark2"),min.freq=30)

