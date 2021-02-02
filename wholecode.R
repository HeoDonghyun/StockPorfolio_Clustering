#주가데이터 전처리

stock<-read.csv('C:/Users/HDH/Desktop/mirae/data/stockprice.csv', stringsAsFactors = F)
head(names(stock))

library(dplyr)
library(reshape)

#데이터 형태 변형 
stockre<-melt(stock, names(stock)[1:3])
stockre$variable<-gsub("X", "", stockre$variable)
stockreM<-stockre
stockreM$variable<-paste(substr(stockre$variable, 1, 4), substr(stockre$variable, 6,7), sep="")
stockreM$value<-replace(stockreM$value, stockreM$value=="", NA) 
stockreM<-na.omit(stockreM)


#종목별 월평균 가격 구하기
stockreM2<-stockreM[, c(1,4,5)]
stockreM2$value<-gsub(",","", stockreM2$value)
stockreM2$value<-as.numeric(stockreM2$value)
stockreM2<-stockreM2 %>% group_by(Symbol, variable) %>% summarize(월평균가격=mean(value)) 

## 데이터 불러오기
ROA<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/ROA.csv', header=T, stringsAsFactor=F)
ROE<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/ROE.csv', header=T, stringsAsFactor=F)
machul<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/매출액.csv', header=T, stringsAsFactor=F)
suneic<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/순이익.csv', header=T, stringsAsFactor=F)
sigachong<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/시가총액.csv', header=T, stringsAsFactor=F)
youdong<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/유동비율.csv', header=T, stringsAsFactor=F)
chongbuchae<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/총부채.csv', header=T, stringsAsFactor=F)
chongbuchae_ratio<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/총부채비율.csv', header=T, stringsAsFactor=F)
chongjasan<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/총자산.csv', header=T, stringsAsFactor=F)
chongjasan_roll<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/총자산회전율.csv', header=T, stringsAsFactor=F)
moneyflow<-read.csv('C:/Users/HDH/Desktop/mirae/data/csv_file/현금흐름.csv', header=T, stringsAsFactor=F)


## 데이터 ','변형 및 문자형의 숫자형화
ROA[, 7:42] <- sapply(ROA[, 7:42], as.numeric)
ROE[, 7:42] <- sapply(ROE[, 7:42], as.numeric)
machul[,7:42]<-sapply(machul[,7:42], function(x) gsub('[$,]','',x))
machul[, 7:42] <- sapply(machul[, 7:42], as.numeric)
suneic[,7:42]<-sapply(suneic[,7:42], function(x) gsub('[$,]','',x))
suneic[, 7:42] <- sapply(suneic[, 7:42], as.numeric)
sigachong[,7:42]<-sapply(sigachong[,7:42], function(x) gsub('[$,]','',x))
sigachong[, 7:42] <- sapply(sigachong[, 7:42], as.numeric)
youdong[, 7:42] <- sapply(youdong[, 7:42], as.numeric)
chongbuchae[,7:42]<-sapply(chongbuchae[,7:42], function(x) gsub('[$,]','',x))
chongbuchae[, 7:42] <- sapply(chongbuchae[, 7:42], as.numeric)
chongbuchae_ratio[, 7:42] <- sapply(chongbuchae_ratio[, 7:42], as.numeric)
chongjasan[,7:42]<-sapply(chongjasan[,7:42], function(x) gsub('[$,]','',x))
chongjasan[, 7:42] <- sapply(chongjasan[, 7:42], as.numeric)
chongjasan_roll[, 7:42] <- sapply(chongjasan_roll[, 7:42], as.numeric)
moneyflow[, 7:42] <- sapply(moneyflow[, 7:42], as.numeric)


## 매출액순이익률 계산 (순이익/매출액)
a<-suneic[,7:42]/machul[,7:42]
b<-suneic[,1:6]
machul_suneic_ratio<-cbind(b, a)


## 2013.03.31 이전 값 제거
ROA<-ROA[,-(7:19)]
ROE<-ROE[,-(7:19)]
machul<-machul[,-(7:19)]
suneic<-suneic[,-(7:19)]
sigachong<-sigachong[,-(7:19)]
youdong<-youdong[,-(7:19)]
chongbuchae<-chongbuchae[,-(7:19)]
chongbuchae_ratio<-chongbuchae_ratio[,-(7:19)]
chongjasan<-chongjasan[,-(7:19)]
chongjasan_roll<-chongjasan_roll[,-(7:19)]
moneyflow<-moneyflow[,-(7:19)]
machul_suneic_ratio<-machul_suneic_ratio[,-(7:19)]

## 분기별 테이블 작성 및 열이름 변경

table.2013.03<-cbind(b, sigachong[,7], chongjasan[,7], machul[,7], 
                     ROA[,7], ROE[,7], chongjasan_roll[,7], machul_suneic_ratio[,7],
                     chongbuchae_ratio[,7], moneyflow[,7])
colnames(table.2013.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2013.06<-cbind(b, sigachong[,8], chongjasan[,8], machul[,8], 
                     ROA[,8], ROE[,8], chongjasan_roll[,8], machul_suneic_ratio[,8],
                     chongbuchae_ratio[,8], moneyflow[,8])
colnames(table.2013.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2013.09<-cbind(b, sigachong[,9], chongjasan[,9], machul[,9], 
                     ROA[,9], ROE[,9], chongjasan_roll[,9], machul_suneic_ratio[,9],
                     chongbuchae_ratio[,9], moneyflow[,9])
colnames(table.2013.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2013.12<-cbind(b, sigachong[,10], chongjasan[,10], machul[,10], 
                     ROA[,10], ROE[,10], chongjasan_roll[,10], machul_suneic_ratio[,10],
                     chongbuchae_ratio[,10], moneyflow[,10])
colnames(table.2013.12)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2014.03<-cbind(b, sigachong[,11], chongjasan[,11], machul[,11], 
                     ROA[,11], ROE[,11], chongjasan_roll[,11], machul_suneic_ratio[,11],
                     chongbuchae_ratio[,11], moneyflow[,11])
colnames(table.2014.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2014.06<-cbind(b, sigachong[,12], chongjasan[,12], machul[,12], 
                     ROA[,12], ROE[,12], chongjasan_roll[,12], machul_suneic_ratio[,12],
                     chongbuchae_ratio[,12], moneyflow[,12])
colnames(table.2014.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2014.09<-cbind(b, sigachong[,13], chongjasan[,13], machul[,13], 
                     ROA[,13], ROE[,13], chongjasan_roll[,13], machul_suneic_ratio[,13],
                     chongbuchae_ratio[,13], moneyflow[,13])
colnames(table.2014.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2014.12<-cbind(b, sigachong[,14], chongjasan[,14], machul[,14], 
                     ROA[,14], ROE[,14], chongjasan_roll[,14], machul_suneic_ratio[,14],
                     chongbuchae_ratio[,14], moneyflow[,14])
colnames(table.2014.12)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2015.03<-cbind(b, sigachong[,15], chongjasan[,15], machul[,15], 
                     ROA[,15], ROE[,15], chongjasan_roll[,15], machul_suneic_ratio[,15],
                     chongbuchae_ratio[,15], moneyflow[,15])
colnames(table.2015.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2015.06<-cbind(b, sigachong[,16], chongjasan[,16], machul[,16], 
                     ROA[,16], ROE[,16], chongjasan_roll[,16], machul_suneic_ratio[,16],
                     chongbuchae_ratio[,16], moneyflow[,16])
colnames(table.2015.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2015.09<-cbind(b, sigachong[,17], chongjasan[,17], machul[,17], 
                     ROA[,17], ROE[,17], chongjasan_roll[,17], machul_suneic_ratio[,17],
                     chongbuchae_ratio[,17], moneyflow[,17])
colnames(table.2015.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2015.12<-cbind(b, sigachong[,18], chongjasan[,18], machul[,18], 
                     ROA[,18], ROE[,18], chongjasan_roll[,18], machul_suneic_ratio[,18],
                     chongbuchae_ratio[,18], moneyflow[,18])
colnames(table.2015.12)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2016.03<-cbind(b, sigachong[,19], chongjasan[,19], machul[,19], 
                     ROA[,19], ROE[,19], chongjasan_roll[,19], machul_suneic_ratio[,19],
                     chongbuchae_ratio[,19], moneyflow[,19])
colnames(table.2016.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2016.06<-cbind(b, sigachong[,20], chongjasan[,20], machul[,20], 
                     ROA[,20], ROE[,20], chongjasan_roll[,20], machul_suneic_ratio[,20],
                     chongbuchae_ratio[,20], moneyflow[,20])
colnames(table.2016.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2016.09<-cbind(b, sigachong[,21], chongjasan[,21], machul[,21], 
                     ROA[,21], ROE[,21], chongjasan_roll[,21], machul_suneic_ratio[,21],
                     chongbuchae_ratio[,21], moneyflow[,21])
colnames(table.2016.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2016.12<-cbind(b, sigachong[,22], chongjasan[,22], machul[,22], 
                     ROA[,22], ROE[,22], chongjasan_roll[,22], machul_suneic_ratio[,22],
                     chongbuchae_ratio[,22], moneyflow[,22])
colnames(table.2016.12)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2017.03<-cbind(b, sigachong[,23], chongjasan[,23], machul[,23], 
                     ROA[,23], ROE[,23], chongjasan_roll[,23], machul_suneic_ratio[,23],
                     chongbuchae_ratio[,23], moneyflow[,23])
colnames(table.2017.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2017.06<-cbind(b, sigachong[,24], chongjasan[,24], machul[,24], 
                     ROA[,24], ROE[,24], chongjasan_roll[,24], machul_suneic_ratio[,24],
                     chongbuchae_ratio[,24], moneyflow[,24])
colnames(table.2017.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2017.09<-cbind(b, sigachong[,25], chongjasan[,25], machul[,25], 
                     ROA[,25], ROE[,25], chongjasan_roll[,25], machul_suneic_ratio[,25],
                     chongbuchae_ratio[,25], moneyflow[,25])
colnames(table.2017.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2017.12<-cbind(b, sigachong[,26], chongjasan[,26], machul[,26], 
                     ROA[,26], ROE[,26], chongjasan_roll[,26], machul_suneic_ratio[,26],
                     chongbuchae_ratio[,26], moneyflow[,26])
colnames(table.2017.12)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2018.03<-cbind(b, sigachong[,27], chongjasan[,27], machul[,27], 
                     ROA[,27], ROE[,27], chongjasan_roll[,27], machul_suneic_ratio[,27],
                     chongbuchae_ratio[,27], moneyflow[,27])
colnames(table.2018.03)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2018.06<-cbind(b, sigachong[,28], chongjasan[,28], machul[,28], 
                     ROA[,28], ROE[,28], chongjasan_roll[,28], machul_suneic_ratio[,28],
                     chongbuchae_ratio[,28], moneyflow[,28])
colnames(table.2018.06)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')
table.2018.09<-cbind(b, sigachong[,29], chongjasan[,29], machul[,29], 
                     ROA[,29], ROE[,29], chongjasan_roll[,29], machul_suneic_ratio[,29],
                     chongbuchae_ratio[,29], moneyflow[,29])
colnames(table.2018.09)<-c('Symbol','Symbol.Name','Kind','Item','Item.Name','Frequency',
                           'sigachong','chongjasan','machul','ROA','ROE','chongjasan_roll',
                           'machul_suneic_ratio','chongbuchae_ratio','moneyflow')


## 삼성전자 제거 ##

table.2013.03<-table.2013.03[-1,]
table.2013.06<-table.2013.06[-1,]
table.2013.09<-table.2013.09[-1,]
table.2013.12<-table.2013.12[-1,]
table.2014.03<-table.2014.03[-1,]
table.2014.06<-table.2014.06[-1,]
table.2014.09<-table.2014.09[-1,]
table.2014.12<-table.2014.12[-1,]
table.2015.03<-table.2015.03[-1,]
table.2015.06<-table.2015.06[-1,]
table.2015.09<-table.2015.09[-1,]
table.2015.12<-table.2015.12[-1,]
table.2016.03<-table.2016.03[-1,]
table.2016.06<-table.2016.06[-1,]
table.2016.09<-table.2016.09[-1,]
table.2016.12<-table.2016.12[-1,]
table.2017.03<-table.2017.03[-1,]
table.2017.06<-table.2017.06[-1,]
table.2017.09<-table.2017.09[-1,]
table.2017.12<-table.2017.12[-1,]
table.2018.03<-table.2018.03[-1,]
table.2018.06<-table.2018.06[-1,]
table.2018.09<-table.2018.09[-1,]


#재무데이터 전처리 

jamoo131Q<-na.omit(table.2013.03[-1295,]) #매출이 0이라 순이익/매출이 -Inf
jamoo131Q<-jamoo131Q[, -c(2,3,4,5,6)]
jamoo132Q<-na.omit(table.2013.06[,-c(2,3,4,5,6)])
jamoo133Q<-na.omit(table.2013.09[,-c(2,3,4,5,6)])
jamoo134Q<-na.omit(table.2013.12[,-c(2,3,4,5,6)])
jamoo141Q<-na.omit(table.2014.03[,-c(2,3,4,5,6)])
jamoo142Q<-na.omit(table.2014.06[,-c(2,3,4,5,6)])
jamoo143Q<-na.omit(table.2014.09[,-c(2,3,4,5,6)])
jamoo144Q<-na.omit(table.2014.12[,-c(2,3,4,5,6)])
jamoo151Q<-na.omit(table.2015.03[,-c(2,3,4,5,6)])
jamoo152Q<-na.omit(table.2015.06[,-c(2,3,4,5,6)])
jamoo153Q<-na.omit(table.2015.09[,-c(2,3,4,5,6)])
jamoo154Q<-na.omit(table.2015.12[,-c(2,3,4,5,6)])
jamoo161Q<-na.omit(table.2016.03[,-c(2,3,4,5,6)])
jamoo162Q<-na.omit(table.2016.06[,-c(2,3,4,5,6)])
jamoo163Q<-na.omit(table.2016.09[,-c(2,3,4,5,6)])
jamoo164Q<-na.omit(table.2016.12[,-c(2,3,4,5,6)])
jamoo171Q<-na.omit(table.2017.03[,-c(2,3,4,5,6)])
jamoo172Q<-na.omit(table.2017.06[,-c(2,3,4,5,6)])
jamoo173Q<-na.omit(table.2017.09[,-c(2,3,4,5,6)])
jamoo174Q<-na.omit(table.2017.12[,-c(2,3,4,5,6)]) 
######################################################
#macro

macro<-read.csv('C:/Users/HDH/Desktop/mirae/data/macro.csv', stringsAsFactors = F)
macro$date<-gsub("-", "",macro$date)
macro2<-na.omit(macro)
names(macro2)[2]<-'날짜'

#주가와 매크로데이터 사이의 상관계수 산출 


macro2$날짜=paste0("20",macro2$날짜)

head(macro2)

head(stockreM2)

names(stockreM2)[2]<-'날짜'
corall<-left_join(stockreM2, macro2, '날짜')
corall<-na.omit(corall)

head(corall)

corall$m2_value=as.numeric(corall$m2_value)

corall2<-corall %>% group_by(Symbol) %>% 
  filter(날짜 %in% c(seq(201301, 201312), seq(201401, 201412), seq(201501, 201512),seq(201601, 201612), seq(201701, 201712))) %>%
  summarize(상관계수DI=cor(월평균가격, dollar_index), 상관계수FI=cor(월평균가격,forward_index ),
                상관계수M=cor(월평균가격, m2_value))



corall2<-na.omit(corall2)

##########################################################
#재무데이터와 매크로데이터 합치기 

jacor131aQ<-left_join(jamoo131Q, corall2, 'Symbol')
jacor132aQ<-left_join(jamoo132Q, corall2, 'Symbol')
jacor133aQ<-left_join(jamoo133Q, corall2, 'Symbol')
jacor134aQ<-left_join(jamoo134Q, corall2, 'Symbol')
jacor141aQ<-left_join(jamoo141Q, corall2, 'Symbol')
jacor142aQ<-left_join(jamoo142Q, corall2, 'Symbol')
jacor143aQ<-left_join(jamoo143Q, corall2, 'Symbol')
jacor144aQ<-left_join(jamoo144Q, corall2, 'Symbol')
jacor151aQ<-left_join(jamoo151Q, corall2, 'Symbol')
jacor152aQ<-left_join(jamoo152Q, corall2, 'Symbol')
jacor153aQ<-left_join(jamoo153Q, corall2, 'Symbol')
jacor154aQ<-left_join(jamoo154Q, corall2, 'Symbol')
jacor161aQ<-left_join(jamoo161Q, corall2, 'Symbol')
jacor162aQ<-left_join(jamoo162Q, corall2, 'Symbol')
jacor163aQ<-left_join(jamoo163Q, corall2, 'Symbol')
jacor164aQ<-left_join(jamoo164Q, corall2, 'Symbol')
jacor171aQ<-left_join(jamoo171Q, corall2, 'Symbol')
jacor172aQ<-left_join(jamoo172Q, corall2, 'Symbol')
jacor173aQ<-left_join(jamoo173Q, corall2, 'Symbol')
jacor174aQ<-left_join(jamoo174Q, corall2, 'Symbol')

#군집분석 가능한 형태로 최종 가공  

jacor131aQ<-na.omit(jacor131aQ)
jacor132aQ<-na.omit(jacor132aQ)
jacor131Q2aQ<-merge(jacor131aQ, jacor132aQ, 'Symbol')
jacor131Q2aQ.s<-scale(jacor131Q2aQ[,-1])
jacor133aQ<-na.omit(jacor133aQ)
jacor132Q3aQ<-merge(jacor132aQ, jacor133aQ, 'Symbol')
jacor132Q3aQ.s<-scale(jacor132Q3aQ[,-1])
jacor134aQ<-na.omit(jacor134aQ)
jacor133Q4aQ<-merge(jacor133aQ, jacor134aQ, 'Symbol')
jacor133Q4aQ.s<-scale(jacor133Q4aQ[,-1])
jacor141aQ<-na.omit(jacor141aQ)
jacor134Q1aQ<-merge(jacor134aQ, jacor141aQ, 'Symbol')
jacor134Q1aQ.s<-scale(jacor134Q1aQ[,-1])
jacor142aQ<-na.omit(jacor142aQ)
jacor141Q2aQ<-merge(jacor141aQ, jacor142aQ, 'Symbol')
jacor141Q2aQ.s<-scale(jacor141Q2aQ[,-1])
jacor143aQ<-na.omit(jacor143aQ)
jacor142Q3aQ<-merge(jacor142aQ, jacor143aQ, 'Symbol')
jacor142Q3aQ.s<-scale(jacor142Q3aQ[,-1])
jacor144aQ<-na.omit(jacor144aQ)
jacor143Q4aQ<-merge(jacor143aQ, jacor144aQ, 'Symbol')
jacor143Q4aQ.s<-scale(jacor143Q4aQ[,-1])
jacor151aQ<-na.omit(jacor151aQ)
jacor144Q1aQ<-merge(jacor144aQ, jacor151aQ, 'Symbol')
jacor144Q1aQ.s<-scale(jacor144Q1aQ[,-1])
jacor152aQ<-na.omit(jacor152aQ)
jacor151Q2aQ<-merge(jacor151aQ, jacor152aQ, 'Symbol')
jacor151Q2aQ.s<-scale(jacor151Q2aQ[,-1])
jacor153aQ<-na.omit(jacor153aQ)
jacor152Q3aQ<-merge(jacor152aQ, jacor153aQ, 'Symbol')
jacor152Q3aQ.s<-scale(jacor152Q3aQ[,-1])
jacor154aQ<-na.omit(jacor154aQ)
jacor153Q4aQ<-merge(jacor153aQ, jacor154aQ, 'Symbol')
jacor153Q4aQ.s<-scale(jacor153Q4aQ[,-1])
jacor161aQ<-na.omit(jacor161aQ)
jacor154Q1aQ<-merge(jacor154aQ, jacor161aQ, 'Symbol')
jacor154Q1aQ.s<-scale(jacor154Q1aQ[,-1])
jacor162aQ<-na.omit(jacor162aQ)
jacor161Q2aQ<-merge(jacor161aQ, jacor162aQ, 'Symbol')
jacor161Q2aQ.s<-scale(jacor161Q2aQ[,-1])
jacor163aQ<-na.omit(jacor163aQ)
jacor162Q3aQ<-merge(jacor162aQ, jacor163aQ, 'Symbol')
jacor162Q3aQ.s<-scale(jacor162Q3aQ[,-1])
jacor164aQ<-na.omit(jacor164aQ)
jacor163Q4aQ<-merge(jacor163aQ, jacor164aQ, 'Symbol')
jacor163Q4aQ.s<-scale(jacor163Q4aQ[,-1])
jacor171aQ<-na.omit(jacor171aQ)
jacor164Q1aQ<-merge(jacor164aQ, jacor171aQ, 'Symbol')
jacor164Q1aQ.s<-scale(jacor164Q1aQ[,-1])
jacor172aQ<-na.omit(jacor172aQ)
jacor171Q2aQ<-merge(jacor171aQ, jacor172aQ, 'Symbol')
jacor171Q2aQ.s<-scale(jacor171Q2aQ[,-1])
jacor173aQ<-na.omit(jacor173aQ)
jacor172Q3aQ<-merge(jacor172aQ, jacor173aQ, 'Symbol')
jacor172Q3aQ.s<-scale(jacor172Q3aQ[,-1])
jacor174aQ<-na.omit(jacor174aQ)
jacor173Q4aQ<-merge(jacor173aQ, jacor174aQ, 'Symbol')
jacor173Q4aQ.s<-scale(jacor173Q4aQ[,-1])


#################################################
# 2. 군집분석



#################################################

library(e1071)

wss<-numeric()

#퍼지군집 1~2000시드중 가장 적합한 시드로 초기 설정 

for (i in 1:2000) { 
  set.seed(i)
  fcl13Q1<-cmeans(jacor131Q2aQ.s[,1:12], centers=4 )
  wss[i] = fcl13Q1$withinerror
}
set.seed(order(wss)[1])

order(wss)[1]

#퍼지군집 시행 
fcl13Q1<-cmeans(jacor131Q2aQ.s[,1:12], centers=4)
fcl13Q2<-cmeans(jacor131Q2aQ.s[,13:24],centers=arrange(data.frame(fcl13Q1$centers)) )

table(fcl13Q1$cluster, fcl13Q2$cluster)
table(fcl13Q1$cluster==fcl13Q2$cluster) 

jacor131Q2aQ$cluster1<-fcl13Q1$cluster
jacor131Q2aQ$cluster2<-fcl13Q2$cluster



fcl13Q22<-cmeans(jacor132Q3aQ.s[,1:12],centers=arrange(data.frame(fcl13Q2$centers)) )
fcl13Q3<-cmeans(jacor132Q3aQ.s[,13:24],centers=arrange(data.frame(fcl13Q2$centers)) )

table(fcl13Q22$cluster, fcl13Q3$cluster)
table(fcl13Q22$cluster==fcl13Q3$cluster)

jacor132Q3aQ$cluster1<-fcl13Q22$cluster
jacor132Q3aQ$cluster2<-fcl13Q3$cluster


fcl13Q32<-cmeans(jacor133Q4aQ.s[,1:12],centers=arrange(data.frame(fcl13Q3$centers)) )
fcl13Q4<-cmeans(jacor133Q4aQ.s[,13:24],centers=arrange(data.frame(fcl13Q3$centers)) )

table(fcl13Q32$cluster, fcl13Q4$cluster)
table(fcl13Q32$cluster==fcl13Q4$cluster)

jacor133Q4aQ$cluster1<-fcl13Q32$cluster
jacor133Q4aQ$cluster2<-fcl13Q4$cluster


fcl13Q42<-cmeans(jacor134Q1aQ.s[,1:12],centers=arrange(data.frame(fcl13Q4$centers)) )
fcl14Q1<-cmeans(jacor134Q1aQ.s[,13:24],centers=arrange(data.frame(fcl13Q4$centers)) )

table(fcl13Q42$cluster, fcl14Q1$cluster)
table(fcl13Q42$cluster==fcl14Q1$cluster)

jacor134Q1aQ$cluster1<-fcl13Q42$cluster
jacor134Q1aQ$cluster2<-fcl14Q1$cluster


fcl14Q12<-cmeans(jacor141Q2aQ.s[,1:12],centers=arrange(data.frame(fcl14Q1$centers)) )
fcl14Q2<-cmeans(jacor141Q2aQ.s[,13:24],centers=arrange(data.frame(fcl14Q1$centers)) )

table(fcl14Q12$cluster, fcl14Q2$cluster)
table(fcl14Q12$cluster==fcl14Q2$cluster)

jacor141Q2aQ$cluster1<-fcl14Q12$cluster
jacor141Q2aQ$cluster2<-fcl14Q2$cluster


fcl14Q22<-cmeans(jacor142Q3aQ.s[,1:12],centers=arrange(data.frame(fcl14Q2$centers)) )
fcl14Q3<-cmeans(jacor142Q3aQ.s[,13:24],centers=arrange(data.frame(fcl14Q2$centers)) )

table(fcl14Q22$cluster, fcl14Q3$cluster)
table(fcl14Q22$cluster==fcl14Q3$cluster)

jacor142Q3aQ$cluster1<-fcl14Q22$cluster
jacor142Q3aQ$cluster2<-fcl14Q3$cluster


fcl14Q32<-cmeans(jacor143Q4aQ.s[,1:12],centers=arrange(data.frame(fcl14Q3$centers)) )
fcl14Q4<-cmeans(jacor143Q4aQ.s[,13:24],centers=arrange(data.frame(fcl14Q3$centers)) )

table(fcl14Q32$cluster, fcl14Q4$cluster)
table(fcl14Q32$cluster==fcl14Q4$cluster)

jacor143Q4aQ$cluster1<-fcl14Q32$cluster
jacor143Q4aQ$cluster2<-fcl14Q4$cluster


fcl14Q42<-cmeans(jacor144Q1aQ.s[,1:12],centers=arrange(data.frame(fcl14Q4$centers)) )
fcl15Q1<-cmeans(jacor144Q1aQ.s[,13:24],centers=arrange(data.frame(fcl14Q4$centers)) )

table(fcl14Q42$cluster, fcl15Q1$cluster)
table(fcl14Q42$cluster==fcl15Q1$cluster)

jacor144Q1aQ$cluster1<-fcl14Q42$cluster
jacor144Q1aQ$cluster2<-fcl15Q1$cluster


fcl15Q12<-cmeans(jacor151Q2aQ.s[,1:12],centers=arrange(data.frame(fcl15Q1$centers)) )
fcl15Q2<-cmeans(jacor151Q2aQ.s[,13:24],centers=arrange(data.frame(fcl15Q1$centers)) )

table(fcl15Q12$cluster, fcl15Q2$cluster)
table(fcl15Q12$cluster==fcl15Q2$cluster)

jacor151Q2aQ$cluster1<-fcl15Q12$cluster
jacor151Q2aQ$cluster2<-fcl15Q2$cluster


fcl15Q22<-cmeans(jacor152Q3aQ.s[,1:12],centers=arrange(data.frame(fcl15Q2$centers)) )
fcl15Q3<-cmeans(jacor152Q3aQ.s[,13:24],centers=arrange(data.frame(fcl15Q2$centers)) )

table(fcl15Q22$cluster, fcl15Q3$cluster)
table(fcl15Q22$cluster==fcl15Q3$cluster)

jacor152Q3aQ$cluster1<-fcl15Q22$cluster
jacor152Q3aQ$cluster2<-fcl15Q3$cluster


fcl15Q32<-cmeans(jacor153Q4aQ.s[,1:12],centers=arrange(data.frame(fcl15Q3$centers)) )
fcl15Q4<-cmeans(jacor153Q4aQ.s[,13:24],centers=arrange(data.frame(fcl15Q3$centers)) )

table(fcl15Q32$cluster, fcl15Q4$cluster)
table(fcl15Q32$cluster==fcl15Q4$cluster)

jacor153Q4aQ$cluster1<-fcl15Q32$cluster
jacor153Q4aQ$cluster2<-fcl15Q4$cluster


fcl15Q42<-cmeans(jacor154Q1aQ.s[,1:12],centers=arrange(data.frame(fcl15Q4$centers)) )
fcl16Q1<-cmeans(jacor154Q1aQ.s[,13:24],centers=arrange(data.frame(fcl15Q4$centers)) )

table(fcl15Q42$cluster, fcl16Q1$cluster)
table(fcl15Q42$cluster==fcl16Q1$cluster)

jacor154Q1aQ$cluster1<-fcl15Q42$cluster
jacor154Q1aQ$cluster2<-fcl16Q1$cluster


fcl16Q12<-cmeans(jacor161Q2aQ.s[,1:12],centers=arrange(data.frame(fcl16Q1$centers)) )
fcl16Q2<-cmeans(jacor161Q2aQ.s[,13:24],centers=arrange(data.frame(fcl16Q1$centers)) )

table(fcl16Q12$cluster, fcl16Q2$cluster)
table(fcl16Q12$cluster==fcl16Q2$cluster)

jacor161Q2aQ$cluster1<-fcl16Q12$cluster
jacor161Q2aQ$cluster2<-fcl16Q2$cluster


fcl16Q22<-cmeans(jacor162Q3aQ.s[,1:12],centers=arrange(data.frame(fcl16Q2$centers)) )
fcl16Q3<-cmeans(jacor162Q3aQ.s[,13:24],centers=arrange(data.frame(fcl16Q2$centers)) )

table(fcl16Q22$cluster, fcl16Q3$cluster)
table(fcl16Q22$cluster==fcl16Q3$cluster)

jacor162Q3aQ$cluster1<-fcl16Q22$cluster
jacor162Q3aQ$cluster2<-fcl16Q3$cluster


fcl16Q32<-cmeans(jacor163Q4aQ.s[,1:12],centers=arrange(data.frame(fcl16Q3$centers)) )
fcl16Q4<-cmeans(jacor163Q4aQ.s[,13:24],centers=arrange(data.frame(fcl16Q3$centers)) )

table(fcl16Q32$cluster, fcl16Q4$cluster)
table(fcl16Q32$cluster==fcl16Q4$cluster)

jacor163Q4aQ$cluster1<-fcl16Q32$cluster
jacor163Q4aQ$cluster2<-fcl16Q4$cluster


fcl16Q42<-cmeans(jacor164Q1aQ.s[,1:12],centers=arrange(data.frame(fcl16Q4$centers)) )
fcl17Q1<-cmeans(jacor164Q1aQ.s[,13:24],centers=arrange(data.frame(fcl16Q4$centers)) )

table(fcl16Q42$cluster, fcl17Q1$cluster)
table(fcl16Q42$cluster==fcl17Q1$cluster)

jacor164Q1aQ$cluster1<-fcl16Q42$cluster
jacor164Q1aQ$cluster2<-fcl17Q1$cluster



fcl17Q12<-cmeans(jacor171Q2aQ.s[,1:12],centers=arrange(data.frame(fcl17Q1$centers)) )
fcl17Q2<-cmeans(jacor171Q2aQ.s[,13:24],centers=arrange(data.frame(fcl17Q1$centers)) )

table(fcl17Q12$cluster, fcl17Q2$cluster)
table(fcl17Q12$cluster==fcl17Q2$cluster)

jacor171Q2aQ$cluster1<-fcl17Q12$cluster
jacor171Q2aQ$cluster2<-fcl17Q2$cluster


fcl17Q22<-cmeans(jacor172Q3aQ.s[,1:12],centers=arrange(data.frame(fcl17Q2$centers)) )
fcl17Q3<-cmeans(jacor172Q3aQ.s[,13:24],centers=arrange(data.frame(fcl17Q2$centers)) )

table(fcl17Q22$cluster, fcl17Q3$cluster)
table(fcl17Q22$cluster==fcl17Q3$cluster)

jacor172Q3aQ$cluster1<-fcl17Q22$cluster
jacor172Q3aQ$cluster2<-fcl17Q3$cluster


fcl17Q32<-cmeans(jacor173Q4aQ.s[,1:12],centers=arrange(data.frame(fcl17Q3$centers)) )
fcl17Q4<-cmeans(jacor173Q4aQ.s[,13:24],centers=arrange(data.frame(fcl17Q3$centers)) )

table(fcl17Q32$cluster, fcl17Q4$cluster)
table(fcl17Q32$cluster==fcl17Q4$cluster)

jacor173Q4aQ$cluster1<-fcl17Q32$cluster
jacor173Q4aQ$cluster2<-fcl17Q4$cluster














#Elbow method

wss<-numeric()

#각 군집개수별 withinerror 산출 
for (i in 1:200) { 
  set.seed(i)
  a<-cmeans(jacor131Q2aQ.s[,1:12], centers=2 )
  wss[i] = a$withinerror
}
set.seed(set.seed(order(wss)[1]))
a<-cmeans(jacor131Q2aQ.s[,1:12], centers=2 )

for (i in 1:200) { 
  set.seed(i)
  b<-cmeans(jacor131Q2aQ.s[,1:12], centers=3 )
  wss[i] = b$withinerror
}
set.seed(set.seed(order(wss)[1]))
b<-cmeans(jacor131Q2aQ.s[,1:12], centers=3 )

for (i in 1:200) { 
  set.seed(i)
  c<-cmeans(jacor131Q2aQ.s[,1:12], centers=4 )
  wss[i] = c$withinerror
}
set.seed(set.seed(order(wss)[1]))
c<-cmeans(jacor131Q2aQ.s[,1:12], centers=4 )

for (i in 1:200) { 
  set.seed(i)
  d<-cmeans(jacor131Q2aQ.s[,1:12], centers=5 )
  wss[i] = d$withinerror
}
set.seed(set.seed(order(wss)[1]))
d<-cmeans(jacor131Q2aQ.s[,1:12], centers=5 )

for (i in 1:200) { 
  set.seed(i)
  e<-cmeans(jacor131Q2aQ.s[,1:12], centers=6 )
  wss[i] = e$withinerror
}
set.seed(set.seed(order(wss)[1]))
e<-cmeans(jacor131Q2aQ.s[,1:12], centers=6 )

#elbow method plot, 군집 4개로 결정 
plot(c(a$withinerror, b$withinerror, c$withinerror, d$withinerror, e$withinerror), type='l', col="red")

par(mfrow=c(1,2))

#PCA를 통해 군집이 나뉜 것을 시각화 
plot(prcomp(jacor131Q2aQ.s[,1:12], center = TRUE,scale=TRUE)$x[,c(1,2)],col =  jacor131Q2aQ$cluster1, pch = jacor131Q2aQ$cluster1)
plot(prcomp(jacor131Q2aQ.s[,13:24], center = TRUE,scale=TRUE)$x[,c(1,2)],col =  jacor131Q2aQ$cluster2, pch = jacor131Q2aQ$cluster2)

#이동한 군집만 시각화 
jacor131Q2aQ$move<-1
jacor131Q2aQ$move<-ifelse(jacor131Q2aQ$cluster1!=jacor131Q2aQ$cluster2, 2, jacor131Q2aQ$move)

plot(prcomp(jacor131Q2aQ.s[,1:12], center = TRUE,scale=TRUE)$x[,c(1,2)])
plot(prcomp(jacor131Q2aQ.s[,13:24], center = TRUE,scale=TRUE)$x[,c(1,2)],col =  jacor131Q2aQ$move, pch = jacor131Q2aQ$move)

#############
# test

jamoo181Q<-na.omit(table.2018.03[,-c(2,3,4,5,6)]) 
jacor181aQ<-left_join(jamoo181Q, corall2, 'Symbol')

jacor181aQ<-na.omit(jacor181aQ)
jacor174Q1aQ<-merge(jacor174aQ, jacor181aQ, 'Symbol')
jacor174Q1aQ.s<-scale(jacor174Q1aQ[,-1])


fcl17Q42<-cmeans(jacor174Q1aQ.s[,1:12],centers=arrange(data.frame(fcl17Q4$centers)) )
fcl18Q1<-cmeans(jacor174Q1aQ.s[,13:24],centers=arrange(data.frame(fcl17Q4$centers)) )

table(fcl17Q42$cluster, fcl18Q1$cluster)
table(fcl17Q42$cluster==fcl18Q1$cluster)

jacor174Q1aQ$cluster1<-fcl17Q42$cluster
jacor174Q1aQ$cluster2<-fcl18Q1$cluster


move1741<-jacor174Q1aQ[,c(1,26,27)]
move1741<-move1741[move1741$cluster1!=move1741$cluster2,]
write.csv(move1741,"C:/Users/HDH/Desktop/mirae/data/move/move1741.csv")

move1741.2<-move1741[move1741$cluster1==4&move1741$cluster2==1, ]
write.csv(move1741.2,"C:/Users/HDH/Desktop/mirae/data/move/move1741t.csv")





#####################################
# 이동저장

#종목명, 원래클러스터, 이동클러스터 

move1312<-jacor131Q2aQ[,c(1,26,27)]
move1323<-jacor132Q3aQ[,c(1,26,27)]
move1334<-jacor133Q4aQ[,c(1,26,27)]
move1341<-jacor134Q1aQ[,c(1,26,27)]
move1412<-jacor141Q2aQ[,c(1,26,27)]
move1423<-jacor142Q3aQ[,c(1,26,27)]
move1434<-jacor143Q4aQ[,c(1,26,27)]
move1441<-jacor144Q1aQ[,c(1,26,27)]
move1512<-jacor151Q2aQ[,c(1,26,27)]
move1523<-jacor152Q3aQ[,c(1,26,27)]
move1534<-jacor153Q4aQ[,c(1,26,27)]
move1541<-jacor154Q1aQ[,c(1,26,27)]
move1612<-jacor161Q2aQ[,c(1,26,27)]
move1623<-jacor162Q3aQ[,c(1,26,27)]
move1634<-jacor163Q4aQ[,c(1,26,27)]
move1641<-jacor164Q1aQ[,c(1,26,27)]
move1712<-jacor171Q2aQ[,c(1,26,27)]
move1723<-jacor172Q3aQ[,c(1,26,27)]
move1734<-jacor173Q4aQ[,c(1,26,27)]

#이동한 클러스터만 저장  
move1312<-move1312[move1312$cluster1!=move1312$cluster2,]
move1323<-move1323[move1323$cluster1!=move1323$cluster2,]
move1334<-move1334[move1334$cluster1!=move1334$cluster2,]
move1341<-move1341[move1341$cluster1!=move1341$cluster2,]
move1412<-move1412[move1412$cluster1!=move1412$cluster2,]
move1423<-move1423[move1423$cluster1!=move1423$cluster2,]
move1434<-move1434[move1434$cluster1!=move1434$cluster2,]
move1441<-move1441[move1441$cluster1!=move1441$cluster2,]
move1512<-move1512[move1512$cluster1!=move1512$cluster2,]
move1523<-move1523[move1523$cluster1!=move1523$cluster2,]
move1534<-move1534[move1534$cluster1!=move1534$cluster2,]
move1541<-move1541[move1541$cluster1!=move1541$cluster2,]
move1612<-move1612[move1612$cluster1!=move1612$cluster2,]
move1623<-move1623[move1623$cluster1!=move1623$cluster2,]
move1634<-move1634[move1634$cluster1!=move1634$cluster2,]
move1641<-move1641[move1641$cluster1!=move1641$cluster2,]
move1712<-move1712[move1712$cluster1!=move1712$cluster2,]
move1723<-move1723[move1723$cluster1!=move1723$cluster2,]
move1734<-move1734[move1734$cluster1!=move1734$cluster2,]

write.csv(move1312,"C:/Users/HDH/Desktop/mirae/data/move/move1312.csv")
write.csv(move1323,"C:/Users/HDH/Desktop/mirae/data/move/move1323.csv")
write.csv(move1334,"C:/Users/HDH/Desktop/mirae/data/move/move1334.csv")
write.csv(move1341,"C:/Users/HDH/Desktop/mirae/data/move/move1341.csv")
write.csv(move1412,"C:/Users/HDH/Desktop/mirae/data/move/move1412.csv")
write.csv(move1423,"C:/Users/HDH/Desktop/mirae/data/move/move1423.csv")
write.csv(move1434,"C:/Users/HDH/Desktop/mirae/data/move/move1434.csv")
write.csv(move1441,"C:/Users/HDH/Desktop/mirae/data/move/move1441.csv")
write.csv(move1512,"C:/Users/HDH/Desktop/mirae/data/move/move1512.csv")
write.csv(move1523,"C:/Users/HDH/Desktop/mirae/data/move/move1523.csv")
write.csv(move1534,"C:/Users/HDH/Desktop/mirae/data/move/move1534.csv")
write.csv(move1541,"C:/Users/HDH/Desktop/mirae/data/move/move1541.csv")
write.csv(move1612,"C:/Users/HDH/Desktop/mirae/data/move/move1612.csv")
write.csv(move1623,"C:/Users/HDH/Desktop/mirae/data/move/move1623.csv")
write.csv(move1634,"C:/Users/HDH/Desktop/mirae/data/move/move1634.csv")
write.csv(move1641,"C:/Users/HDH/Desktop/mirae/data/move/move1641.csv")
write.csv(move1712,"C:/Users/HDH/Desktop/mirae/data/move/move1712.csv")
write.csv(move1723,"C:/Users/HDH/Desktop/mirae/data/move/move1723.csv")
write.csv(move1734,"C:/Users/HDH/Desktop/mirae/data/move/move1734.csv")

#군집분석에 쓰인 종목코드만 따로 저장 
s1312<-data.frame(jacor131Q2aQ[,c(1)])
s1323<-data.frame(jacor132Q3aQ[,c(1)])
s1334<-data.frame(jacor133Q4aQ[,c(1)])
s1341<-data.frame(jacor134Q1aQ[,c(1)])
s1412<-data.frame(jacor141Q2aQ[,c(1)])
s1423<-data.frame(jacor142Q3aQ[,c(1)])
s1434<-data.frame(jacor143Q4aQ[,c(1)])
s1441<-data.frame(jacor144Q1aQ[,c(1)])
s1512<-data.frame(jacor151Q2aQ[,c(1)])
s1523<-data.frame(jacor152Q3aQ[,c(1)])
s1534<-data.frame(jacor153Q4aQ[,c(1)])
s1541<-data.frame(jacor154Q1aQ[,c(1)])
s1612<-data.frame(jacor161Q2aQ[,c(1)])
s1623<-data.frame(jacor162Q3aQ[,c(1)])
s1634<-data.frame(jacor163Q4aQ[,c(1)])
s1641<-data.frame(jacor164Q1aQ[,c(1)])
s1712<-data.frame(jacor171Q2aQ[,c(1)])
s1723<-data.frame(jacor172Q3aQ[,c(1)])
s1734<-data.frame(jacor173Q4aQ[,c(1)])
s1741<-data.frame(jacor174Q1aQ[,c(1)])


write.csv(s1312,"C:/Users/HDH/Desktop/mirae/data/move/s1312.csv")
write.csv(s1323,"C:/Users/HDH/Desktop/mirae/data/move/s1323.csv")
write.csv(s1334,"C:/Users/HDH/Desktop/mirae/data/move/s1334.csv")
write.csv(s1341,"C:/Users/HDH/Desktop/mirae/data/move/s1341.csv")
write.csv(s1412,"C:/Users/HDH/Desktop/mirae/data/move/s1412.csv")
write.csv(s1423,"C:/Users/HDH/Desktop/mirae/data/move/s1423.csv")
write.csv(s1434,"C:/Users/HDH/Desktop/mirae/data/move/s1434.csv")
write.csv(s1441,"C:/Users/HDH/Desktop/mirae/data/move/s1441.csv")
write.csv(s1512,"C:/Users/HDH/Desktop/mirae/data/move/s1512.csv")
write.csv(s1523,"C:/Users/HDH/Desktop/mirae/data/move/s1523.csv")
write.csv(s1534,"C:/Users/HDH/Desktop/mirae/data/move/s1534.csv")
write.csv(s1541,"C:/Users/HDH/Desktop/mirae/data/move/s1541.csv")
write.csv(s1612,"C:/Users/HDH/Desktop/mirae/data/move/s1612.csv")
write.csv(s1623,"C:/Users/HDH/Desktop/mirae/data/move/s1623.csv")
write.csv(s1634,"C:/Users/HDH/Desktop/mirae/data/move/s1634.csv")
write.csv(s1641,"C:/Users/HDH/Desktop/mirae/data/move/s1641.csv")
write.csv(s1712,"C:/Users/HDH/Desktop/mirae/data/move/s1712.csv")
write.csv(s1723,"C:/Users/HDH/Desktop/mirae/data/move/s1723.csv")
write.csv(s1734,"C:/Users/HDH/Desktop/mirae/data/move/s1734.csv")
write.csv(s1741,"C:/Users/HDH/Desktop/mirae/data/move/s1741.csv")

############################################################################
# Session 3
# 수익률 측정

## 군집분석 결과 ## 군집이동 19개

setwd('C:/Users/HDH/Desktop/mirae/data/move')
move1312<-read.csv('move1312.csv', header=T, stringsAsFactors = F)
move1323<-read.csv('move1323.csv', header=T, stringsAsFactors = F)
move1334<-read.csv('move1334.csv', header=T, stringsAsFactors = F)
move1341<-read.csv('move1341.csv', header=T, stringsAsFactors = F)
move1412<-read.csv('move1412.csv', header=T, stringsAsFactors = F)
move1423<-read.csv('move1423.csv', header=T, stringsAsFactors = F)
move1434<-read.csv('move1434.csv', header=T, stringsAsFactors = F)
move1441<-read.csv('move1441.csv', header=T, stringsAsFactors = F)
move1512<-read.csv('move1512.csv', header=T, stringsAsFactors = F)
move1523<-read.csv('move1523.csv', header=T, stringsAsFactors = F)
move1534<-read.csv('move1534.csv', header=T, stringsAsFactors = F)
move1541<-read.csv('move1541.csv', header=T, stringsAsFactors = F)
move1612<-read.csv('move1612.csv', header=T, stringsAsFactors = F)
move1623<-read.csv('move1623.csv', header=T, stringsAsFactors = F)
move1634<-read.csv('move1634.csv', header=T, stringsAsFactors = F)
move1641<-read.csv('move1641.csv', header=T, stringsAsFactors = F)
move1712<-read.csv('move1712.csv', header=T, stringsAsFactors = F)
move1723<-read.csv('move1723.csv', header=T, stringsAsFactors = F)
move1734<-read.csv('move1734.csv', header=T, stringsAsFactors = F)

stock<-read.csv('C:/Users/HDH/Desktop/mirae/data/stockprice.csv', header=T, stringsAsFactors=F)
stock[,-(1:6)]<-sapply(stock[,-(1:6)], function(x) gsub('[$,]','',x))
stock[,-(1:6)]<- sapply(stock[,-(1:6)], as.numeric)

##### <<-- 데이터 불러오기 끝 #####=============================================================

##### 2분기 #####


names(stock)[1326]

names(stock)[598-233]


head(test1312)
# test에 기간별(10,20,30) 수익률 컬럼 추가
move1312$from_to<-paste(move1312$cluster1, move1312$cluster2, sep='-')
test1312<-merge(move1312[,-1], stock[,c(1,1093+233,1093+243,1093+253,1093+263)], by='Symbol')
test1312$benefit_10<-(test1312$X2013.08.25-test1312$X2013.08.15)/(test1312$X2013.08.15)*100
test1312$benefit_20<-(test1312$X2013.09.04-test1312$X2013.08.15)/(test1312$X2013.08.15)*100
test1312$benefit_30<-(test1312$X2013.09.14-test1312$X2013.08.15)/(test1312$X2013.08.15)*100

move1412$from_to<-paste(move1412$cluster1, move1412$cluster2, sep='-')
test1412<-merge(move1412[,-1], stock[,c(1,1093+598,1093+608,1093+618,1093+628)], by='Symbol')
test1412$benefit_10<-(test1412$X2014.08.25-test1412$X2014.08.15)/(test1412$X2014.08.15)*100
test1412$benefit_20<-(test1412$X2014.09.04-test1412$X2014.08.15)/(test1412$X2014.08.15)*100
test1412$benefit_30<-(test1412$X2014.09.14-test1412$X2014.08.15)/(test1412$X2014.08.15)*100

move1512$from_to<-paste(move1512$cluster1, move1512$cluster2, sep='-')
test1512<-merge(move1512[,-1], stock[,c(1,1093+963,1093+973,1093+983,1093+993)], by='Symbol')
test1512$benefit_10<-(test1512$X2015.08.25-test1512$X2015.08.15)/(test1512$X2015.08.15)*100
test1512$benefit_20<-(test1512$X2015.09.04-test1512$X2015.08.15)/(test1512$X2015.08.15)*100
test1512$benefit_30<-(test1512$X2015.09.14-test1512$X2015.08.15)/(test1512$X2015.08.15)*100

move1612$from_to<-paste(move1612$cluster1, move1612$cluster2, sep='-')
test1612<-merge(move1612[,-1], stock[,c(1,1093+1329,1093+1339,1093+1349,1093+1359)], by='Symbol')
test1612$benefit_10<-(test1612$X2016.08.25-test1612$X2016.08.15)/(test1612$X2016.08.15)*100
test1612$benefit_20<-(test1612$X2016.09.04-test1612$X2016.08.15)/(test1612$X2016.08.15)*100
test1612$benefit_30<-(test1612$X2016.09.14-test1612$X2016.08.15)/(test1612$X2016.08.15)*100

move1712$from_to<-paste(move1712$cluster1, move1712$cluster2, sep='-')
test1712<-merge(move1712[,-1], stock[,c(1,1093+1694,1093+1704,1093+1714,1093+1724)], by='Symbol')
test1712$benefit_10<-(test1712$X2017.08.25-test1712$X2017.08.15)/(test1712$X2017.08.15)*100
test1712$benefit_20<-(test1712$X2017.09.04-test1712$X2017.08.15)/(test1712$X2017.08.15)*100
test1712$benefit_30<-(test1712$X2017.09.14-test1712$X2017.08.15)/(test1712$X2017.08.15)*100

move_Q2<-rbind(test1312[,c(4,9,10,11)],
               test1412[,c(4,9,10,11)],
               test1512[,c(4,9,10,11)],
               test1612[,c(4,9,10,11)],
               test1712[,c(4,9,10,11)])

# total_Q2에 군집이동유형별 수익률 평균 컬럼 추가
library(dplyr)
total_Q2<-move_Q2 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

# 최종 total_Q2에 빈도 컬럼 추가
move2<-rbind(move1312[,5], move1412[,5], move1512[,5], move1612[,5], move1712[,5])
table_Q2<-as.data.frame(table(move2))
colnames(table_Q2)<-c('from_to', 'freq')
total_Q2<-merge(total_Q2, table_Q2)

#===================================================================================
## 3분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1323$from_to<-paste(move1323$cluster1, move1323$cluster2, sep='-')
test1323<-merge(move1323[,-1], stock[,c(1,1093+325,1093+335,1093+345,1093+355)], by='Symbol')
test1323$benefit_10<-(test1323$X2013.11.25-test1323$X2013.11.15)/(test1323$X2013.11.15)*100
test1323$benefit_20<-(test1323$X2013.12.05-test1323$X2013.11.15)/(test1323$X2013.11.15)*100
test1323$benefit_30<-(test1323$X2013.12.15-test1323$X2013.11.15)/(test1323$X2013.11.15)*100

move1423$from_to<-paste(move1423$cluster1, move1423$cluster2, sep='-')
test1423<-merge(move1423[,-1], stock[,c(1,1093+690,1093+700,1093+710,1093+720)], by='Symbol')
test1423$benefit_10<-(test1423$X2014.11.25-test1423$X2014.11.15)/(test1423$X2014.11.15)*100
test1423$benefit_20<-(test1423$X2014.12.05-test1423$X2014.11.15)/(test1423$X2014.11.15)*100
test1423$benefit_30<-(test1423$X2014.12.15-test1423$X2014.11.15)/(test1423$X2014.11.15)*100

move1523$from_to<-paste(move1523$cluster1, move1523$cluster2, sep='-')
test1523<-merge(move1523[,-1], stock[,c(1,1093+1055,1093+1065,1093+1075,1093+1085)], by='Symbol')
test1523$benefit_10<-(test1523$X2015.11.25-test1523$X2015.11.15)/(test1523$X2015.11.15)*100
test1523$benefit_20<-(test1523$X2015.12.05-test1523$X2015.11.15)/(test1523$X2015.11.15)*100
test1523$benefit_30<-(test1523$X2015.12.15-test1523$X2015.11.15)/(test1523$X2015.11.15)*100

move1623$from_to<-paste(move1623$cluster1, move1623$cluster2, sep='-')
test1623<-merge(move1623[,-1], stock[,c(1,1093+1421,1093+1431,1093+1441,1093+1451)], by='Symbol')
test1623$benefit_10<-(test1623$X2016.11.25-test1623$X2016.11.15)/(test1623$X2016.11.15)*100
test1623$benefit_20<-(test1623$X2016.12.05-test1623$X2016.11.15)/(test1623$X2016.11.15)*100
test1623$benefit_30<-(test1623$X2016.12.15-test1623$X2016.11.15)/(test1623$X2016.11.15)*100

move1723$from_to<-paste(move1723$cluster1, move1723$cluster2, sep='-')
test1723<-merge(move1723[,-1], stock[,c(1,1093+1786,1093+1796,1093+1806,1093+1816)], by='Symbol')
test1723$benefit_10<-(test1723$X2017.11.25-test1723$X2017.11.15)/(test1723$X2017.11.15)*100
test1723$benefit_20<-(test1723$X2017.12.05-test1723$X2017.11.15)/(test1723$X2017.11.15)*100
test1723$benefit_30<-(test1723$X2017.12.15-test1723$X2017.11.15)/(test1723$X2017.11.15)*100

move_Q3<-rbind(test1323[,c(4,9,10,11)],
               test1423[,c(4,9,10,11)],
               test1523[,c(4,9,10,11)],
               test1623[,c(4,9,10,11)],
               test1723[,c(4,9,10,11)])

# total_Q2에 군집이동유형별 수익률 평균 컬럼 추가
library(dplyr)
total_Q3<-move_Q3 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

# 최종 total_Q2에 빈도 컬럼 추가
move3<-rbind(move1323[,5], move1423[,5], move1523[,5], move1623[,5], move1723[,5])
table_Q3<-as.data.frame(table(move3))
colnames(table_Q3)<-c('from_to', 'freq')
total_Q3<-merge(total_Q3, table_Q3)
#===================================================================================
## 4분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1334$from_to<-paste(move1334$cluster1, move1334$cluster2, sep='-')
test1334<-merge(move1334[,-1], stock[,c(1,1093+460,1093+470,1093+480,1093+490)], by='Symbol')
test1334$benefit_10<-(test1334$X2014.04.09-test1334$X2014.03.30)/(test1334$X2014.03.30)*100
test1334$benefit_20<-(test1334$X2014.04.19-test1334$X2014.03.30)/(test1334$X2014.03.30)*100
test1334$benefit_30<-(test1334$X2014.04.29-test1334$X2014.03.30)/(test1334$X2014.03.30)*100

move1434$from_to<-paste(move1434$cluster1, move1434$cluster2, sep='-')
test1434<-merge(move1434[,-1], stock[,c(1,1093+825,1093+835,1093+845,1093+855)], by='Symbol')
test1434$benefit_10<-(test1434$X2015.04.09-test1434$X2015.03.30)/(test1434$X2015.03.30)*100
test1434$benefit_20<-(test1434$X2015.04.19-test1434$X2015.03.30)/(test1434$X2015.03.30)*100
test1434$benefit_30<-(test1434$X2015.04.29-test1434$X2015.03.30)/(test1434$X2015.03.30)*100

move1534$from_to<-paste(move1534$cluster1, move1534$cluster2, sep='-')
test1534<-merge(move1534[,-1], stock[,c(1,1093+1191,1093+1201,1093+1211,1093+1221)], by='Symbol')
test1534$benefit_10<-(test1534$X2016.04.09-test1534$X2016.03.30)/(test1534$X2016.03.30)*100
test1534$benefit_20<-(test1534$X2016.04.19-test1534$X2016.03.30)/(test1534$X2016.03.30)*100
test1534$benefit_30<-(test1534$X2016.04.29-test1534$X2016.03.30)/(test1534$X2016.03.30)*100

move1634$from_to<-paste(move1634$cluster1, move1634$cluster2, sep='-')
test1634<-merge(move1634[,-1], stock[,c(1,1093+1556,1093+1566,1093+1576,1093+1586)], by='Symbol')
test1634$benefit_10<-(test1634$X2017.04.09-test1634$X2017.03.30)/(test1634$X2017.03.30)*100
test1634$benefit_20<-(test1634$X2017.04.19-test1634$X2017.03.30)/(test1634$X2017.03.30)*100
test1634$benefit_30<-(test1634$X2017.04.29-test1634$X2017.03.30)/(test1634$X2017.03.30)*100

move1734$from_to<-paste(move1734$cluster1, move1734$cluster2, sep='-')
test1734<-merge(move1734[,-1], stock[,c(1,1093+1921,1093+1931,1093+1941,1093+1951)], by='Symbol')
test1734$benefit_10<-(test1734$X2018.04.09-test1734$X2018.03.30)/(test1734$X2018.03.30)*100
test1734$benefit_20<-(test1734$X2018.04.19-test1734$X2018.03.30)/(test1734$X2018.03.30)*100
test1734$benefit_30<-(test1734$X2018.04.29-test1734$X2018.03.30)/(test1734$X2018.03.30)*100

move_Q4<-rbind(test1334[,c(4,9,10,11)],
               test1434[,c(4,9,10,11)],
               test1534[,c(4,9,10,11)],
               test1634[,c(4,9,10,11)],
               test1734[,c(4,9,10,11)])

# total_Q2에 군집이동유형별 수익률 평균 컬럼 추가
library(dplyr)
total_Q4<-move_Q4 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

# 최종 total_Q2에 빈도 컬럼 추가
move4<-rbind(move1334[,5], move1434[,5], move1534[,5], move1634[,5], move1734[,5])
table_Q4<-as.data.frame(table(move4))
colnames(table_Q4)<-c('from_to', 'freq')
total_Q4<-merge(total_Q4, table_Q4)
#===================================================================================
## 1분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1341$from_to<-paste(move1341$cluster1, move1341$cluster2, sep='-')
test1341<-merge(move1341[,-1], stock[,c(1,1093+506,1093+516,1093+526,1093+536)], by='Symbol')
test1341$benefit_10<-(test1341$X2014.05.25-test1341$X2014.05.15)/(test1341$X2014.05.15)*100
test1341$benefit_20<-(test1341$X2014.06.04-test1341$X2014.05.15)/(test1341$X2014.05.15)*100
test1341$benefit_30<-(test1341$X2014.06.14-test1341$X2014.05.15)/(test1341$X2014.05.15)*100

move1441$from_to<-paste(move1441$cluster1, move1441$cluster2, sep='-')
test1441<-merge(move1441[,-1], stock[,c(1,1093+871,1093+881,1093+891,1093+901)], by='Symbol')
test1441$benefit_10<-(test1441$X2015.05.25-test1441$X2015.05.15)/(test1441$X2015.05.15)*100
test1441$benefit_20<-(test1441$X2015.06.04-test1441$X2015.05.15)/(test1441$X2015.05.15)*100
test1441$benefit_30<-(test1441$X2015.06.14-test1441$X2015.05.15)/(test1441$X2015.05.15)*100

move1541$from_to<-paste(move1541$cluster1, move1541$cluster2, sep='-')
test1541<-merge(move1541[,-1], stock[,c(1,1093+1237,1093+1247,1093+1257,1093+1267)], by='Symbol')
test1541$benefit_10<-(test1541$X2016.05.25-test1541$X2016.05.15)/(test1541$X2016.05.15)*100
test1541$benefit_20<-(test1541$X2016.06.04-test1541$X2016.05.15)/(test1541$X2016.05.15)*100
test1541$benefit_30<-(test1541$X2016.06.14-test1541$X2016.05.15)/(test1541$X2016.05.15)*100

move1641$from_to<-paste(move1641$cluster1, move1641$cluster2, sep='-')
test1641<-merge(move1641[,-1], stock[,c(1,1093+1602,1093+1612,1093+1622,1093+1632)], by='Symbol')
test1641$benefit_10<-(test1641$X2017.05.25-test1641$X2017.05.15)/(test1641$X2017.05.15)*100
test1641$benefit_20<-(test1641$X2017.06.04-test1641$X2017.05.15)/(test1641$X2017.05.15)*100
test1641$benefit_30<-(test1641$X2017.06.14-test1641$X2017.05.15)/(test1641$X2017.05.15)*100


move_Q1<-rbind(test1341[,c(4,9,10,11)],
               test1441[,c(4,9,10,11)],
               test1541[,c(4,9,10,11)],
               test1641[,c(4,9,10,11)])

# total_Q2에 군집이동유형별 수익률 평균 컬럼 추가
library(dplyr)
total_Q1<-move_Q1 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

# 최종 total_Q2에 빈도 컬럼 추가
move1<-rbind(move1341[,5], move1441[,5], move1541[,5], move1641[,5])
table_Q1<-as.data.frame(table(move1))
colnames(table_Q1)<-c('from_to', 'freq')
total_Q1<-merge(total_Q1, table_Q1)

#######################
########################

stock<-read.csv('C:/Users/HDH/Desktop/mirae/data/stockprice.csv', stringsAsFactor=F)
stock[,-(1:6)]<-sapply(stock[,-(1:6)], function(x) gsub('[$,]','',x))
stock[,-(1:6)]<-sapply(stock[,-(1:6)], as.numeric)

length(names(stock))

head(names(stock))
stock=stock[,c(1:3,1097:length(names(stock)))]

##### 2분기 #####

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1312$from_to<-paste(move1312$cluster1, move1312$cluster2, sep='-')
test1312<-merge(move1312[,-1], stock[,c(1,233,243,253,263)], by='Symbol')
test1312$benefit_10<-(test1312$X2013.08.25-test1312$X2013.08.15)/(test1312$X2013.08.15)*100
test1312$benefit_20<-(test1312$X2013.09.04-test1312$X2013.08.15)/(test1312$X2013.08.15)*100
test1312$benefit_30<-(test1312$X2013.09.14-test1312$X2013.08.15)/(test1312$X2013.08.15)*100

library(dplyr)
total1312<-test1312 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30))  %>% as.data.frame()

table1312<-as.data.frame(table(move1312[,5]))
colnames(table1312)<-c('from_to', 'freq')
total1312<-merge(total1312, table1312)



move1412$from_to<-paste(move1412$cluster1, move1412$cluster2, sep='-')
test1412<-merge(move1412[,-1], stock[,c(1,598,608,618,628)], by='Symbol')
test1412$benefit_10<-(test1412$X2014.08.25-test1412$X2014.08.15)/(test1412$X2014.08.15)*100
test1412$benefit_20<-(test1412$X2014.09.04-test1412$X2014.08.15)/(test1412$X2014.08.15)*100
test1412$benefit_30<-(test1412$X2014.09.14-test1412$X2014.08.15)/(test1412$X2014.08.15)*100
total1412<-test1412 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1412<-as.data.frame(table(move1412[,5]))
colnames(table1412)<-c('from_to', 'freq')
total1412<-merge(total1412, table1412)


move1512$from_to<-paste(move1512$cluster1, move1512$cluster2, sep='-')
test1512<-merge(move1512[,-1], stock[,c(1,963,973,983,993)], by='Symbol')
test1512$benefit_10<-(test1512$X2015.08.25-test1512$X2015.08.15)/(test1512$X2015.08.15)*100
test1512$benefit_20<-(test1512$X2015.09.04-test1512$X2015.08.15)/(test1512$X2015.08.15)*100
test1512$benefit_30<-(test1512$X2015.09.14-test1512$X2015.08.15)/(test1512$X2015.08.15)*100
total1512<-test1512 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1512<-as.data.frame(table(move1512[,5]))
colnames(table1512)<-c('from_to', 'freq')
total1512<-merge(total1512, table1512)

move1612$from_to<-paste(move1612$cluster1, move1612$cluster2, sep='-')
test1612<-merge(move1612[,-1], stock[,c(1,1329,1339,1349,1359)], by='Symbol')
test1612$benefit_10<-(test1612$X2016.08.25-test1612$X2016.08.15)/(test1612$X2016.08.15)*100
test1612$benefit_20<-(test1612$X2016.09.04-test1612$X2016.08.15)/(test1612$X2016.08.15)*100
test1612$benefit_30<-(test1612$X2016.09.14-test1612$X2016.08.15)/(test1612$X2016.08.15)*100
total1612<-test1612 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1612<-as.data.frame(table(move1612[,5]))
colnames(table1612)<-c('from_to', 'freq')
total1612<-merge(total1612, table1612)

move1712$from_to<-paste(move1712$cluster1, move1712$cluster2, sep='-')
test1712<-merge(move1712[,-1], stock[,c(1,1694,1704,1714,1724)], by='Symbol')
test1712$benefit_10<-(test1712$X2017.08.25-test1712$X2017.08.15)/(test1712$X2017.08.15)*100
test1712$benefit_20<-(test1712$X2017.09.04-test1712$X2017.08.15)/(test1712$X2017.08.15)*100
test1712$benefit_30<-(test1712$X2017.09.14-test1712$X2017.08.15)/(test1712$X2017.08.15)*100
total1712<-test1712 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1712<-as.data.frame(table(move1712[,5]))
colnames(table1712)<-c('from_to', 'freq')
total1712<-merge(total1712, table1712)
#===================================================================================
## 3분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1323$from_to<-paste(move1323$cluster1, move1323$cluster2, sep='-')
test1323<-merge(move1323[,-1], stock[,c(1,325,335,345,355)], by='Symbol')
test1323$benefit_10<-(test1323$X2013.11.25-test1323$X2013.11.15)/(test1323$X2013.11.15)*100
test1323$benefit_20<-(test1323$X2013.12.05-test1323$X2013.11.15)/(test1323$X2013.11.15)*100
test1323$benefit_30<-(test1323$X2013.12.15-test1323$X2013.11.15)/(test1323$X2013.11.15)*100

library(dplyr)
total1323<-test1323 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

table1323<-as.data.frame(table(move1323[,5]))
colnames(table1323)<-c('from_to', 'freq')
total1323<-merge(total1323, table1323)



move1423$from_to<-paste(move1423$cluster1, move1423$cluster2, sep='-')
test1423<-merge(move1423[,-1], stock[,c(1,690,700,710,720)], by='Symbol')
test1423$benefit_10<-(test1423$X2014.11.25-test1423$X2014.11.15)/(test1423$X2014.11.15)*100
test1423$benefit_20<-(test1423$X2014.12.05-test1423$X2014.11.15)/(test1423$X2014.11.15)*100
test1423$benefit_30<-(test1423$X2014.12.15-test1423$X2014.11.15)/(test1423$X2014.11.15)*100
total1423<-test1423 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1423<-as.data.frame(table(move1423[,5]))
colnames(table1423)<-c('from_to', 'freq')
total1423<-merge(total1423, table1423)


move1523$from_to<-paste(move1523$cluster1, move1523$cluster2, sep='-')
test1523<-merge(move1523[,-1], stock[,c(1,1055,1065,1075,1085)], by='Symbol')
test1523$benefit_10<-(test1523$X2015.11.25-test1523$X2015.11.15)/(test1523$X2015.11.15)*100
test1523$benefit_20<-(test1523$X2015.12.05-test1523$X2015.11.15)/(test1523$X2015.11.15)*100
test1523$benefit_30<-(test1523$X2015.12.15-test1523$X2015.11.15)/(test1523$X2015.11.15)*100
total1523<-test1523 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1523<-as.data.frame(table(move1523[,5]))
colnames(table1523)<-c('from_to', 'freq')
total1523<-merge(total1523, table1523)

move1623$from_to<-paste(move1623$cluster1, move1623$cluster2, sep='-')
test1623<-merge(move1623[,-1], stock[,c(1,1421,1431,1441,1451)], by='Symbol')
test1623$benefit_10<-(test1623$X2016.11.25-test1623$X2016.11.15)/(test1623$X2016.11.15)*100
test1623$benefit_20<-(test1623$X2016.12.05-test1623$X2016.11.15)/(test1623$X2016.11.15)*100
test1623$benefit_30<-(test1623$X2016.12.15-test1623$X2016.11.15)/(test1623$X2016.11.15)*100
total1623<-test1623 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1623<-as.data.frame(table(move1623[,5]))
colnames(table1623)<-c('from_to', 'freq')
total1623<-merge(total1623, table1623)

move1723$from_to<-paste(move1723$cluster1, move1723$cluster2, sep='-')
test1723<-merge(move1723[,-1], stock[,c(1,1786,1796,1806,1816)], by='Symbol')
test1723$benefit_10<-(test1723$X2017.11.25-test1723$X2017.11.15)/(test1723$X2017.11.15)*100
test1723$benefit_20<-(test1723$X2017.12.05-test1723$X2017.11.15)/(test1723$X2017.11.15)*100
test1723$benefit_30<-(test1723$X2017.12.15-test1723$X2017.11.15)/(test1723$X2017.11.15)*100
total1723<-test1723 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1723<-as.data.frame(table(move1723[,5]))
colnames(table1723)<-c('from_to', 'freq')
total1723<-merge(total1723, table1723)
#===================================================================================
## 4분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1334$from_to<-paste(move1334$cluster1, move1334$cluster2, sep='-')
test1334<-merge(move1334[,-1], stock[,c(1,460,470,480,490)], by='Symbol')
test1334$benefit_10<-(test1334$X2014.04.09-test1334$X2014.03.30)/(test1334$X2014.03.30)*100
test1334$benefit_20<-(test1334$X2014.04.19-test1334$X2014.03.30)/(test1334$X2014.03.30)*100
test1334$benefit_30<-(test1334$X2014.04.29-test1334$X2014.03.30)/(test1334$X2014.03.30)*100

library(dplyr)
total1334<-test1334 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

table1334<-as.data.frame(table(move1334[,5]))
colnames(table1334)<-c('from_to', 'freq')
total1334<-merge(total1334, table1334)



move1434$from_to<-paste(move1434$cluster1, move1434$cluster2, sep='-')
test1434<-merge(move1434[,-1], stock[,c(1,825,835,845,855)], by='Symbol')
test1434$benefit_10<-(test1434$X2015.04.09-test1434$X2015.03.30)/(test1434$X2015.03.30)*100
test1434$benefit_20<-(test1434$X2015.04.19-test1434$X2015.03.30)/(test1434$X2015.03.30)*100
test1434$benefit_30<-(test1434$X2015.04.29-test1434$X2015.03.30)/(test1434$X2015.03.30)*100
total1434<-test1434 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1434<-as.data.frame(table(move1434[,5]))
colnames(table1434)<-c('from_to', 'freq')
total1434<-merge(total1434, table1434)


move1534$from_to<-paste(move1534$cluster1, move1534$cluster2, sep='-')
test1534<-merge(move1534[,-1], stock[,c(1,1191,1201,1211,1221)], by='Symbol')
test1534$benefit_10<-(test1534$X2016.04.09-test1534$X2016.03.30)/(test1534$X2016.03.30)*100
test1534$benefit_20<-(test1534$X2016.04.19-test1534$X2016.03.30)/(test1534$X2016.03.30)*100
test1534$benefit_30<-(test1534$X2016.04.29-test1534$X2016.03.30)/(test1534$X2016.03.30)*100
total1534<-test1534 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1534<-as.data.frame(table(move1534[,5]))
colnames(table1534)<-c('from_to', 'freq')
total1534<-merge(total1534, table1534)

move1634$from_to<-paste(move1634$cluster1, move1634$cluster2, sep='-')
test1634<-merge(move1634[,-1], stock[,c(1,1556,1566,1576,1586)], by='Symbol')
test1634$benefit_10<-(test1634$X2017.04.09-test1634$X2017.03.30)/(test1634$X2017.03.30)*100
test1634$benefit_20<-(test1634$X2017.04.19-test1634$X2017.03.30)/(test1634$X2017.03.30)*100
test1634$benefit_30<-(test1634$X2017.04.29-test1634$X2017.03.30)/(test1634$X2017.03.30)*100
total1634<-test1634 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1634<-as.data.frame(table(move1634[,5]))
colnames(table1634)<-c('from_to', 'freq')
total1634<-merge(total1634, table1634)

move1734$from_to<-paste(move1734$cluster1, move1734$cluster2, sep='-')
test1734<-merge(move1734[,-1], stock[,c(1,1921,1931,1941,1951)], by='Symbol')
test1734$benefit_10<-(test1734$X2018.04.09-test1734$X2018.03.30)/(test1734$X2018.03.30)*100
test1734$benefit_20<-(test1734$X2018.04.19-test1734$X2018.03.30)/(test1734$X2018.03.30)*100
test1734$benefit_30<-(test1734$X2018.04.29-test1734$X2018.03.30)/(test1734$X2018.03.30)*100
total1734<-test1734 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1734<-as.data.frame(table(move1734[,5]))
colnames(table1734)<-c('from_to', 'freq')
total1734<-merge(total1734, table1734)


#===================================================================================
## 1분기 ##

# test에 기간별(10,20,30) 수익률 컬럼 추가
move1341$from_to<-paste(move1341$cluster1, move1341$cluster2, sep='-')
test1341<-merge(move1341[,-1], stock[,c(1,506,516,526,536)], by='Symbol')
test1341$benefit_10<-(test1341$X2014.05.25-test1341$X2014.05.15)/(test1341$X2014.05.15)*100
test1341$benefit_20<-(test1341$X2014.06.04-test1341$X2014.05.15)/(test1341$X2014.05.15)*100
test1341$benefit_30<-(test1341$X2014.06.14-test1341$X2014.05.15)/(test1341$X2014.05.15)*100

library(dplyr)
total1341<-test1341 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()

table1341<-as.data.frame(table(move1341[,5]))
colnames(table1341)<-c('from_to', 'freq')
total1341<-merge(total1341, table1341)



move1441$from_to<-paste(move1441$cluster1, move1441$cluster2, sep='-')
test1441<-merge(move1441[,-1], stock[,c(1,871,881,891,901)], by='Symbol')
test1441$benefit_10<-(test1441$X2015.05.25-test1441$X2015.05.15)/(test1441$X2015.05.15)*100
test1441$benefit_20<-(test1441$X2015.06.04-test1441$X2015.05.15)/(test1441$X2015.05.15)*100
test1441$benefit_30<-(test1441$X2015.06.14-test1441$X2015.05.15)/(test1441$X2015.05.15)*100
total1441<-test1441 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1441<-as.data.frame(table(move1441[,5]))
colnames(table1441)<-c('from_to', 'freq')
total1441<-merge(total1441, table1441)


move1541$from_to<-paste(move1541$cluster1, move1541$cluster2, sep='-')
test1541<-merge(move1541[,-1], stock[,c(1,1237,1247,1257,1267)], by='Symbol')
test1541$benefit_10<-(test1541$X2016.05.25-test1541$X2016.05.15)/(test1541$X2016.05.15)*100
test1541$benefit_20<-(test1541$X2016.06.04-test1541$X2016.05.15)/(test1541$X2016.05.15)*100
test1541$benefit_30<-(test1541$X2016.06.14-test1541$X2016.05.15)/(test1541$X2016.05.15)*100
total1541<-test1541 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30)) %>% as.data.frame()
table1541<-as.data.frame(table(move1541[,5]))
colnames(table1541)<-c('from_to', 'freq')
total1541<-merge(total1541, table1541)

move1641$from_to<-paste(move1641$cluster1, move1641$cluster2, sep='-')
test1641<-merge(move1641[,-1], stock[,c(1,1602,1612,1622,1632)], by='Symbol')
test1641$benefit_10<-(test1641$X2017.05.25-test1641$X2017.05.15)/(test1641$X2017.05.15)*100
test1641$benefit_20<-(test1641$X2017.06.04-test1641$X2017.05.15)/(test1641$X2017.05.15)*100
test1641$benefit_30<-(test1641$X2017.06.14-test1641$X2017.05.15)/(test1641$X2017.05.15)*100
total1641<-test1641 %>% group_by(from_to) %>% na.omit()%>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30))  %>% as.data.frame()
table1641<-as.data.frame(table(move1641[,5]))
colnames(table1641)<-c('from_to', 'freq')
total1641<-merge(total1641, table1641)





##########################
############################


















setwd('C:/Users/HDH/Desktop/mirae/data/move')
s1312<-read.csv('s1312.csv', header=F, stringsAsFactors = F)[-1,]
s1323<-read.csv('s1323.csv', header=F, stringsAsFactors = F)[-1,]
s1334<-read.csv('s1334.csv', header=F, stringsAsFactors = F)[-1,]
s1341<-read.csv('s1341.csv', header=F, stringsAsFactors = F)[-1,]
s1412<-read.csv('s1412.csv', header=F, stringsAsFactors = F)[-1,]
s1423<-read.csv('s1423.csv', header=F, stringsAsFactors = F)[-1,]
s1434<-read.csv('s1434.csv', header=F, stringsAsFactors = F)[-1,]
s1441<-read.csv('s1441.csv', header=F, stringsAsFactors = F)[-1,]
s1512<-read.csv('s1512.csv', header=F, stringsAsFactors = F)[-1,]
s1523<-read.csv('s1523.csv', header=F, stringsAsFactors = F)[-1,]
s1534<-read.csv('s1534.csv', header=F, stringsAsFactors = F)[-1,]
s1541<-read.csv('s1541.csv', header=F, stringsAsFactors = F)[-1,]
s1612<-read.csv('s1612.csv', header=F, stringsAsFactors = F)[-1,]
s1623<-read.csv('s1623.csv', header=F, stringsAsFactors = F)[-1,]
s1634<-read.csv('s1634.csv', header=F, stringsAsFactors = F)[-1,]
s1641<-read.csv('s1641.csv', header=F, stringsAsFactors = F)[-1,]
s1712<-read.csv('s1712.csv', header=F, stringsAsFactors = F)[-1,]
s1723<-read.csv('s1723.csv', header=F, stringsAsFactors = F)[-1,]
s1734<-read.csv('s1734.csv', header=F, stringsAsFactors = F)[-1,]
colnames(s1312)<-c('obs','Symbol')
colnames(s1323)<-c('obs','Symbol')
colnames(s1334)<-c('obs','Symbol')
colnames(s1341)<-c('obs','Symbol')
colnames(s1412)<-c('obs','Symbol')
colnames(s1423)<-c('obs','Symbol')
colnames(s1434)<-c('obs','Symbol')
colnames(s1441)<-c('obs','Symbol')
colnames(s1512)<-c('obs','Symbol')
colnames(s1523)<-c('obs','Symbol')
colnames(s1534)<-c('obs','Symbol')
colnames(s1541)<-c('obs','Symbol')
colnames(s1612)<-c('obs','Symbol')
colnames(s1623)<-c('obs','Symbol')
colnames(s1634)<-c('obs','Symbol')
colnames(s1641)<-c('obs','Symbol')
colnames(s1712)<-c('obs','Symbol')
colnames(s1723)<-c('obs','Symbol')
colnames(s1734)<-c('obs','Symbol')

stock<-read.csv('C:/Users/HDH/Desktop/mirae/data/stockprice.csv', stringsAsFactor=F)
stock[,-(1:6)]<-sapply(stock[,-(1:6)], function(x) gsub('[$,]','',x))
stock[,-(1:6)]<-sapply(stock[,-(1:6)], as.numeric)

length(names(stock))

head(names(stock))
stock=stock[,c(1:3,1097:length(names(stock)))]



##==========================================================================

market1312<-merge(s1312, stock[,c(1,233,243,253,263)], key='Symbol')
market1312$benefit_10<-(market1312$X2013.08.25-market1312$X2013.08.15)/(market1312$X2013.08.15)*100
market1312$benefit_20<-(market1312$X2013.09.04-market1312$X2013.08.15)/(market1312$X2013.08.15)*100
market1312$benefit_30<-(market1312$X2013.09.14-market1312$X2013.08.15)/(market1312$X2013.08.15)*100
benefit1312<-apply(market1312[,7:9], 2, mean)

market1412<-merge(s1412, stock[,c(1,598,608,618,628)], key='Symbol')
market1412$benefit_10<-(market1412$X2014.08.25-market1412$X2014.08.15)/(market1412$X2014.08.15)*100
market1412$benefit_20<-(market1412$X2014.09.04-market1412$X2014.08.15)/(market1412$X2014.08.15)*100
market1412$benefit_30<-(market1412$X2014.09.14-market1412$X2014.08.15)/(market1412$X2014.08.15)*100
benefit1412<-apply(market1412[,7:9], 2, mean)

market1512<-merge(s1512, stock[,c(1,963,973,983,993)], by='Symbol')
market1512$benefit_10<-(market1512$X2015.08.25-market1512$X2015.08.15)/(market1512$X2015.08.15)*100
market1512$benefit_20<-(market1512$X2015.09.04-market1512$X2015.08.15)/(market1512$X2015.08.15)*100
market1512$benefit_30<-(market1512$X2015.09.14-market1512$X2015.08.15)/(market1512$X2015.08.15)*100
benefit1512<-apply(market1512[,7:9], 2, mean)

market1612<-merge(s1612, stock[,c(1,1329,1339,1349,1359)], by='Symbol')
market1612$benefit_10<-(market1612$X2016.08.25-market1612$X2016.08.15)/(market1612$X2016.08.15)*100
market1612$benefit_20<-(market1612$X2016.09.04-market1612$X2016.08.15)/(market1612$X2016.08.15)*100
market1612$benefit_30<-(market1612$X2016.09.14-market1612$X2016.08.15)/(market1612$X2016.08.15)*100
benefit1612<-apply(market1612[,7:9], 2, mean)

market1712<-merge(s1712, stock[,c(1,1694,1704,1714,1724)], by='Symbol')
market1712$benefit_10<-(market1712$X2017.08.25-market1712$X2017.08.15)/(market1712$X2017.08.15)*100
market1712$benefit_20<-(market1712$X2017.09.04-market1712$X2017.08.15)/(market1712$X2017.08.15)*100
market1712$benefit_30<-(market1712$X2017.09.14-market1712$X2017.08.15)/(market1712$X2017.08.15)*100
benefit1712<-apply(market1712[,7:9], 2, mean)

#--------------------------------------------------------------------------------------

market1323<-merge(s1323, stock[,c(1,325,335,345,355)], key='Symbol')
market1323$benefit_10<-(market1323$X2013.11.25-market1323$X2013.11.15)/(market1323$X2013.11.15)*100
market1323$benefit_20<-(market1323$X2013.12.05-market1323$X2013.11.15)/(market1323$X2013.11.15)*100
market1323$benefit_30<-(market1323$X2013.12.15-market1323$X2013.11.15)/(market1323$X2013.11.15)*100
benefit1323<-apply(market1323[,7:9], 2, mean)

market1423<-merge(s1423, stock[,c(1,690,700,710,720)], key='Symbol')
market1423$benefit_10<-(market1423$X2014.11.25-market1423$X2014.11.15)/(market1423$X2014.11.15)*100
market1423$benefit_20<-(market1423$X2014.12.05-market1423$X2014.11.15)/(market1423$X2014.11.15)*100
market1423$benefit_30<-(market1423$X2014.12.15-market1423$X2014.11.15)/(market1423$X2014.11.15)*100
benefit1423<-apply(market1423[,7:9], 2, mean)

market1523<-merge(s1523, stock[,c(1,1055,1065,1075,1085)], by='Symbol')
market1523$benefit_10<-(market1523$X2015.11.25-market1523$X2015.11.15)/(market1523$X2015.11.15)*100
market1523$benefit_20<-(market1523$X2015.12.05-market1523$X2015.11.15)/(market1523$X2015.11.15)*100
market1523$benefit_30<-(market1523$X2015.12.15-market1523$X2015.11.15)/(market1523$X2015.11.15)*100
benefit1523<-apply(market1523[,7:9], 2, mean)

market1623<-merge(s1623, stock[,c(1,1421,1431,1441,1451)], by='Symbol')
market1623$benefit_10<-(market1623$X2016.11.25-market1623$X2016.11.15)/(market1623$X2016.11.15)*100
market1623$benefit_20<-(market1623$X2016.12.05-market1623$X2016.11.15)/(market1623$X2016.11.15)*100
market1623$benefit_30<-(market1623$X2016.12.15-market1623$X2016.11.15)/(market1623$X2016.11.15)*100
benefit1623<-apply(market1623[,7:9], 2, mean)

market1723<-merge(s1723, stock[,c(1,1786,1796,1806,1816)], by='Symbol')
market1723$benefit_10<-(market1723$X2017.11.25-market1723$X2017.11.15)/(market1723$X2017.11.15)*100
market1723$benefit_20<-(market1723$X2017.12.05-market1723$X2017.11.15)/(market1723$X2017.11.15)*100
market1723$benefit_30<-(market1723$X2017.12.15-market1723$X2017.11.15)/(market1723$X2017.11.15)*100
benefit1723<-apply(market1723[,7:9], 2, mean)

#--------------------------------------------------------------------------------------

market1334<-merge(s1334, stock[,c(1,460,470,480,490)], key='Symbol')
market1334$benefit_10<-(market1334$X2014.04.09-market1334$X2014.03.30)/(market1334$X2014.03.30)*100
market1334$benefit_20<-(market1334$X2014.04.19-market1334$X2014.03.30)/(market1334$X2014.03.30)*100
market1334$benefit_30<-(market1334$X2014.04.29-market1334$X2014.03.30)/(market1334$X2014.03.30)*100
benefit1334<-apply(market1334[,7:9], 2, mean)

market1434<-merge(s1434, stock[,c(1,825,835,845,855)], key='Symbol')
market1434$benefit_10<-(market1434$X2015.04.09-market1434$X2015.03.30)/(market1434$X2015.03.30)*100
market1434$benefit_20<-(market1434$X2015.04.19-market1434$X2015.03.30)/(market1434$X2015.03.30)*100
market1434$benefit_30<-(market1434$X2015.04.29-market1434$X2015.03.30)/(market1434$X2015.03.30)*100
benefit1434<-apply(market1434[,7:9], 2, mean)

market1534<-merge(s1534, stock[,c(1,1191,1201,1211,1221)], by='Symbol')
market1534$benefit_10<-(market1534$X2016.04.09-market1534$X2016.03.30)/(market1534$X2016.03.30)*100
market1534$benefit_20<-(market1534$X2016.04.19-market1534$X2016.03.30)/(market1534$X2016.03.30)*100
market1534$benefit_30<-(market1534$X2016.04.29-market1534$X2016.03.30)/(market1534$X2016.03.30)*100
benefit1534<-apply(market1534[,7:9], 2, mean)

market1634<-merge(s1634, stock[,c(1,1556,1566,1576,1586)], by='Symbol')
market1634$benefit_10<-(market1634$X2017.04.09-market1634$X2017.03.30)/(market1634$X2017.03.30)*100
market1634$benefit_20<-(market1634$X2017.04.19-market1634$X2017.03.30)/(market1634$X2017.03.30)*100
market1634$benefit_30<-(market1634$X2017.04.29-market1634$X2017.03.30)/(market1634$X2017.03.30)*100
benefit1634<-apply(market1634[,7:9], 2, mean)

market1734<-merge(s1734, stock[,c(1,1921,1931,1941,1951)], by='Symbol')
market1734$benefit_10<-(market1734$X2018.04.09-market1734$X2018.03.30)/(market1734$X2018.03.30)*100
market1734$benefit_20<-(market1734$X2018.04.19-market1734$X2018.03.30)/(market1734$X2018.03.30)*100
market1734$benefit_30<-(market1734$X2018.04.29-market1734$X2018.03.30)/(market1734$X2018.03.30)*100
benefit1734<-apply(market1734[,7:9], 2, mean)

#--------------------------------------------------------------------------------------

market1341<-merge(s1341, stock[,c(1,506,516,526,536)], key='Symbol')
market1341$benefit_10<-(market1341$X2014.05.25-market1341$X2014.05.15)/(market1341$X2014.05.15)*100
market1341$benefit_20<-(market1341$X2014.06.04-market1341$X2014.05.15)/(market1341$X2014.05.15)*100
market1341$benefit_30<-(market1341$X2014.06.14-market1341$X2014.05.15)/(market1341$X2014.05.15)*100
benefit1341<-apply(market1341[,7:9], 2, mean)

market1441<-merge(s1441, stock[,c(1,871,881,891,901)], key='Symbol')
market1441$benefit_10<-(market1441$X2015.05.25-market1441$X2015.05.15)/(market1441$X2015.05.15)*100
market1441$benefit_20<-(market1441$X2015.06.04-market1441$X2015.05.15)/(market1441$X2015.05.15)*100
market1441$benefit_30<-(market1441$X2015.06.14-market1441$X2015.05.15)/(market1441$X2015.05.15)*100
benefit1441<-apply(market1441[,7:9], 2, mean)

market1541<-merge(s1541, stock[,c(1,1237,1247,1257,1267)], by='Symbol')
market1541$benefit_10<-(market1541$X2016.05.25-market1541$X2016.05.15)/(market1541$X2016.05.15)*100
market1541$benefit_20<-(market1541$X2016.06.04-market1541$X2016.05.15)/(market1541$X2016.05.15)*100
market1541$benefit_30<-(market1541$X2016.06.14-market1541$X2016.05.15)/(market1541$X2016.05.15)*100
benefit1541<-apply(market1541[,7:9], 2, mean)

market1641<-merge(s1641, stock[,c(1,1602,1612,1622,1632)], by='Symbol')
market1641$benefit_10<-(market1641$X2017.05.25-market1641$X2017.05.15)/(market1641$X2017.05.15)*100
market1641$benefit_20<-(market1641$X2017.06.04-market1641$X2017.05.15)/(market1641$X2017.05.15)*100
market1641$benefit_30<-(market1641$X2017.06.14-market1641$X2017.05.15)/(market1641$X2017.05.15)*100
benefit1641<-apply(market1641[,7:9], 2, mean)


## 2분기

for (i in 1:3){
  total1312[,i+1]<-total1312[ ,i+1]-benefit1312[i]
}

for (i in 1:3){
  total1412[,i+1]<-total1412[ ,i+1]-benefit1412[i]
}

for (i in 1:3){
  total1512[,i+1]<-total1512[ ,i+1]-benefit1512[i]
}

for (i in 1:3){
  total1612[,i+1]<-total1612[ ,i+1]-benefit1612[i]
}

for (i in 1:3){
  total1712[,i+1]<-total1712[ ,i+1]-benefit1712[i]
}

total_Q22<-rbind(total1312, total1412, total1512, total1612, total1712)

total_Q22<-total_Q22 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(mean_10),
            mean_20=mean(mean_20),
            mean_30=mean(mean_30),
            freq=sum(freq)) %>% as.data.frame()

##------------------------------------------------------
## 3분기

for (i in 1:3){
  total1323[,i+1]<-total1323[ ,i+1]-benefit1323[i]
}

for (i in 1:3){
  total1423[,i+1]<-total1423[ ,i+1]-benefit1423[i]
}

for (i in 1:3){
  total1523[,i+1]<-total1523[ ,i+1]-benefit1523[i]
}

for (i in 1:3){
  total1623[,i+1]<-total1623[ ,i+1]-benefit1623[i]
}

for (i in 1:3){
  total1723[,i+1]<-total1723[ ,i+1]-benefit1723[i]
}

total_Q33<-rbind(total1323, total1423, total1523, total1623, total1723)

total_Q33<-total_Q33 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(mean_10),
            mean_20=mean(mean_20),
            mean_30=mean(mean_30),
            freq=sum(freq)) %>% as.data.frame()

#-------------------------------------------------------
## 4분기

for (i in 1:3){
  total1334[,i+1]<-total1334[ ,i+1]-benefit1334[i]
}

for (i in 1:3){
  total1434[,i+1]<-total1434[ ,i+1]-benefit1434[i]
}

for (i in 1:3){
  total1534[,i+1]<-total1534[ ,i+1]-benefit1534[i]
}

for (i in 1:3){
  total1634[,i+1]<-total1634[ ,i+1]-benefit1634[i]
}

for (i in 1:3){
  total1734[,i+1]<-total1734[ ,i+1]-benefit1734[i]
}

total_Q44<-rbind(total1334, total1434, total1534, total1634, total1734)

total_Q44<-total_Q44 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(mean_10),
            mean_20=mean(mean_20),
            mean_30=mean(mean_30),
            freq=sum(freq)) %>% as.data.frame()

#-------------------------------------------------------
## 1분기

for (i in 1:3){
  total1341[,i+1]<-total1341[ ,i+1]-benefit1341[i]
}

for (i in 1:3){
  total1441[,i+1]<-total1441[ ,i+1]-benefit1441[i]
}

for (i in 1:3){
  total1541[,i+1]<-total1541[ ,i+1]-benefit1541[i]
}

for (i in 1:3){
  total1641[,i+1]<-total1641[ ,i+1]-benefit1641[i]
}

total_Q11<-rbind(total1341, total1441, total1541, total1641)

total_Q11<-total_Q11 %>% group_by(from_to) %>% 
  summarize(mean_10=mean(mean_10),
            mean_20=mean(mean_20),
            mean_30=mean(mean_30),
            freq=sum(freq)) %>% as.data.frame()


move1741<-read.csv('C:/Users/HDH/Desktop/mirae/data/move/move1741.csv')
move1741$from_to<-paste(move1741$cluster1, move1741$cluster2, sep='-')
test1741<-merge(move1741[,-1], stock[,c(1,1967,1977,1987,1997)], by='Symbol')
test1741$benefit_10<-(test1741$X2018.05.25-test1741$X2018.05.15)/(test1741$X2018.05.15)*100
test1741$benefit_20<-(test1741$X2018.06.04-test1741$X2018.05.15)/(test1741$X2018.05.15)*100
test1741$benefit_30<-(test1741$X2018.06.14-test1741$X2018.05.15)/(test1741$X2018.05.15)*100

total1741<-test1741 %>% 
  summarize(mean_10=mean(benefit_10),
            mean_20=mean(benefit_20),
            mean_30=mean(benefit_30))  %>% as.data.frame()
table1741<-as.data.frame(table(move1741[,5]))
colnames(table1741)<-c('from_to', 'freq')
total1741<-merge(total1741, table1741)


s1741<-read.csv('C:/Users/HDH/Desktop/mirae/data/move/s1741.csv', header=F, stringsAsFactors = F)[-1,]
colnames(s1741)<-c('obs','Symbol')

market1741<-merge(s1741, stock[,c(1,1967,1977,1987,1997)], by='Symbol')
market1741$benefit_10<-(market1741$X2018.05.25-market1741$X2018.05.15)/(market1741$X2018.05.15)*100
market1741$benefit_20<-(market1741$X2018.06.04-market1741$X2018.05.15)/(market1741$X2018.05.15)*100
market1741$benefit_30<-(market1741$X2018.06.14-market1741$X2018.05.15)/(market1741$X2018.05.15)*100
benefit1741<-apply(market1741[,7:9], 2, mean)

total1741[,1:3]-benefit1741

total_Q11sd<-rbind(total1341, total1441, total1541, total1641)

total_Q11sd<-total_Q11sd %>% group_by(from_to) %>% 
  summarize(sd_10=sd(mean_10),
            sd_20=sd(mean_20),
            sd_30=sd(mean_30),
            freq=sum(freq)) %>% as.data.frame()


total_Q22sd<-rbind(total1312, total1412, total1512, total1612, total1712)

total_Q22sd<-total_Q22sd %>% group_by(from_to) %>% 
  summarize(sd_10=sd(mean_10),
            sd_20=sd(mean_20),
            sd_30=sd(mean_30),
            freq=sum(freq)) %>% as.data.frame()


total_Q33sd<-rbind(total1323, total1423, total1523, total1623, total1723)

total_Q33sd<-total_Q33sd %>% group_by(from_to) %>% 
  summarize(sd_10=sd(mean_10),
            sd_20=sd(mean_20),
            sd_30=sd(mean_30),
            freq=sum(freq)) %>% as.data.frame()


total_Q44sd<-rbind(total1334, total1434, total1534, total1634, total1734)

total_Q44sd<-total_Q44sd %>% group_by(from_to) %>% 
  summarize(sd_10=sd(mean_10),
            sd_20=sd(mean_20),
            sd_30=sd(mean_30),
            freq=sum(freq)) %>% as.data.frame()
total_Q11

total_Q11sd








