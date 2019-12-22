# This script is for the categorization of the traders.
## tradebook data used

# Clear your workspace by removing all objects returned by ls():
rm(list = ls()) 

getwd()
setwd("C:/Users/CEYDA/Desktop/emprical_research")

Sys.timezone() 

# install.packages("chron")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("chron")
# install.packages("strptime")


library(chron)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

# You already save them as RData file.

#alldata_1 <- read.csv("data/alldata_1.csv", sep=",",stringsAsFactor=FALSE,header = T)
#alldata_2<- read.csv("data/alldata_2.csv", sep=",",stringsAsFactor=FALSE,header = T)

#save(alldata_1, file="data/alldata_1.RData")
#save(alldata_2, file="data/alldata_2.RData")

load("~/Desktop/emprical_research/data/tradebook/alldata_1.RData")
#load("~/Desktop/emprical_research/data/tradebook/alldata_2.RData")


# First start with only AKBNK.E, for now only first week.
 
akbank <- alldata_1[alldata_1$INSTRUMENTCODE=="AKBNK.E",]
#akbank_2 <- alldata_2[alldata_2$INSTRUMENTCODE=="AKBNK.E",]

#akbank <- rbind(akbank_1, akbank_2)

# you dropped the unneceserray columns for now
 akbank <- akbank[-c(1,14,16,19,20,22,23,25,26,27,28)]

#saving the variables in correct form

 akbank$GENERICDESC <- as.factor(akbank$GENERICDESC)
 #akbank$TRADEDATE <- as.factor(akbank$TRADEDATE) 
 akbank$INSTRUMENTCODE <- as.factor(akbank$INSTRUMENTCODE)
 akbank$MATCHTRADEGROUPID <- as.factor(akbank$MATCHTRADEGROUPID)
 akbank$TRADEID <- as.factor(akbank$TRADEID)
 akbank$BUYSELL <- as.factor(akbank$BUYSELL)
 akbank$ACCOUNTTYPE <- as.factor(akbank$ACCOUNTTYPE)
 akbank$ACCOUNTNO <- as.factor(akbank$ACCOUNTNO)
 akbank$AFK <- as.factor(akbank$AFK)
 akbank$ORDERID <- as.factor(akbank$ORDERID)
 akbank$USERNAME<- as.factor(akbank$USERNAME)
 akbank$SESSIONNAME<- as.factor(akbank$SESS)
 akbank$PASSIVEAGGRESSIVE<- as.factor(akbank$PASSIVEAGGRESSIVE)


# For having seperate users 

akbank <- mutate(akbank, useraccount = paste(GENERICDESC,"_",ACCOUNTNO))

akbank$useraccount <- as.factor(akbank$useraccount)

# the idea here is just pooling TW users in TW, FIX users in FIX,
# but take each HFT and Ouch users seperately.


akbank$User <- ifelse(grepl("HFT", akbank$USERNAME, ignore.case = T), paste(akbank$useraccount,"_",akbank$USERNAME), 
                       ifelse(grepl("EH", akbank$USERNAME, ignore.case = T), paste(akbank$useraccount,"_",akbank$USERNAME),
                              ifelse(grepl("COLO", akbank$USERNAME, ignore.case = T), paste(akbank$useraccount,"_COLO"),
                                     ifelse(grepl("YKREF", akbank$USERNAME, ignore.case = T), paste(akbank$useraccount,"_",akbank$USERNAME), 
                                            ifelse(grepl("FIX\\d", akbank$USERNAME, ignore.case = T), paste(akbank$useraccount,"_FIX"), paste(akbank$useraccount,"_TW"))))))

akbank$account <- ifelse(grepl("HFT", akbank$User, ignore.case = T), paste(akbank$useraccount,"_",akbank$USERNAME),
                          ifelse(grepl("FIX", akbank$User, ignore.case = T), paste(akbank$useraccount,"_OTHER"),
                           ifelse(grepl("TW", akbank$User, ignore.case = T), paste(akbank$useraccount,"_OTHER"),
                           ifelse(grepl("COLO", akbank$User, ignore.case = T),paste(akbank$useraccount,"_OTHER"), paste(akbank$useraccount,"_",akbank$USERNAME)))))


akbank$User <- as.factor(akbank$User)
akbank$account <- as.factor(akbank$account)

akbank18 <- akbank[akbank$TRADEDATE=="2019-03-18",]
akbank19 <- akbank[akbank$TRADEDATE=="2019-03-19",]
akbank20 <- akbank[akbank$TRADEDATE=="2019-03-20",]
akbank21 <- akbank[akbank$TRADEDATE=="2019-03-21",]
akbank22 <- akbank[akbank$TRADEDATE=="2019-03-22",]

avgmarket <- (sum(akbank18$QUANTITY) + sum(akbank19$QUANTITY) + sum(akbank20$QUANTITY) + sum(akbank21$QUANTITY) + sum(akbank22$QUANTITY))/5

#trade volume for each trading account
trade_quantity <- tapply(akbank$QUANTITY, list(akbank$account, akbank$TRADEDATE), FUN=sum)
trade_quantity <- as.data.frame(trade_quantity)
trade_quantity[is.na(trade_quantity)] <- 0
colnames(trade_quantity)

#end of day net position for each trading account
buy_position <- tapply(akbank[akbank$BUYSELL=="A",]$QUANTITY, list(akbank[akbank$BUYSELL=="A",]$account, akbank[akbank$BUYSELL=="A",]$TRADEDATE), FUN=sum)
sell_position <- tapply(akbank[akbank$BUYSELL=="S",]$QUANTITY, list(akbank[akbank$BUYSELL=="S",]$account, akbank[akbank$BUYSELL=="S",]$TRADEDATE), FUN=sum)

buy_position[is.na(buy_position)] <- 0
sell_position[is.na(sell_position)] <- 0

net_position <- buy_position - sell_position
net_position[is.na(net_position)] <- 0
colnames(net_position)

trade_stats <- cbind(trade_quantity,  net_position)
trade_stats[is.na(trade_stats)] <- 0

colnames(trade_stats) <- c("tq_18","tq_19","tq_20","tq_21","tq_22","np_18","np_19","np_20","np_21","np_22")

str(trade_stats)

trade_stats$avgtq <- (trade_stats$tq_18 + trade_stats$tq_19 + trade_stats$tq_20 + trade_stats$tq_21 + trade_stats$tq_22)/5
trade_stats$avgnp <- (trade_stats$np_18 + trade_stats$np_19 + trade_stats$np_20 + trade_stats$np_21 + trade_stats$np_22)/5

## CATEGORIZATION ##

# march 18

trade_stats$xaxis18<- trade_stats$np_18/ trade_stats$tq_18
trade_stats$yaxis18 <- trade_stats$tq_18/sum(akbank18$QUANTITY)


trade_stats$category18 = ifelse(trade_stats$xaxis18==1 & trade_stats$yaxis18 > 0.001, "BB",
                                ifelse(trade_stats$xaxis18==-1 & trade_stats$yaxis18 > 0.001, "BS",
                                       ifelse(trade_stats$xaxis18==1 & trade_stats$yaxis18 <= 0.001, "SB",
                                              ifelse(trade_stats$xaxis18==-1 & trade_stats$yaxis18 <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$xaxis18)<=0.25 & trade_stats$yaxis18> 0.001, "BN",
                                                            ifelse(abs(trade_stats$xaxis18)<=0.25 & trade_stats$yaxis18<= 0.001,"SN",
                                                                   ifelse(trade_stats$yaxis18> 0.001,"BI", "SI")))))))


trade_stats$category18 <- as.factor(trade_stats$category18)

trade_stats$category18 <- addNA(trade_stats$category18)
table(trade_stats$category18)

# for your usual visualization for March 18 ###

march18 <- ggplot(data=trade_stats, aes(x=xaxis18, y=yaxis18, color=category18)) +
         geom_point(size = 1, alpha = 0.75) +
         xlab("Net Position Scaled by Trader Volume") +
         ylab("Trader Volume Scaled by Market Trading Volume") +
        ggtitle("AKBANK.E - March 18th, 2019") +
        theme(plot.title = element_text(colour="mediumvioletred", size=15, hjust=0.5),
              axis.title.x = element_text(colour="darkslateblue", size=10),
              axis.title.y=element_text(colour="mediumspringgreen", size=10))
march18 <- march18 + scale_colour_manual(values = c("red", "darkorchid4", "yellow","mediumspringgreen", "violetred1", "black", "seagreen", "coral"))

ggsave(march18, file="output/figures/categorization/march18.png",width=13.66,height=7.05,limitsize = FALSE)


march18


# continue to categorization - March 19

trade_stats$xaxis19<- trade_stats$np_19/ trade_stats$tq_19
trade_stats$yaxis19 <- trade_stats$tq_19/sum(akbank19$QUANTITY)

trade_stats$category19 = ifelse(trade_stats$xaxis19==1 & trade_stats$yaxis19 > 0.001, "BB",
                                ifelse(trade_stats$xaxis19==-1 & trade_stats$yaxis19 > 0.001, "BS",
                                       ifelse(trade_stats$xaxis19==1 & trade_stats$yaxis19 <= 0.001, "SB",
                                              ifelse(trade_stats$xaxis19==-1 & trade_stats$yaxis19 <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$xaxis19)<=0.25 & trade_stats$yaxis19> 0.001, "BN",
                                                            ifelse(abs(trade_stats$xaxis19)<=0.25 & trade_stats$yaxis19<= 0.001,"SN",
                                                                   ifelse(trade_stats$yaxis19> 0.001,"BI", "SI")))))))


trade_stats$category19 <- as.factor(trade_stats$category19)

trade_stats$category19 <- addNA(trade_stats$category19)
table(trade_stats$category19)

# March 20

trade_stats$xaxis20<- trade_stats$np_20/ trade_stats$tq_20
trade_stats$yaxis20 <- trade_stats$tq_20/sum(akbank20$QUANTITY)

trade_stats$category20 = ifelse(trade_stats$xaxis20==1 & trade_stats$yaxis20 > 0.001, "BB",
                                ifelse(trade_stats$xaxis20==-1 & trade_stats$yaxis20 > 0.001, "BS",
                                       ifelse(trade_stats$xaxis20==1 & trade_stats$yaxis20<= 0.001, "SB",
                                              ifelse(trade_stats$xaxis20==-1 & trade_stats$yaxis20 <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$xaxis20)<=0.25 & trade_stats$yaxis20> 0.001, "BN",
                                                            ifelse(abs(trade_stats$xaxis20)<=0.25 & trade_stats$yaxis20<= 0.001,"SN",
                                                                   ifelse(trade_stats$yaxis20> 0.001,"BI", "SI")))))))


trade_stats$category20 <- as.factor(trade_stats$category20)

trade_stats$category20 <- addNA(trade_stats$category20)
table(trade_stats$category20)

# March 21

trade_stats$xaxis21<- trade_stats$np_21/ trade_stats$tq_21
trade_stats$yaxis21 <- trade_stats$tq_21/sum(akbank21$QUANTITY)

trade_stats$category21 = ifelse(trade_stats$xaxis21==1 & trade_stats$yaxis21 > 0.001, "BB",
                                ifelse(trade_stats$xaxis21==-1 & trade_stats$yaxis21 > 0.001, "BS",
                                       ifelse(trade_stats$xaxis21==1 & trade_stats$yaxis21<= 0.001, "SB",
                                              ifelse(trade_stats$xaxis21==-1 & trade_stats$yaxis21 <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$xaxis21)<=0.25 & trade_stats$yaxis21> 0.001, "BN",
                                                            ifelse(abs(trade_stats$xaxis21)<=0.25 & trade_stats$yaxis21<= 0.001,"SN",
                                                                   ifelse(trade_stats$yaxis21> 0.001,"BI", "SI")))))))


trade_stats$category21 <- as.factor(trade_stats$category21)

trade_stats$category21 <- addNA(trade_stats$category21)
table(trade_stats$category21)

# March 22

trade_stats$xaxis22<- trade_stats$np_22/ trade_stats$tq_22
trade_stats$yaxis22 <- trade_stats$tq_22/sum(akbank22$QUANTITY)

trade_stats$category22 = ifelse(trade_stats$xaxis22==1 & trade_stats$yaxis22 > 0.001, "BB",
                                ifelse(trade_stats$xaxis22==-1 & trade_stats$yaxis22 > 0.001, "BS",
                                       ifelse(trade_stats$xaxis22==1 & trade_stats$yaxis22<= 0.001, "SB",
                                              ifelse(trade_stats$xaxis22==-1 & trade_stats$yaxis22 <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$xaxis22)<=0.25 & trade_stats$yaxis22> 0.001, "BN",
                                                            ifelse(abs(trade_stats$xaxis22)<=0.25 & trade_stats$yaxis22<= 0.001,"SN",
                                                                   ifelse(trade_stats$yaxis22> 0.001,"BI", "SI")))))))


trade_stats$category22 <- as.factor(trade_stats$category22)

trade_stats$category22 <- addNA(trade_stats$category22)
table(trade_stats$category22)


### March 18-22 week average categorization ##


trade_stats$avgxaxis<- trade_stats$avgnp/ trade_stats$avgtq
trade_stats$avgyaxis <- trade_stats$avgtq/avgmarket

trade_stats$categoryavg = ifelse(trade_stats$avgxaxis==1 & trade_stats$avgyaxis > 0.001, "BB",
                                ifelse(trade_stats$avgxaxis==-1 & trade_stats$avgyaxis > 0.001, "BS",
                                       ifelse(trade_stats$avgxaxis==1 & trade_stats$avgyaxis<= 0.001, "SB",
                                              ifelse(trade_stats$avgxaxis==-1 & trade_stats$avgyaxis <= 0.001, "SS",
                                                     ifelse(abs(trade_stats$avgxaxis)<=0.25 & trade_stats$avgyaxis> 0.001, "BN",
                                                            ifelse(abs(trade_stats$avgxaxis)<=0.25 & trade_stats$avgyaxis<= 0.001,"SN",
                                                                   ifelse(trade_stats$avgyaxis> 0.001,"BI", "SI")))))))


trade_stats$categoryavg <- as.factor(trade_stats$categoryavg)

trade_stats$categoryavg <- addNA(trade_stats$categoryavg)
table(trade_stats$categoryavg)

#write.csv(trade_stats, "output/statistics/categorizationresults_AKBNK.csv", quote=F)

# Intertrade duration 

akbank <- akbank[-c(21)]

akbank$datetime <- chron(akbank$TRADEDATE, akbank$TRADETIME, format=c(dates="y-m-d",times="h:m:s"))

akbank$datetime <- as.POSIXct(akbank$datetime)

attr(akbank$datetime,"tzone")<-"GMT"

akbank <- mutate(akbank, tradestart = paste(TRADEDATE," ","09:55:00"))
akbank$tradestart  <- as.POSIXct(akbank$tradestart)

# take the time diference 

df <- akbank %>% group_by(TRADEDATE, account) %>% mutate(intertrade = ifelse(is.na(lag(datetime)), 
                                 difftime(datetime, tradestart, unit="sec"), difftime(datetime, lag(datetime), unit="sec")))
        
## to see the ifelse working correctly or not

dff <- df %>% group_by(TRADEDATE, account) %>% mutate(see = ifelse(is.na(lag(datetime)), 
                  paste0("datetime, tradestart"), paste("datetime, lag(datetime)")))
                                                                            
dff$intertrade <- as.numeric(dff$intertrade)

dff$intertrade[dff$see == "datetime, tradestart"] <- dff$intertrade[dff$see=="datetime, tradestart"]

df$intertrade[is.na(df$intertrade)] <- 0

average_duration <- aggregate(dff$intertrade, by = list(df$account), FUN=mean, na.rm = TRUE)

colnames(average_duration) <- c("account", "avgtd")
#average_duration <- average_duration %>% spread(tradedate, trade_duration)

# cleaned category table and then merge with durations

category_akbank <- trade_stats[-c(13,14,16,17,19,20,22,23,25,26,28,29)] 

trade_features <- cbind(average_duration, category_akbank)

rownames(trade_features) <- c()

colnames(trade_features) <- c("account", "avgtd", "tq_18","tq_19", "tq_20","tq_21","tq_22","np_18","np_19","np_20","np_21",
                              "np_22","avgtq", "avgnp","category18","category19","category20","category21","category22", "categoryavg")

# Trades per minute for each account - number of transactions for 420 minutes

tradepermin <- akbank %>% group_by(account) %>% summarise(avgtpm=mean(n()/420))

#tradepermin <- tradepermin %>% spread(TRADEDATE, count)

#tradepermin[is.na(tradepermin)] <- 0

trade_features <- merge(tradepermin, trade_features, by.x="account")

colnames(trade_features) <- c("account", "avgtpm","avgtd", "tq_18","tq_19", "tq_20","tq_21","tq_22","np_18","np_19","np_20","np_21",
                              "np_22","avgtq", "avgnp","category18","category19","category20","category21","category22", "categoryavg")

save(trade_features, file="data/tradefeatures.RData")

#write.csv(trade_features, "output/statistics/tradefeatures.csv", quote=F)

### ORDERBOOK ANALYSIS

load("~/Desktop/emprical_research/data/orderbook/week_1/akbank_1.RData")

akbank_1$GENERICDESC <- as.factor(akbank_1$GENERICDESC)
akbank_1$INSTRUMENTCODE <- as.factor(akbank_1$INSTRUMENTCODE)
akbank_1$BUYSELL <- as.factor(akbank_1$BUYSELL)
akbank_1$ACCOUNTTYPE <- as.factor(akbank_1$ACCOUNTTYPE)
akbank_1$ACCOUNTNO <- as.factor(akbank_1$ACCOUNTNO)
akbank_1$USERNAME<- as.factor(akbank_1$USERNAME)

# For having seperate users 

akbank_1 <- mutate(akbank_1, useraccount = paste(GENERICDESC,"_",ACCOUNTNO))

akbank_1$useraccount <- as.factor(akbank_1$useraccount)

# the idea here is just pooling TW users in TW, FIX users in FIX,
# but take each HFT and Ouch users seperately.


akbank_1$User <- ifelse(grepl("HFT", akbank_1$USERNAME, ignore.case = T), paste(akbank_1$useraccount,"_",akbank_1$USERNAME), 
                      ifelse(grepl("EH", akbank_1$USERNAME, ignore.case = T), paste(akbank_1$useraccount,"_",akbank_1$USERNAME),
                             ifelse(grepl("COLO", akbank_1$USERNAME, ignore.case = T), paste(akbank_1$useraccount,"_COLO"),
                                    ifelse(grepl("YKREF", akbank_1$USERNAME, ignore.case = T), paste(akbank_1$useraccount,"_",akbank_1$USERNAME), 
                                           ifelse(grepl("FIX\\d", akbank_1$USERNAME, ignore.case = T), paste(akbank_1$useraccount,"_FIX"), paste(akbank_1$useraccount,"_TW"))))))

akbank_1$account <- ifelse(grepl("HFT", akbank_1$User, ignore.case = T), paste(akbank_1$useraccount,"_",akbank_1$USERNAME),
                         ifelse(grepl("FIX", akbank_1$User, ignore.case = T), paste(akbank_1$useraccount,"_OTHER"),
                                ifelse(grepl("TW", akbank_1$User, ignore.case = T), paste(akbank_1$useraccount,"_OTHER"),
                                       ifelse(grepl("COLO", akbank_1$User, ignore.case = T),paste(akbank_1$useraccount,"_OTHER"), paste(akbank_1$useraccount,"_",akbank_1$USERNAME)))))


akbank_1$User <- as.factor(akbank_1$User)
akbank_1$account <- as.factor(akbank_1$account)

#order volume for each trading account
order_quantity <- tapply(akbank_1$QUANTITY, list(akbank_1$account, akbank_1$ORDERDATE), FUN=sum)
order_quantity[is.na(order_quantity)] <- 0
order_quantity <- as.data.frame(order_quantity)
colnames(order_quantity) <- c("oq18","oq19","oq20", "oq21","oq22")

order_quantity$account <- rownames(order_quantity)
rownames(order_quantity) <- c()

# order per minute 

orderpermin <- akbank_1 %>% group_by(ORDERDATE, account) %>% summarise(count = n()/420)

orderpermin <- orderpermin %>% group_by(account) %>% summarise(avgopm=mean(count))

#order duration

akbank_1_trade <- akbank_1[akbank_1$CHANGEREASON=="3",]

length(unique(akbank_1_trade$account))

akbank_1_trade$ORDERENTRYDATETIME <- ymd_hms(akbank_1_trade$ORDERENTRYDATETIME)
akbank_1_trade$ORDERUPDATEDATETIME <- ymd_hms(akbank_1_trade$ORDERUPDATEDATETIME)
akbank_1_trade$duration <- akbank_1_trade$ORDERUPDATEDATETIME - akbank_1_trade$ORDERENTRYDATETIME

akbank_1_trade$ORDERDATE <- as.factor(akbank_1_trade$ORDERDATE)

average_duration <-  akbank_1_trade %>% group_by(account) %>% summarise(mean(duration))

colnames(average_duration) <- c("account", "avgod")

orderstats_ <- merge(order_quantity, orderpermin)

# Order type share

akbank_1$ORDERPRICETYPEID <- as.factor(akbank_1$ORDERPRICETYPEID)

LOMO <- aggregate(akbank_1$QUANTITY,
                by = list(akbank_1$account, akbank_1$ORDERPRICETYPEID), FUN=sum)

colnames(LOMO) <- c("account","ordertype","ordervolume")

LOMO <- spread(LOMO, ordertype, ordervolume)

colnames(LOMO) <- c("account","limit","market","markettolimit", "other")

LOMO[is.na(LOMO)] <- 0

LOMO$totalvolume <- LOMO$limit + LOMO$market + LOMO$markettolimit + LOMO$other

lastLOMO <- LOMO %>% group_by(account) %>% transmute(LOshare = limit/totalvolume, MOshare=market/totalvolume)

# last order table

orderstats <- merge(average_duration, orderstats_, by="account",all.x=TRUE)
orderstats <- merge(lastLOMO, orderstats, by="account",all.x=TRUE)

#save(orderstats, file="data/orderstats.RData")

#write.csv(orderstats, "output/statistics/orderfeatures.csv", quote=F)

# there is 5866 in orderstats table, but 5865 in trading account table so see the difference below:

anti_join(orderstats, trade_features)
anti_join(trade_features, orderstats)

all_features <- left_join(trade_features, orderstats)
save(all_features, file="data/all_features.RData")

all_features$avgov <- (all_features$oq18 + all_features$oq19+ all_features$oq20+ all_features$oq21+all_features$oq22)/5 
all_features$avgov[is.na(all_features$avgov)] <- 0

#write.csv(all_features, "output/statistics/allfeatures.csv", quote=F)
#save(all_features, file="data/all_features.RData")

# count how many day they are BN or SN


all_features$connection <-ifelse(grepl("*_OTHER", all_features$account),"nonHFT", "HFT")

all_features$ifn <- apply(all_features[-c(21)][, grep("category", names(trade_features))], MARGIN = 1,
                          function(x) sum(x %in% c("BN","SN"), na.rm = T))

BNorSN <- subset(all_features, all_features$ifn>=3 & all_features$avgtpm >= median(all_features$avgtpm)
                 & all_features$avgtq >= median(all_features$avgtq)
                & all_features$avgov >= median(all_features$avgov))

tradebook_BNorSN <- akbank %>% filter(akbank$account %in% BNorSN$account)

# intraday mean net position for 1 minute -- Need to search how to test whether this pattern holds or not.

min_buy_position <- tradebook_BNorSN %>% filter(BUYSELL=="A") %>%
        mutate(datetime.bin = floor_date(datetime, unit = "1 minute")) %>%
        group_by(TRADEDATE, datetime.bin, account) %>% 
        summarise(buyposition = sum(QUANTITY)) 

min_sell_position <- tradebook_BNorSN %>% filter(BUYSELL=="S") %>%
        mutate(datetime.bin = floor_date(datetime, unit = "1 minute")) %>%
        group_by(TRADEDATE, datetime.bin, account) %>% 
        summarise(sellposition = sum(QUANTITY)) 

min_netposition <- merge(min_sell_position, min_buy_position, all=TRUE)

min_netposition$buyposition[is.na(min_netposition$buyposition)] <- 0
min_netposition$sellposition[is.na(min_netposition$sellposition)] <- 0

min_netposition$netposition <- min_netposition$buyposition - min_netposition$sellposition
colnames(min_netposition) <- c("tradedate", "datetime.bin", "account" ,"sellposition", "buyposition" ,"netposition" )

str(min_netposition)


# to see whether one minute net position holdings has mean reversion pattern.

trade_quantity <- aggregate(tradebook_BNorSN$QUANTITY, by = list(tradebook_BNorSN$TRADEDATE, tradebook_BNorSN$account), FUN=sum)
colnames(trade_quantity) <- c("tradedate","account","tradequantity")

##first convert net position table to 1 minute frequency:

# min_netposition$datetime.bin <- as.POSIXct(min_netposition$datetime.bin)

# first create one minute bin datetime list

datetime <- min_netposition$datetime.bin[1]
datetime_list <- c(datetime)

lastDatetime <- min_netposition$datetime.bin[nrow(min_netposition)]

while (datetime < lastDatetime){
        datetime <- datetime + 60
        datetime_list <- c(datetime_list,datetime)
}

datetime_list <- format(ymd_hms(datetime_list))

df_1 <- data.frame(datetimevar=datetime_list, stringsAsFactors = F)

colnames(df_1) <- c("datetime.bin")

# str(df_1$datetime.bin)

##### make expansion of every account with every datetime bin, vals is the matrix for accounts with 
# all possible dtetime bins

vals <- expand.grid(account = unique(min_netposition$account),
                    datetime.bin = unique(df_1$datetime.bin))

vals$try <- vals$datetime.bin
vals$try <- as.character(vals$try)

newvals<- separate(vals, col = try, into  = c('Date', 'Time'), sep = ' ') 

newvals$Time <- as.POSIXct(newvals$Time, format="%H:%M:%S", tz="GMT")

## filter morning session ---> 09:55-13:00
newvalsMorning <- newvals %>%  group_by(Date) %>% filter(format(Time, "%H:%M:$S") < "13:00:00")
newvalsMorning <- newvalsMorning %>%  group_by(Date) %>% filter(format(Time, "%H:%M:$S") > "09:54:00")

## filter afternoon session ---> 14:00-18:10
newvalsAfternoon <- newvals %>%  group_by(Date) %>% filter(format(Time, "%H:%M:$S") > "13:59:00")
newvalsAfternoon <- newvalsAfternoon %>%  group_by(Date) %>% filter(format(Time, "%H:%M:$S") < "18:11:00")

contrade <- rbind(newvalsMorning, newvalsAfternoon)
contrade$datetime.bin <- as.POSIXct(contrade$datetime.bin, tz="GMT")

netposmatrix <- left_join(contrade, min_netposition)
netposmatrix <- netposmatrix[-c(4,5)]
netposmatrix[is.na(netposmatrix)] <- 0

netposmatrixx <- netposmatrix %>% group_by(Date, account) %>% 
        mutate(cumbuy = cumsum(buyposition), cumsell = cumsum(sellposition)) 

netposmatrixx$cumnet <- netposmatrixx$cumbuy - netposmatrixx$cumsell

netposmatrixx <- netposmatrixx[-c(4,5,6)]

colnames(netposmatrixx) <- c("account","datetime.bin","tradedate","Cumbuy", "Cumsell","Cumnet" )

minute_position <- merge(netposmatrixx, trade_quantity, by = c("tradedate", "account"))

mean_rev <- function(P,d){
        res <- 0
        for(i in d) {
                temp <- 0
                subP <- subset(P, tradedate==i)
                n <- nrow(subP)
                EP <- tail(subP, 1)[1,6]
                #cat('day:', i, ' EP:', EP, '\n')
                for(t in 1:n) {
                        #cat(' temp:', temp, ' t:', t, ' subP[t,6]:', subP[t,6], ' subP[t,7]:', subP[t,7], '\n')
                        temp <- temp + ((subP[t,6] - EP) / subP[t,7])^2
                }
                temp <- (temp / n) ^ (1/2)
                res <- res + temp
        }
        return(res / length(d))
}

mean_all <- function(dfParam) {
        uniq_account = unique(dfParam$account)
        resultDf <- data.frame()
        for (x in uniq_account) {
                P <- subset(dfParam, account==x)
                d <- unique(P$tradedate)
                means <- mean_rev(P,d)
                resultDf <- rbind(resultDf, data.frame(x, means))
                cat('account:',x, ' - mean:', means, ' d:', d,'\n')
        }
        return (resultDf)
}

qwe <- mean_all(minute_position)







#### here below is the ÇÖPLÜK

# Finding the subset of LP candidates

# BNORSN <- trade_features %>% filter(category18=="BN" | category18=="SN"  & category19=="BN" | category19=="SN" &
#                                             category20=="BN" | category20=="SN" & category21=="BN" | category21=="SN" 
#                                     & category22=="BN" | category22=="SN")

# BNORSN <- trade_features %>% filter(category18=="BN" | category19=="BN" | 
#                                             category20=="BN" |  category21=="BN" |  
#                                      category22=="BN" )


# BNORSN$BNSN <- apply(BNORSN[c(24:28)], 1, function(x) count(is.na))
# 
# new_BNORSN <- subset(BNORSN, BNORSN$na_count<="1")
# new_BNORSN$na_count <- as.factor(new_BNORSN$na_count)

#ADF test
# install.packages("urca")
# library(urca)
# x=ur.df(min_netpos18$netposition, type="trend", selectlags = "AIC" )
# x
# summary(x)
# ##
# 
# ## Intrdaday position graphs:
# 
# uniq_account = unique(min_netposition$account)
# min_netposition$TRADEDATE <- factor(min_netposition$TRADEDATE)
# 
# for (i in uniq_account) { 
#         
#         NP <- ggplot(subset(min_netposition, uniq_account==i),
#                      aes(x= datetime.bin, y=netposition, colour=TRADEDATE)) +
#                 geom_line() +
#                 geom_hline(aes(yintercept = mean(netposition)), color="black")+
#                 xlab("Trade Time") + ylab("Intraday position change") +
#                 ggtitle(i, "Intraday position change - 1 min") +
#                 theme(plot.title = element_text(colour="mediumvioletred", size=15, hjust=0.5),
#                       axis.title.x = element_text(colour="darkslateblue", size=15),
#                       axis.title.y=element_text(colour="mediumspringgreen", size=15))
#         NP<- NP + scale_colour_manual(values = c("darkorchid4", "yellow","mediumspringgreen", "violetred1", "red"))
#         
#         ggsave(NP, file=paste0("plot_",i,".png"), path="output/figures/categorization/intraday_LPs",width=13.66,height=7.05,limitsize = FALSE)
#         
# }
# 
# netpos_1min<- aggregate(min_netposition$netposition,
#                             by = list(min_netposition$TRADEDATE, min_netposition$account), FUN=mean)
# 
# colnames(netpos_1min) <- c("tradedate","account", "mean_netposition")
# 
# netpos_1min_mean <- aggregate(netpos_1min$mean_netposition,
#                 by = list(netpos_1min$account), FUN=mean)
# 
# colnames(netpos_1min_mean) <- c("account", "mean_daily_mean")
# 
# 
# 
# 
# 
# 
# # 
# # # EACH DAY avg qauntity
# # 
# # trade_quantity <- akbank %>%  group_by(TRADEDATE, account) %>%  summarise(totalq=sum(QUANTITY)) 
# # tq18 <- trade_quantity[trade_quantity$TRADEDATE=="2019-03-18",]
# # 
# # march18 <- ggplot(data=tq18, aes(x=account, y=totalq)) + geom_point(size = 1, alpha = 0.75)
# # march18 
# # hist(tq18$totalq)
# # mean(tq18$totalq)
# # 
# # 
# # 
# # 
# # 
# # #Tomy suggestion - make the yaxis as logs and the plot - see the clusters
# # 
# # # trade_stats$logyaxis18<- log(trade_stats$tq_18/sum(akbank18$QUANTITY))
# # # 
# # # march18_log <- ggplot(data=trade_stats, aes(x=xaxis18, y=logyaxis18)) + geom_point(size = 1, alpha = 0.75) +
# # #         xlab("Net Position Scaled by Trader Volume") + ylab("Trader Volume Scaled by Market Trading Volume") +
# # #         ggtitle("AKBANK.E - March 18th, 2019") +
# # #         theme(plot.title = element_text(colour="mediumvioletred", size=15, hjust=0.5),
# # #               axis.title.x = element_text(colour="darkslateblue", size=10),
# # #               axis.title.y=element_text(colour="mediumspringgreen", size=10))
# # # 
# # # 
# # # march18_log
# # # ggsave(march18_log, file="output/figures/categorization/logversion_march18.png",width=13.66,height=7.05,limitsize = FALSE)
