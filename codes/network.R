### Network map

# very useful notes for network map

# http://www.kateto.net/wp-content/uploads/2019/06/Sunbelt%202019%20R%20Network%20Visualization%20Workshop.pdf


# Clear your workspace by removing all objects returned by ls():


rm(list = ls()) 

getwd()
setwd("C:/Users/CEYDA/Desktop/emprical_research")

# install.packages("GGally")
# install.packages("sna")
# install.packages("RColorBrewer")
# install.packages("intergraph")
# install.packages("tidyverse")
# install.packages("igraph")

library(GGally)
library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)
library(intergraph)
library(tidyverse)
library(igraph)

#Load the data

load("~/Desktop/emprical_research/data/tradebook/alldata_1.RData")
#load("~/Desktop/emprical_research/data/tradebook/alldata_2.RData")


# First start with only AKBNK.E, for now only first week.

akbank <- alldata_1[alldata_1$INSTRUMENTCODE=="AKBNK.E",]


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

# for checking matching group id is also unique across days.
akbank18 <- akbank[akbank$TRADEDATE=="2019-03-18",]
akbank19 <- akbank[akbank$TRADEDATE=="2019-03-19",]
akbank20 <- akbank[akbank$TRADEDATE=="2019-03-20",]
akbank21 <- akbank[akbank$TRADEDATE=="2019-03-21",]
akbank22 <- akbank[akbank$TRADEDATE=="2019-03-22",]

# you dropped the unneceserray columns for now
akbank <- akbank[-c(1,14,16,19,20,22,23,25,26,27,28)]

# define the accounts

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

# counteraccount ekleme
# akbank  <- akbank  %>% group_by(MATCHTRADEGROUPID) %>% mutate(counter= row_number(MATCHTRADEGROUPID))
# 
# akbank <- akbank %>% group_by(MATCHTRADEGROUPID) %>% mutate(counteraccount = ifelse(counter=="2", paste(account[counter==1]), paste(account[counter==2])))
# 
# save(akbank, file="data/akbank/akbankwc_1.RData")

load("~/Desktop/emprical_research/data/akbank/akbankwc_1.RData")

# drop the counter column
akbank <- akbank[-c(21)]


net <- akbank[akbank$TRADEDATE=="2019-03-22",  c(1,2,6,7,20,21)]

net <- mutate(net, tofrom = paste(account," ",counteraccount))

net_single <- aggregate(net$QUANTITY, by = list(net$GENERICDESC,net$TRADEDATE,net$BUYSELL, net$account, net$counteraccount
, net$tofrom), FUN=sum)

colnames(net_single)<- c("GENERICDESC","TRADEDATE","BUYSELL","account","counteraccount","tofrom", "quantity")

# d will describe the edges of the network, should start with 2 columns containing 
# source and target node ids for each network tie.

# first delete column 1 and 6 - unnecessary anymore.
net_single <- net_single[-c(1,6)]

# make the order of columns
net_single <- net_single[c(3,4,5,1,2)]

#generate connection column, turn it into factor variable, take only one side
net_single$type <-ifelse(grepl("*_OTHER", net_single$account),"nonHFT", "HFT")
net_single$type <- as.factor(net_single$type)

# vertices are nodes so its your one column account df
nodes <- unique(net_single$account)
nodes <- as.data.frame(nodes)

unique(net_single$account)

g <- graph_from_data_frame(net_single, directed=TRUE, vertices=nodes)
str(g)
g

plot(g, edge.arrow.size=.2, edge.color="gray50", vertex.color="tomato", vertex.label=NA, main = "March 22nd, 2019")





