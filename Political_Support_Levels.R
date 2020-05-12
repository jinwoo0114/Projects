library(MASS)
library(ggplot2)
library(dplyr) # for %>%
library(gmodels) # for CrossTables

setwd("C:/Users/chung/Downloads")
d = read.csv('GOVRACEtargetingpoll.csv')

dim(d)
colnames(d)
summary(d)


# use tables, cross tabs and graphs to assess the predictive power of attributes

table(d$Vote) 
d$Vote<-recode_factor(d$Vote, '1' = "Orson", '2'="Trout", '3'="Pans", '9'="DK")
table(d$Vote)
margin.table(table(d$Vote))
100*table(d$Vote)/margin.table(table(d$Vote))

table(d$Gender,d$Vote)
rowSums(table(d$Gender,d$Vote))
100*table(d$Gender,d$Vote)/rowSums(table(d$Gender,d$Vote))

table(d$Party)
100*table(d$Party,d$Vote,dnn=c("Party","Vote")) /rowSums(table(d$Party,d$Vote))
d$Party<-recode_factor(d$Party, '1' = "Strong D", '2'="Weak D", '3'="Strong R", '4'="Weak R",'5'="Lean D", '6'="Lean R",'7'='Independent')
100*table(d$Party,d$Vote,dnn=c("Party","Vote")) /rowSums(table(d$Party,d$Vote))


#  redefine non-scalar attributes as indicator or dummy variables
d$VoteOrson<- 0  #create variable
d$VoteOrson[ d$Vote == 'Orson'] <- 1 # set selected case equal to 1
d %>% group_by(Gender, MaritalStatus) %>% summarise_at(vars(VoteOrson), funs(n(),mean)) 
ggplot( d, aes( x=MaritalStatus, y=VoteOrson )) + stat_summary( fun.y="mean", geom="bar") 
                                                                
# use help(ggplot) to see other options   

#Looking at voters by gender and party affilation
d$VoteOrson<- 0  #create variable
d$VoteOrson[ d$Vote == 'Orson'] <- 1 # set selected case equal to 1
d %>% group_by(Gender, Party) %>% summarise_at(vars(VoteOrson), funs(n(),mean)) 
ggplot( d, aes( x=Party, y=VoteOrson )) + stat_summary( fun.y="mean", geom="bar")


# 1. create a variable called d$VoteOrsonWithLeaners
# includes d$Vote='Orson' or d$Lean='1'

table(d$Vote,d$Lean,dnn=c("Vote","Lean"))
d$OrsonWithLeaners<-0
d$OrsonWithLeaners[d$Vote=='Orson' | d$Lean=='1']<-1
table(d$OrsonWithLeaners)

# 2. create an indicator variables for d$Female,   d$Democrat  and d$Married
#Vote for Orson and Female
table(d$Vote,d$Gender)
d$OrsonWithFemale<-0
d$OrsonWithFemale[d$Vote=='Orson' & d$Gender=='F']<-1
table(d$OrsonWithFemale)

#Vote for Orson and Democrat
table(d$Vote,d$regas)
d$OrsonWithDem<-0
d$OrsonWithDem[d$Vote=='Orson' & d$regas=='DEMOCRA']<-1
table(d$OrsonWithDem)

#Vote for Orson and Married
table(d$Vote,d$MaritalStatus)
d$OrsonWithMarried<-0
d$OrsonWithMarried[d$Vote=='Orson' & d$MaritalStatus=='M']<-1
table(d$OrsonWithMarried)


#  replace missing values

summary(d$homeage)
is.na(d$homeage)[1:100]
sum(is.na(d$homeage))

mean(d$homeage)
mean(d$homeage,na.rm=TRUE)
d[is.na(d$homeage),"homeage"]<-mean(d$homeage,na.rm=TRUE)



# 3. replace missing values from all other VOTERFILE variables with NA's (using mean, median or 0)


# Replace ag6_11 NA with 0
sum(is.na(d$ag6_11))
mean(d$ag6_11)
mean(d$ag6_11,na.rm=TRUE)
d[is.na(d$ag6_11),"ag6_11"]<-0


# Replace pwhitcol2000 NA with mean
sum(is.na(d$pwhitcol2000))
mean(d$pwhitcol2000)
mean(d$pwhitcol2000,na.rm=TRUE)
d[is.na(d$pwhitcol2000),"pwhitcol2000"]<-mean(d$pwhitcol2000,na.rm=TRUE)

# Replace phhblack2000 NA with mean
sum(is.na(d$phhblack2000))
mean(d$phhblack2000)
mean(d$phhblack2000,na.rm=TRUE)
d[is.na(d$phhblack2000),"phhblack2000"]<-mean(d$phhblack2000,na.rm=TRUE)

# Replace childcnt NA with 0
sum(is.na(d$childcnt))
mean(d$childcnt)
mean(d$childcnt,na.rm=TRUE)
d[is.na(d$childcnt),"childcnt"]<-0


# remove outliers

summary(d$Age)
d$Age[d$Age>100]<-100


# create training dset with sample()
sampled <- sample( 1:nrow(d), 7500, replace=FALSE )
training <- d[ sampled, ]
holdout <- d[ -sampled, ]


#Fit logistic regression model on training data

fit <- glm(VoteOrson ~   p2000 + phhblack2000 +  homeage + childcnt, 
           training, family = "binomial")

summary(fit)
summary(training$childcnt)
training[is.na(training$childcnt),"childcnt"]<-0
# fix the input data so you have no deleted observations

# score the holdout sample
holdout$OrsonPred <- predict(fit, holdout, type="response")
# sort the sample from low to high
holdout <- holdout[order(holdout$OrsonPred),]   

# plot the results in equal deciles
holdout$decile = as.integer( cut( holdout$OrsonPred, quantile( holdout$OrsonPred, probs=0:10/10, na.rm=TRUE )) )
ggplot( holdout, aes( x=factor(decile ), y=VoteOrson )) + stat_summary( fun.y ="mean", geom="bar" )




# OPTIONAL
# selecting variables using stepwise
minfit<-glm(VoteOrson ~ 1,data=training,family = "binomial")
summary(minfit)
biggest<-formula(glm(VoteOrson ~  childcnt  +  ag6_11 +
                       p2000 + p2001 + p2002 + p2003 + p2004 +
                       g2000 + g2001 + g2002 + g2003 + g2004 +
                       Age + homeage + Female + Dem + 
                       phhblack2000 + pwhitcol2000,data=training,family = "binomial"))
fwdfit<-step(minfit, direction='forward',scope=biggest, family='binomial')
summary(fwdfit)

# score the holdout sample
d$OrsonPred <- predict(fwdfit, d, type="response")
d$decile = as.integer( cut( d$OrsonPred, quantile( d$OrsonPred, probs=0:10/10, na.rm=TRUE )) )
ggplot(d) +
  geom_bar(aes(factor(decile), VoteOrson, fill = train), 
           position = "dodge", stat = "summary", fun.y = "mean")
