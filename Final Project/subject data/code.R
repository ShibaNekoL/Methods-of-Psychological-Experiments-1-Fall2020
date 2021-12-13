data <- read.csv(file.choose(), header=T)

data <- subset(data, Procedure=="wkproc")

allsubject <- data.frame(mean.ACC=0, m500.ACC=0, m1500.ACC=0, e100.ACC=0, e500.ACC=0, e900.ACC=0, m500e100.ACC=0, m500e500.ACC=0, m500e900.ACC=0, m1500e100.ACC=0, m1500e500.ACC=0, m1500e900.ACC=0, mean.RT=0, m500.RT=0, m1500.RT=0, e100.RT=0, e500.RT=0, e900.RT=0, m500e100.RT=0, m500e500.RT=0, m500e900.RT=0, m1500e100.RT=0, m1500e500.RT=0, m1500e900.RT=0)
allsubject <- allsubject[-1,]

for(i in 1:8){
  subject <- subset(data, Subject==i)
  
  # ACC
  mean.ACC <- mean(subject$resp.ACC)
  
  m500.ACC <- mean(subset(subject, memorizing==500)$resp.ACC)
  m1500.ACC <- mean(subset(subject, memorizing==1500)$resp.ACC)
  
  e100.ACC <- mean(subset(subject, encoding==100)$resp.ACC)
  e500.ACC <- mean(subset(subject, encoding==500)$resp.ACC)
  e900.ACC <- mean(subset(subject, encoding==900)$resp.ACC)
  
  m500e100.ACC <- mean(subset(subject, memorizing==500 & encoding==100)$resp.ACC)
  m500e500.ACC <- mean(subset(subject, memorizing==500 & encoding==500)$resp.ACC)
  m500e900.ACC <- mean(subset(subject, memorizing==500 & encoding==900)$resp.ACC)
  m1500e100.ACC <- mean(subset(subject, memorizing==1500 & encoding==100)$resp.ACC)
  m1500e500.ACC <- mean(subset(subject, memorizing==1500 & encoding==500)$resp.ACC)
  m1500e900.ACC <- mean(subset(subject, memorizing==1500 & encoding==900)$resp.ACC)
  
  # RT
  # only the right answer will be calculated
  subject <- subset(subject, resp.ACC==1)
  
  mean.RT <- mean(subject$resp.RT)
  
  m500.RT <- mean(subset(subject, memorizing==500)$resp.RT  )
  m1500.RT <- mean(subset(subject, memorizing==1500)$resp.RT  )
  
  e100.RT <- mean(subset(subject, encoding==100)$resp.RT  )
  e500.RT <- mean(subset(subject, encoding==500)$resp.RT  )
  e900.RT <- mean(subset(subject, encoding==900)$resp.RT  )
  
  m500e100.RT <- mean(subset(subject, memorizing==500 & encoding==100)$resp.RT  )
  m500e500.RT <- mean(subset(subject, memorizing==500 & encoding==500)$resp.RT  )
  m500e900.RT <- mean(subset(subject, memorizing==500 & encoding==900)$resp.RT  )
  m1500e100.RT <- mean(subset(subject, memorizing==1500 & encoding==100)$resp.RT  )
  m1500e500.RT <- mean(subset(subject, memorizing==1500 & encoding==500)$resp.RT  )
  m1500e900.RT <- mean(subset(subject, memorizing==1500 & encoding==900)$resp.RT  )
  
  onesubject <- cbind(mean.ACC, m500.ACC, m1500.ACC, e100.ACC, e500.ACC, e900.ACC, m500e100.ACC, m500e500.ACC, m500e900.ACC, m1500e100.ACC, m1500e500.ACC, m1500e900.ACC, mean.RT, m500.RT, m1500.RT, e100.RT, e500.RT, e900.RT, m500e100.RT, m500e500.RT, m500e900.RT, m1500e100.RT, m1500e500.RT, m1500e900.RT)
  allsubject <- rbind(allsubject, onesubject)
}


# mean (not subject)
meanACC <- read.csv(file.choose(), header=T)
meanRT <- read.csv(file.choose(), header=T)

meandata <- merge(meanACC, meanRT, by=c("encoding", "memorizing"))


# mean anova
# continous type
# no interaction
summary(aov(ACC ~ encoding, data = meandata))

model.ACC <- aov(ACC ~ encoding + memorizing, data = meandata)
summary(model.ACC)

model.RT <- aov(RT ~ encoding + memorizing, data = meandata)
summary(model.RT)


# interaction
model.ACC <- aov(ACC ~ encoding * memorizing, data = meandata)
summary(model.ACC)

model.RT <- aov(RT ~ encoding * memorizing, data = meandata)
summary(model.RT)


# factor
################### repeated measured
library(afex)
sACC <- read.csv(file.choose())


sACC$no <- factor(sACC$no)

r.model <- aov_car(ACC ~ c(encoding, memorizing) + Error(no/c(encoding, memorizing)), sACC)
summary(r.model)



sRT <- read.csv(file.choose())

sRT$no <- factor(sRT$no)

r.model <- aov_car(RT ~ c(encoding, memorizing) + Error(no/c(encoding, memorizing)), sRT)
summary(r.model)
