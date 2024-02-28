#question 1a 
df_nhpi<-read.csv("proj1_2022.csv",header=TRUE)
#question 1b-a
df_nhpi$HTN[df_nhpi$HTN=="7"|df_nhpi$HTN=="8"|df_nhpi$HTN=="9"]<-NA
df_nhpi$HTNMED[df_nhpi$HTNMED=="7"|df_nhpi$HTNMED=="8"|df_nhpi$HTNMED=="9"]<-NA
df_nhpi$MI[df_nhpi$MI=="7"|df_nhpi$MI=="8"|df_nhpi$MI=="9"]<-NA
df_nhpi$COPD[df_nhpi$COPD=="7"|df_nhpi$COPD=="8"|df_nhpi$COPD=="9"]<-NA
df_nhpi$CANCER[df_nhpi$CANCER=="7"|df_nhpi$CANCER=="8"|df_nhpi$CANCER=="9"]<-NA
df_nhpi$DIABETES[df_nhpi$DIABETES=="7"|df_nhpi$DIABETES=="8"|df_nhpi$DIABETES=="9"]<-NA
df_nhpi$HEIGHT[df_nhpi$HEIGHT=="96"|df_nhpi$HEIGHT=="97"|df_nhpi$HEIGHT=="98"|df_nhpi$HEIGHT=="99"]<-NA
df_nhpi$WEIGHT[df_nhpi$WEIGHT=="996"|df_nhpi$WEIGHT=="997"|df_nhpi$WEIGHT=="998"|df_nhpi$WEIGHT=="999"]<-NA
#question1b-c
df_nhpi$BMI<-(df_nhpi$WEIGHT/((df_nhpi$HEIGHT)^2)*703)
# part 1 : question1c
df_nhpi$SMOKING <- ifelse(df_nhpi$SMKSTAT2==9, "NA", ifelse(df_nhpi$SMKSTAT2==4, "never", ifelse(df_nhpi$SMKSTAT2==3, "former", ifelse(df_nhpi$SMKSTAT2==5, "current", ifelse(df_nhpi$SMKSTAT2<=2,"current", NA)))))
#question 1d
df_nhpi$HTN<-ifelse(df_nhpi$HTN==1,"Yes","NO")
df_nhpi[is.na(df_nhpi$HTN),]$HTN<-"Unknown"
df_nhpi$HTNMED<-ifelse(df_nhpi$HTNMED==1,"Yes","NO")
df_nhpi[is.na(df_nhpi$HTNMED),]$HTNMED<-"Unknown"
df_nhpi$MI<-ifelse(df_nhpi$MI==1,"Yes","NO")
df_nhpi[is.na(df_nhpi$MI),]$MI<-"Unknown"
df_nhpi$COPD<-ifelse(df_nhpi$COPD==1,"Yes","NO")
df_nhpi[is.na(df_nhpi$COPD),]$COPD<-"Unknown"
df_nhpi$CANCER <- ifelse(df_nhpi$CANCER==1,"YES", "NO")
df_nhpi[is.na(df_nhpi$CANCER),]$CANCER <- "Unknown"
df_nhpi$DIABETES<-ifelse(df_nhpi$DIABETES==1,"Yes",ifelse(df_nhpi$DIABETES==2,"NO","BORDERLINE"))
df_nhpi[is.na(df_nhpi$DIABETES),]$DIABETES <- "Unknown"
df_nhpi$SEX <- ifelse(df_nhpi$SEX==1,"MALE","FEMALE")
#question1e
df_nhpi$MAR_STAT <- ifelse(df_nhpi$MAR_STAT==4,0, ifelse(df_nhpi$MAR_STAT==2&3,1, ifelse(df_nhpi$MAR_STAT==1&5,2,NA)))
#part 2
#age 
tapply(df_nhpi$AGE,df_nhpi$SEX,mean,na.rm=TRUE)
tapply(df_nhpi$AGE,df_nhpi$SEX,sd,na.rm=TRUE)
t.test(df_nhpi$AGE~df_nhpi$SEX,var.equal=TRUE)
#height
tapply(df_nhpi$HEIGHT,df_nhpi$SEX,mean,na.rm=TRUE)
tapply(df_nhpi$HEIGHT,df_nhpi$SEX,sd,na.rm=TRUE)
t.test(df_nhpi$HEIGHT~df_nhpi$SEX,var.equal=TRUE)
#weight
tapply(df_nhpi$WEIGHT,df_nhpi$SEX,mean,na.rm=TRUE)
tapply(df_nhpi$WEIGHT,df_nhpi$SEX,sd,na.rm=TRUE)
t.test(df_nhpi$WEIGHT~df_nhpi$SEX,var.equal=TRUE)
#BMI
tapply(df_nhpi$BMI,df_nhpi$SEX,mean,na.rm=TRUE)
tapply(df_nhpi$BMI,df_nhpi$SEX,sd,na.rm=TRUE)
t.test(df_nhpi$BMI~df_nhpi$SEX,var.equal=TRUE)
#MAR_STAT
table(df_nhpi$MAR_STAT,df_nhpi$SEX,exclude=NULL)
100*prop.table(table(df_nhpi$MAR_STAT,df_nhpi$SEX,exclude=NULL),2)
chisq.test(df_nhpi$MAR_STAT,df_nhpi$SEX, correct = FALSE)
#HTN
table(df_nhpi$HTN,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$HTN,df_nhpi$SEX,exclude = NULL),2)
df_htn<-df_nhpi[!(df_nhpi$HTN=="Unknown"),]
chisq.test(df_htn$HTN,df_htn$SEX,correct=FALSE)

#HTNMED
table(df_nhpi$HTNMED,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$HTNMED,df_nhpi$SEX,exclude = NULL),2)
df_HTNMED<-df_nhpi[!(df_nhpi$HTNMED=="Unknown"),]
chisq.test(df_HTNMED$HTNMED,df_HTNMED$SEX,correct = FALSE)
#MI
table(df_nhpi$MI,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$MI,df_nhpi$SEX,exclude = NULL),2)
df_MI<-df_nhpi[!(df_nhpi$MI=="Unknown"),]
chisq.test(df_MI$MI,df_MI$SEX,correct = FALSE)

#SMOKING
table(df_nhpi$SMOKING,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$SMOKING,df_nhpi$SEX,exclude = NULL),2)
df_SMOKING<-df_nhpi[!(df_nhpi$SMOKING=="Unknown"),]
chisq.test(df_SMOKING$SMOKING,df_SMOKING$SEX,correct = FALSE)
#COPD
table(df_nhpi$COPD,df_nhpi$SEX,exclude=NULL)
100*prop.table(table(df_nhpi$COPD,df_nhpi$SEX,exclude=NULL),2)
df_COPD<-df_nhpi[!(df_nhpi$COPD=="Unknown"),]
chisq.test(df_COPD$COPD,df_COPD$SEX,correct = FALSE)

#CANCER
table(df_nhpi$CANCER,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$CANCER,df_nhpi$SEX,exclude = NULL),2)
df_CANCER<-df_nhpi[!(df_nhpi$CANCER=="Unknown"),]
chisq.test(df_CANCER$CANCER,df_CANCER$SEX,correct = FALSE)
#Diabetes 
table(df_nhpi$DIABETES,df_nhpi$SEX,exclude = NULL)
100*prop.table(table(df_nhpi$DIABETES,df_nhpi$SEX, exclude =NULL),2)
df_DIABETES<-df_nhpi[!(df_nhpi$DIABETES=="Unknown"),]
chisq.test(df_DIABETES$DIABETES,df_DIABETES$SEX,correct = FALSE)
# part 3: A 
#male
males <- df_nhpi[(df_nhpi$SEX=="MALE"),]
t.test(males$BMI, mu=27.80, alternative = "two.sided")
#female
female <- df_nhpi[(df_nhpi$SEX=="FEMALE"),]
t.test(female$BMI, mu=27.51, alternative = "two.sided")

#Part 3B
t.test(df_nhpi$BMI~df_nhpi$SEX,var.equal = TRUE)
#Part 3C
currentsmoker<-df_nhpi[(df_nhpi$SMOKING=="current"),]
table(df_nhpi$SMOKING)
length((currentsmoker$SMOKING))
chisq.test(currentsmoker$SEX, currentsmoker$CANCER, correct = FALSE)
x <- table(currentsmoker$SEX, currentsmoker$CANCER)
library(epitools)
riskratio(x, method="wald")


