CancerRisk<- read.csv("D:\\CancerRisk.csv")

#1 
summary(CancerRisk$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  19.00   39.00   48.00   50.15   62.50   83.00 
sd(CancerRisk$Age)
 14.57523
IQR 62.50-39= 23.5

summary(CancerRisk$Calories)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
  445.2  1338.0  1666.8  1796.7  2100.4  6662.2 
sd(CancerRisk$Calories)
 680.3474
IQR 2100.4-1338.0= 762.4 

summary(CancerRisk$Fat)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.40   53.95   72.90   77.03   95.25  235.90 
sd(CancerRisk$Fat)
 33.82944
95.25-53.95 IQR 41.3

#2

hist(CancerRisk$Calories, main="Figure 1.1: Histogram of Calories", xlab="Calories")
 hist(CancerRisk$Fat, main="Figure 1.2: Histogram of Fat", xlab="Grams of Fat")

boxplot(CancerRisk$Calories, main = "Figure 1.3:Boxplot for Calories", xlab = "Calories")
boxplot(CancerRisk$Fat, main = "Figure 1.4:Boxplot for Fat", xlab = "Grams of Fat")

CancerRisk$agecat[CancerRisk$Age < 40] <- "Younger"
CancerRisk$agecat[CancerRisk$Age >= 40 & CancerRisk$Age <50] <- "Middle Aged"
CancerRisk$agecat[CancerRisk$Age >= 50 & CancerRisk$Age <60] <- "Older Adults"
CancerRisk$agecat[CancerRisk$Age >= 60] <- "Oldest"

table(CancerRisk$agecat)

agecat <- ordered(CancerRisk$agecat,c("Younger","Middle Aged","Older Adults","Oldest"))
table(agecat)
agecat_labels <- round(prop.table(table(agecat))*100, 1)
agecat_labels <- paste(agecat_labels, "%", sep="")
colors <- c("mediumorchid","aquamarine","sienna","navyblue")
pie(table(agecat), main="Figure 1.5: Pie Chart of agecat",col=colors,
labels=agecat_labels) 
legend("bottomright", c("Younger","Middle Aged","Older Adults","Oldest"),fill=colors,bg="white")

barplot(table(agecat),xlab="Age Category",names.arg=c("Younger","Middle Aged","Older Adults","Oldest"),
main="Figure 1.6:Bar Plot of agecat",col=c("aquamarine","hotpink","sienna","navyblue"),
ylim=c(0,100),ylab="Frequency")

prop.table(table(widge$Gender,widge$Plant))
prop.table(table(CancerRisk$Gender,CancerRisk$SmokeStat),2)

barplot(prop.table(table(CancerRisk$Gender,CancerRisk$SmokeStat),2), xlab="Smoking Status",
names.arg=c("Never Smoked","Former Smoker","Current Smoker"), main="Figure 1.7:100% Stacked Bar Chart of Gender by SmokeStat",
col=c("aquamarine","hotpink"),ylab="Frequency",xlim=c(0,5))
legend("topright",c("Male","Female"),fill=c("aquamarine","hotpink"))

CancerRisk$agecat[CancerRisk$Age < 40] <- "Younger"
CancerRisk$agecat[CancerRisk$Age >= 40 & CancerRisk$Age <50] <- "Middle Aged"
CancerRisk$agecat[CancerRisk$Age >= 50 & CancerRisk$Age <60] <- "Older Adults"
CancerRisk$agecat[CancerRisk$Age >= 60] <- "Oldest"
table(CancerRisk$agecat)

agecat <- ordered(CancerRisk$agecat,c("Younger","Middle Aged","Older Adults","Oldest"))
table(agecat)
agecat_labels <- round(prop.table(table(agecat))*100, 1)
agecat_labels <- paste(agecat_labels, "%", sep="")
colors <- c("mediumorchid","aquamarine","sienna","navyblue")
pie(table(agecat), main="Figure 1.5: Pie Chart of agecat",col=colors,
labels=agecat_labels) 
legend("bottomright", c("Younger","Middle Aged","Older Adults","Oldest"),fill=colors,bg="white")

set.seed(44522)
samp <- CancerRisk[sample(1:nrow(CancerRisk),75,replace=FALSE),]

CI<-function(x,alpha=0.05){
n<-sum(!is.na(x))
con<-(1-alpha)*100
me<-qt(1-alpha/2,n-1)*sd(x,na.rm=T)/sqrt(n)
lclm<-round(mean(x,na.rm=T)-me,digits=3)
uclm<-round(mean(x,na.rm=T)+me,digits=3)
mean<-round(mean(x,na.rm=T),digits=3)
{limits<-data.frame(cbind(variable=deparse(substitute(x)),n,
c.level=con,
mean,me=round(me,digits=3),lclm,uclm))}
print(limits)
rm(n,con,lclm,uclm,mean)}

CI(samp$Calories)
CI(samp$Calories,alpha=0.05)