dev.off()
data <- read.csv("C:/Users/ranja/Downloads/hsb21.csv",header = T)
data$Gender=factor(data$Gender,levels=c(0,1),labels = c("Boy","Girls"))
str(data)
levels(data$Gender)
sum(is.na(data$Gender))
data
plot(density(data$read))
mean(data$read)
sd(data$read)
table(data$read)
t.test(data$read~data$Gender,var.equal=T)
t.test(data$read~data$Gender,var.equal=T)
##
plot(density(data$write))
m=mean(data$write)
sd=sd(data$write)
table(data$write)
m+sd
m-sd
t.test(data$write~data$Gender,var.equal=T)
data$schtyp=factor(data$schtyp,levels=c(1,2),labels = c("Public","Private"))
str(data)
plot(density(data$read))
mean(data$science)
sd(data$science)
plot(density(data$science))
unique(data$schtyp)
t.test(data$read~data$schtyp,var.equal=T)
##Non parametric test
wilcox.test(data$write~data$Gender)
wilcox.test(data$read~data$Gender)
quantile(data2$science,probs = c(0.25,0.50,0.75))
q1=44
q2=50
q3=60
#Science no outlier
data2=data
#Assignment1
data2$Gender=factor(data2$Gender,levels=c(0,1),labels = c("Boy","Girls"))
data2$schtyp=factor(data2$schtyp,levels=c(1,2),
                   labels = c("Public","Private"))
unique(data2$Gender)
str(data2)
count=table(data2$Gender)
table(data2$science)
barplot(count,main="Bar plot for gender",
        xlab="Gender",ylab="count")
data$diff=data2$read~data2$write
#The null hypothesis is accepted which means all the student who have done good in 
#reading have performed good in writing
wilcox.test(data$read,data$write)
#The p value from r is 0.4238 which is greater than 0.05 and therefor the null 
#hypothesis is accepted.
data$performance=data
#Assignment attempt2
#Outliers
quantile(data2$science,probs = c(0.25,0.50,0.75))
table(data2$science)
quantile(data2$math,probs = c(0.25,0.50,0.75))
table(data2$math)
str(data2)
str(data)
data2$schtyp=factor(data2$schtyp,levels=c(1,2),
                   labels = c("Public","Private"))
unique(data2$schtyp)
plot(density(data2$read))
mean(data2$read)
sd(data2$read)
table(data2$read)
t.test(data2$read~data2$Gender,var.equal=T)
qt(p=0.913,df=0.10945, lower.tail=TRUE)
plot(density(data2$read))
#write normality test
plot(density(data2$write))
a=mean(data2$write)
m=round(a,digits=2)
b=sd(data2$write)
sd=round(b,digits = 2)
m+sd
m-sd
table(data2$write)
m-2*sd
m+2*sd
m-3*sd
m+3*sd
head(data2,3)
data2$performance=(data2$read+data2$write+data2$math+data2$science+data2$socst)
data2
#normality check for performance
plot(density(data2$performance))
a=mean(data2$performance)
b=sd(data2$performance)
m=round(a,digits=2)
sd=round(b,digits = 2)
m+sd
m-sd
table(data2$performance)
t.test(data2$performance~data2$Gender,var.equal=TRUE)
data2$prog=factor(data2$prog,levels=c(1,2,3),
                    labels = c("BBA","MBA","PHD"))
unique(data2$prog)
count=table(data2$Gender)
table(data2$science)
barplot(count,main="Bar plot for gender",
        xlab="Gender",ylab="count")
unique(data2$schtyp)
head(data2$Gender)
data2$Gender=factor(data2$Gender,levels=c(0,1),labels = c("Boy","Girls"))
data2$schtyp=factor(data2$schtyp,levels=c(1,2),
                    labels = c("Public","Private"))
count_sch=table(data2$schtyp)
barplot(count_sch,main="Bar plot for School Type",
        xlab="School Type",ylab="count")
#Q5: To see is there a significant 
#difference between science and social science
t.test(data2$science,data2$socst, paired=TRUE)
#normality science
ks.test(data2$science,data2$socst)
a=mean(data2$science)
b=sd(data2$science)
m=round(a,digits = 2)
sd=round(b,digits = 2)
m+sd
m-sd
table(data2$science)
m-2*sd
m+2*sd
m-3*sd
m+3*sd
#Normality social studies
a=mean(data2$socst)
b=sd(data2$socst)
m=round(a,digits = 2)
sd=round(b,digits = 2)
table(data2$socst)
m-2*sd
m+2*sd
m-3*sd
m+3*sd
t.test(data2$science,data2$socst, paired=TRUE)
unique(data2$schtyp)
#Day 2
data2=hsb2
data2$performance=(data2$read+data$write+data$math+data$science+data$socst)

aov(read~prog,data=data2)
str(data2)
data2$ses=factor(data2$ses,levels=c(1,2,3),labels = c("Low","Midle","High"))
unique(data$prog)
data2$race=factor(data2$race,levels=c(1,2,3,4),labels = c("Asian","European","American","Australian"))
unique(data2$ses)
data$prog=factor(data$prog,levels=c(1,2,3),
                  labels = c("BBA","MBA","PHD"))


model_ses=aov(performance~ses,data=data2)
kruskal.test(data2$performance~data2$ses)
summary(model_ses)
data2$average=data2$performance/5
head(data2)
model_race=aov(average~race,data=data2)
summary(model_race)


#barplot
plot(data2$race, col="yellow")
unique(data2$prog)
data2$ses
plot(data2$y,data2$write,main="Variation of Marks",xlab="read",ylab="write")
plot(data2$math,data2$science,main="Variation of Marks",xlab="math",ylab="science",col="brown")
plot(data2$write,data2$socst,main="Variation of Marks",xlab="write",ylab="socst",col="yellow2")
#stacked bar plot
count=table(data2$Gender,data2$prog)
barplot(count,col=c("yellow","red"),legend=rownames(count))
barplot(count,col=c(""))
#subject wise breakup of race
count=table(data2$schtyp,data2$prog)
barplot(count,col=c("yellow","red","green"),legend=rownames(count))
unique(data2$prog)
head(data2$prog)
data2$schtyp=factor(data2$schtyp,levels=c(1,2),labels = c("Public","Private"))
unique(data2$schtyp)
data2$prog=factor(data2$prog,levels=c(1,2,3),
                  labels = c("BBA","MBA","PHD"))
head(data2,2)
#Pie chart
count=table(data2$Gender)
count
lbls=c("Boy","Girl")
pct=(count/sum(count)*100)
pct
lbls=paste(lbls,pct,"%",sep= "")
lbls
pie(count, labels=lbls, col=rainbow(length(lbls)))
#Pie chart2
count=table(data2$race)
boxplot(data2$average)
lbls=c("Asian","European","American","Australian")
lbls=paste(lbls,pct,"%",sep= "")
pie(count, labels=lbls, col=rainbow(length(lbls)))
#Regression
data=read.csv("hs.csv",header=T)
data
model2=lm(read~write,data=data)
model2
data$Pre_read=18.1622+0.6455*data$write
data$error=data$read-data$Pre_read
data$per_error=data$error/data$read
data$abs_error=abs(data$per_error)
head(data)
mean(data$abs_error)
model3=lm(read~science,data=data)
model3
data$Pre_read1=18.3945+0.6526*data$science
data$error1=data$read-data$Pre_read1
data$per_error1=data$error1/data$read
data$abs_error1=abs(data$per_error1)
head(data)
mean(data$abs_error1)
#
model4=lm(read~math,data=data)
model4
data$Pre_read1=14.0725+0.7248*data$math
data$error1=data$read-data$Pre_read1
data$per_error1=data$error1/data$read
data$abs_error1=abs(data$per_error1)
head(data)
mean(data$abs_error1)

#box

data2
diff_readwrite=data2$read-data2$write
boxplot(diff_readwrite)
diff_sciesoci=data2$science-data2$socst
boxplot(diff_sciesoci)
diff_mathscience=data2$math-data2$science
boxplot(diff_mathscience)
#
install.packages("Rtools", dependencies = TRUE)
str(data2)
save.image("C:/USERP/R/Day.RData")
