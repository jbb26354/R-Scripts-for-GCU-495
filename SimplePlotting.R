
# input Data
commute=read.csv(file.choose(), header=T)


#get commute column
comm=commute$Commute


#make histogram
h=hist(comm)
hist(comm,breaks=20)
br=seq(0,60,5)
h=hist(comm,breaks=br)
lines(h$mids,h$counts)


#make quantile plot
sortWage=sort(wage)
rankWage=rank(sortWage)
pWage=rankWage/(length(wage)+1)
plot(sortWage,pWage)
plot(sortWage,100*pWage,main='teacher wages quantile plot',xlab='wages in dollars/year',ylab='p',col='red') #make it pretty

#quartiles
plot(sortComm,4*pComm)
plot(sortWage,4*pWage)
#percentiles
plot(sortComm,100*pComm)


#make boxplot
boxplot(comm)
boxplot(comm ~ commute$gender)


#make cumulative frequency histogram
h = hist(comm)
h$counts = cumsum(h$counts)
plot(h,main="Cumulative histogram")

