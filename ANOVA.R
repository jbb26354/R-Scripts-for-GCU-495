
x = read.csv(file.choose(), header=T)
state=x$GESTCEN
wage=x$PTERNWA
wage[wage<1]=NA
gender=x$PESEX
education=x$PREDUCA5
wageAZ=wage[state==86]
genderAZ=gender[state=86]
educationAZ=education[state=86]
mwage=wage[gender==1]
fwage=wage[gender==2]
mwageAZ=wageAZ[gender==1]
fwageAZ=wageAZ[gender==2]
var.test(mwage,fwage)
t.test(fwage,mwage,alternative="less")
aw=aov(wage~factor(education))
summary(aw)
print(model.tables(aw,"means"),digits=3) 
TukeyHSD(aw)
