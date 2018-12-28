
# QUESTION B.2
# ============

# confidence interval for xBar, sigma is unknown

cIxBarT = function(x,l)
{
  aDiv2 = (1-l)/2;
  pointEstimate = mean(x);
  tScore = qt(l+aDiv2,df=length(x)-1);
  standardError = (sd(x)/sqrt(length(x)));
  iMinus = pointEstimate - (tScore*standardError);
  iPlus = pointEstimate + (tScore*standardError);
  return (paste("Confidence interval for ",l, " is from ", 
                as.character(iMinus), " to ", 
                as.character(iPlus)));
}
cIxBarTdemo = cIxBarT(c(32.1, 34.4, 34.9, 30.6, 38.4, 29.4, 28.9, 32.6, 32.9, 44.9),.99);

# confidence interval for P = pHat = x/n
# x = number of successes, n = sample size, l=confidence level (1-alpha)
cIpHat = function(x,n,l)
{
  pointEstimate = x/n;
  aDiv2 = (1-l)/2;
  zScore = qnorm(l+aDiv2);
  standardError = sqrt(pointEstimate*(1-pointEstimate)/n);
  iMinus = pointEstimate - (zScore*standardError);
  iPlus = pointEstimate + (zScore*standardError);
  return (paste("Confidence interval for ", l, " is from ", 
                as.character(iMinus), " to ", 
                as.character(iPlus)));
}
cIpHatDemo1 = cIpHat(38,50,.95);
cIpHatDemo2 = cIpHat(38,50,.99);
checkIt=prop.test(38,50)$conf.int;

# 
x = 38;
n = 50;
l = .99;
  pointEstimate = x/n;
  aDiv2 = (1-l)/2;
  zScore = qnorm(l+aDiv2);
  standardError = sqrt(pointEstimate*(1-pointEstimate)/n);
  iMinus = pointEstimate - (zScore*standardError);
  iPlus = pointEstimate + (zScore*standardError);
  return (paste("Confidence interval for ", l, " is from ", 
                as.character(iMinus), " to ", 
                as.character(iPlus)));

binom.test();


# QUESTION D.
# ===========

censusDec2013 <- read.csv("C:/Users/jbonifas/Desktop/censusDec2013.csv");

# confidence interval for xBar, sigma is unknown
# x = dataset, l=confidence level (1-alpha)
x = censusDec2013$age;

cIxBarT = function(x,l)
{
  aDiv2 = (1-l)/2;
  se = qt(l+aDiv2,df=length(x)-1)*(sd(x)/sqrt(length(x)));
  iMinus = mean(x) - se;
  iPlus = mean(x) + se;
  return (paste("Confidence interval for ",l, " is from ", 
                as.character(iMinus), " to ", 
                as.character(iPlus)));
}
cIxBarTdemo1 = cIxBarT(x,.95);
cIxBarTdemo2 = cIxBarT(x,.99);

# ------ [order(censusDec2013$gender),]; -------------------------------

censusDec2013 <- read.csv("C:/Users/jbonifas/Desktop/censusDec2013.csv");
# must all be on one line
maleAges <- data.frame(gender=censusDec2013$gender, age=censusDec2013$age)[which(censusDec2013$gender==1),];
femaleAges <- data.frame(gender=censusDec2013$gender, age=censusDec2013$age)[which(censusDec2013$gender==2),];

cIxBarT = function(x,l)
{
  aDiv2 = (1-l)/2;
  se = qt(l+aDiv2,df=length(x)-1)*(sd(x)/sqrt(length(x)));
  iMinus = mean(x) - se;
  iPlus = mean(x) + se;
  return (paste("Confidence interval for ",l, " is from ", 
                as.character(iMinus), " to ", 
                as.character(iPlus)));
}
cIxBarTdemo1 = cIxBarT(maleAges$age,.95);
cIxBarTdemo2 = cIxBarT(femaleAges$age,.95);

# ------------------------
censusDec2013 <- read.csv("~/Dropbox/CoursesCurrent/GCU495/Assignments/5/censusDec2013.csv");
BAs <- data.frame(education=censusDec2013$education)[which(censusDec2013$education>=5),];
x=length(BAs);
n=length(censusDec2013$education);

prop.test(x,n);
binom.test(x,n);

# Question C
# ----------

prop.test(1500*.44,1500)$conf.int - .44:.44;
prop.test(1500*.51,1500)$conf.int - .51:.51;
prop.test(1500*.44,1500,p=.44)$conf.int - .44:.44;
prop.test(1500*.51,1500,p=.51)$conf.int - .51:.51;

binom.test(1500*.44,1500,p=.5)$conf.int - .44:.44;
binom.test(1500*.51,1500,p=.5)$conf.int - .51:.51;
binom.test(1500*.44,1500,p=.44)$conf.int - .44:.44;
binom.test(1500*.51,1500,p=.51)$conf.int - .51:.51;

