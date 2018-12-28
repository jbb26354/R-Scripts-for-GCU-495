
# read it in
dataset = c(76.7, 64, 71.1, 40.1, 52.3, 64.5, 74.4, 78.6, 69.5, 62.6, 57.9, 42.4, 69.5, 73, 50.8, 78, 71, 66.6, 48.6, 76.9, 70, 68.9, 70, 72.4, 65.1, 39.3, 69.6, 47.9, 73.1, 72.9, 58, 69.9, 44.9, 51.7, 74, 69.4, 70.5, 57.3, 45, 65.4, 78.5, 73.9, 73, 64.4, 69.5, 45.2, 78.1, 78.5, 71.4, 64, 48.5, 64, 66.3, 72.9, 78.1, 47.2, 72.4, 68.2, 72, 76, 78.1)

# n
n = length(dataset)

# mean
mean = sum(dataset) / length(dataset)

# sample variance
temp = 0
for (v in dataset)
{
 temp = temp + ((v - mean)^2)
}
sampleVariance = temp / (length(dataset) - 1)

#sample standard deviation
sampleSD = sqrt(sampleVariance)

#skewness
temp = 0
for (v in dataset)
{
 temp = temp + ((v - mean)^3)
}
skewness = temp / (n * (sampleSD^3))

#kurtosis
temp = 0
for (v in dataset)
{
 temp = temp + ((v - mean)^4)
}
kurtosis = temp / (n * (sampleSD^4))
kurtosisRnorm = kurtosis - 3

#histogram
histogram = hist(dataset, main="Histogram", xlab="Items")

#make quantile plot
sortData=sort(dataset)
rankData=rank(sortData)
pData=rankData/(length(dataset)+1)
plot(sortData,100*pData,main='quantile plot', 
  xlab='x-values',ylab='p',col='blue') 

#make boxplot
boxplot(dataset, main="Box Plot")

#make cumulative frequency histogram
h = hist(dataset)
h$counts = cumsum(h$counts)
plot(h,main="Cumulative histogram")

#quartile calculations
quartile1 = sortData[round((25 * (n + 1))/100, digits=0)]
quartile2 = sortData[round((50 * (n + 1))/100, digits=0)]
quartile3 = sortData[round((75 * (n + 1))/100, digits=0)]
interquartileRange = quartile3 - quartile1
percentile10 = sortData[round((10 * (n + 1))/100, digits=0)]
percentile60 = sortData[round((60 * (n + 1))/100, digits=0)]
percentile90 = sortData[round((90 * (n + 1))/100, digits=0)]








