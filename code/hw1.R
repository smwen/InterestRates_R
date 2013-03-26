# Data Cleaning

##setwd("~/Users/SMW/Dropbox/Coursera/DA_012213/Homework/HW1/code")
setwd("~/Documents/Sandy/DA_HW1/code")
### load loansData from RDA
load("../data/loansData.rda")

## cluster code
source('~/Documents/Sandy/DA_HW1/code/myplclust.R')

### convert columns to numeric, when applicable
loansData$Interest.Rate <- as.numeric(sub("%", "", loansData$Interest.Rate))
loansData$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loansData$Debt.To.Income.Ratio))
loansData$Amount.Requested <- as.numeric(loansData$Amount.Requested)
loansData$Amount.Funded.By.Investors <- as.numeric(loansData$Amount.Funded.By.Investors)
loansData$Monthly.Income <- as.numeric(loansData$Monthly.Income)
loansData$Open.CREDIT.Lines <- as.numeric(loansData$Open.CREDIT.Lines)
loansData$Revolving.CREDIT.Balance <- as.numeric(loansData$Revolving.CREDIT.Balance)
loansData$Inquiries.in.the.Last.6.Months<- as.numeric(loansData$Inquiries.in.the.Last.6.Months)

### split FICO Range into min/max
loansData$FICO.Range.min <- as.numeric(gsub("-(.*)","",loansData$FICO.Range))
loansData$FICO.Range.max <- as.numeric(gsub("(.*)-","",loansData$FICO.Range))

### convert loan length into numeric
loansData$Loan.Length.months <- as.numeric(gsub(" months","",loansData$Loan.Length))

head(loansData)
names(loansData)
table(is.na(loansData)) # 7 NAs
plot(loansData[,-3], col=c("red","gray"))

# svd attempt

loansSub <- na.omit(loansData[,c(1,2,3,17,6,9,15,11,12,13)])
# large outlier due to entry 54487; High monthly income, so it is removed
loansSub <- loansSub[loansSub$Monthly.Income<100000,]
names(loansSub)

#source("http://bioconductor.org/biocLite.R")
#biocLite("impute")
#library(impute)
#loansSub <- impute.knn(loansSub)

svd1 <- svd(scale(loansSub))
plot(svd1$d^2/sum(svd1$d^2), col="blue", pch=19, cex=0.5)
loansSub_pca <- prcomp(loansSub, scale=TRUE)
summary(loansSub_pca)
loansSub_pca$rotation[,1]    # look at LOADINGs matrix
# loadings matrix indicate that Inquiries in the Last 6 months has minimal effect
predict(loansSub_pca)[,1]
plot(loansSub_pca)
cor(loansSub$Interest.Rate, loansSub_pca$x[,1])
biplot(loansSub_pca, col=c("gray","red"), cex=0.7)

## exploratory plots
plot(loansData$Amount.Requested, loansData$Interest.Rate, col="blue", pch=19, cex=0.5)


### higher FICO score, lower rates
par(mfrow=c(1,3))
boxplot(loansData$Interest.Rate ~ as.factor(loansData$FICO.Range), cex=0.5)
boxplot(loansData$Interest.Rate ~ as.factor(loansData$Home.Ownership), col=6:10, cex=0.5, varwidth=TRUE)
boxplot(loansData$Interest.Rate ~ as.factor(loansData$Loan.Purpose), cex=0.5)
boxplot(loansData$Interest.Rate ~ as.factor(loansData$State), cex=0.5)
barplot(table(loansData$State), col="orange", cex.axis=0.7)
par(mfrow=c(1,1))

### higher interest rates for really high number of credit lines, but sample size is small
boxplot(loansData$Interest.Rate ~ loansData$Open.CREDIT.Lines, cex=0.5, varwidth=TRUE)

### look at one FICO score bucket
levels(loansData$FICO.Range)
table(loansData$FICO.Range)
## pick bottom, center, middle
## "660-664", "780-784", "720-724"
low_FICO <- subset(loansData, FICO.Range=="660-664")
mid_FICO <- subset(loansData, FICO.Range=="720-724")
high_FICO <- subset(loansData, FICO.Range=="780-784")
plot(low_FICO$Amount.Requested, low_FICO$Interest.Rate, col="blue", pch=19, cex=0.5)
points(mid_FICO$Amount.Requested, mid_FICO$Interest.Rate, col="red", pch=19, cex=0.5)
points(high_FICO$Amount.Requested, high_FICO$Interest.Rate, col="green", pch=19, cex=0.5)

## Try clustering
numericLP <- as.numeric(as.factor(loansData$Loan.Purpose))[loansData$FICO.Range == "780-784"]
plot(loansData[loansData$FICO.Range =="780-784",3], pch=19, col=numericLP)
legend(150, -0.1, legend=unique(loansData$Loan.Purpose), col=unique(numericLP), pch=19)

distanceM <- dist(loansData[loansData$FICO.Range=="720-724" & loansData$Loan.Purpose!="credit_card",c(1,3,6)])
hclustM <- hclust(distanceM)
myplclust(hclustM, lab.col=as.numeric(loansData$Loan.Purpose), cex=0.5)

distAll <-dist(loansData[,1:3])
hclustAll <- hclust(distAll)
myplclust(hclustAll, lab.col=numericLP, cex=0.2)

## More Money requested, higher interest rates
plot(low_FICO$Amount.Requested, low_FICO$Interest.Rate, col=as.factor(low_FICO$Loan.Purpose), pch=19, cex=0.5)
lFh <- hclust(dist(low_FICO[,1:5]))
plot(lFh, col=as.factor(low_FICO$Loan.Purpose), cex=0.5)

plot(mid_FICO$Amount.Requested, mid_FICO$Interest.Rate, col=as.factor(mid_FICO$Loan.Purpose), pch=19, cex=0.5)
plot(high_FICO$Amount.Requested, high_FICO$Interest.Rate, col=as.factor(mid_FICO$Loan.Purpose), pch=19, cex=0.5)

### reducing dimensions
table(complete.cases(loansData))

# First stab at linear regression
lm1 <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested)
summary(lm1)
plot(loansData$Amount.Requested, loansData$Interest.Rate, col="blue", pch=19, cex=0.5)
abline(lm1$coeff, col="red", lwd=3)
confint(lm1)

lm2 <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested + loansData$FICO.Range)
summary(lm2)
lm2

## this seems to indicate Amount.Requested and FICO.Range have minimal interaction
## since P-values are high, so not statistically significant
lm3 <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested + loansData$FICO.Range 
          + loansData$Amount.Requested*loansData$FICO.Range)
summary(lm3)
lm3

# plot(loansData$Monthly.Income, loansData$Interest.Rate, col="blue", pch=19, cex=0.5)
plot(log10(loansData$Monthly.Income+1), loansData$Interest.Rate, col="blue", pch=19, cex=0.5)
boxplot(loansData$Interest.Rate ~ as.factor(loansData$Loan.Length), col=c("blue","orange"), varwidth=TRUE)
boxplot(loansData$Interest.Rate ~ loansData$Inquiries.in.the.Last.6.Months, cex=0.5, varwidth=TRUE)
plot(loansData$Amount.Funded.By.Investors, loansData$Interest.Rate, pch=19, cex=0.5)
plot(loansData$Debt.To.Income.Ratio, loansData$Interest.Rate, pch=19, cex=0.5)
summary(loansData$Employment.Length)
boxplot(loansData$Interest.Rate ~ as.factor(loansData$Employment.Length), cex=0.5, varwidth=TRUE)
plot(log10(loansData$Revolving.CREDIT.Balance+1), loansData$Interest.Rate, cex=0.5)
plot(loansData$Revolving.CREDIT.Balance, loansData$Interest.Rate, cex=0.5)
# hist(loansData$Interest.Rate)
# plot(loansData$Amount.Requested, loansData$Amount.Funded.By.Investors/loansData$Amount.Requested, col="blue", pch=19, cex=0.5)
hist(loansData$Amount.Requested)
# hist((loansData$Amount.Funded.By.Investors/loansData$Amount.Requested), breaks=100)
# percentFunded <- loansData$Amount.Funded.By.Investors/loansData$Amount.Requested
# plot(loansData$Interest.Rate, percentFunded, col=as.factor(loansData$FICO.Range), pch=19, cex=0.5)
# plot(loansData$Interest.Rate, loansData$Amount.Requested, col=as.factor(loansData$FICO.Range), pch=19, cex=0.5)
# plot(loansData$Interest.Rate, loansData$Amount.Requested, col=as.factor(loansData$State), pch=19, cex=0.5)

# some pattern here <-- Shorter loans, 
# plot(loansData$Interest.Rate, loansData$Monthly.Income, col=as.factor(loansData$Loan.Length), pch=19, cex=0.5)
plot(loansData$Interest.Rate, log10(loansData$Monthly.Income+1), col=as.factor(loansData$Loan.Length), pch=19, cex=0.5)
plot(loansData$Interest.Rate, log10(loansData$Monthly.Income+1), col=as.factor(loansData$FICO.Range), pch=19, cex=0.5)
plot(loansData$Interest.Rate, log10(loansData$Monthly.Income+1), col=as.factor(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansData$Interest.Rate, log10(loansData$Monthly.Income+1), col=as.factor(loansData$Home.Ownership), pch=19, cex=0.5)
plot(loansData$Interest.Rate, loansData$Amount.Requested, col=as.factor(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansData$Amount.Requested, loansData$Interest.Rate, col=as.factor(loansData$Loan.Purpose), pch=19, cex=0.5)

## reiterate <-- smaller loans tend to be shorter
plot(loansData$Amount.Requested, loansData$Interest.Rate, col=as.factor(loansData$Loan.Length), pch=19, cex=0.5)
plot(loansData$Debt.To.Income.Ratio, loansData$Interest.Rate, col=as.factor(loansData$Home.Ownership), pch=19, cex=0.5)
plot(loansData$Amount.Requested, loansData$Interest.Rate, col=as.factor(loansData$Home.Ownership), pch=19, cex=0.5)

## check Interest rate densities
densIR <- density(loansData$Interest.Rate)
plot(densIR, lwd=3, col="black")

densIR2 <- density(loansData$Interest.Rate[which(loansData$Monthly.Income<2000)])
lines(densIR2, lwd=2, col="red")
densIR2 <- density(loansData$Interest.Rate[which(loansData$Monthly.Income>=2000 & loansData$Monthly.Income<4000)])
lines(densIR2, lwd=2, col="blue")
densIR3 <- density(loansData$Interest.Rate[which(loansData$Monthly.Income>4000)])
lines(densIR3, lwd=2, col="green")

densIR2 <- density(loansData$Interest.Rate[which(loansData$Amount.Requested<6000)])
lines(densIR2, lwd=2, col="red")
densIR2 <- density(loansData$Interest.Rate[which(loansData$Amount.Requested>=6000 & loansData$Amount.Requested<10000)])
lines(densIR2, lwd=2, col="blue")
densIR3 <- density(loansData$Interest.Rate[which(loansData$Amount.Requested>=10000 & loansData$Amount.Requested<17000)])
lines(densIR3, lwd=2, col="green")
densIR4 <- density(loansData$Interest.Rate[which(loansData$Amount.Requested>=17000)])
lines(densIR4, lwd=2, col="orange")

hist(loansData$Interest.Rate)
hist(loansData$Amount.Requested)
barplot(table(loansData$Loan.Length))

hist(loansData$Interest.Rate[which(loansData$Amount.Requested<6000)])


### conclusions so far: correlation between Interest Rate and Loan Length
### Larger loans in the >75 percentile tend to have higher interest rates
### Smaller loans tend to be shorter
### So if FICO score was the same, I would expect to look at loan length and duration
### Not much to say about monthly income and debt ratios at this time

hist (loansData$Debt.To.Income.Ratio)

plot(loansData$Debt.To.Income.Ratio, log10(loansData$Monthly.Income+1), col=loansData$FICO.Range, pch=19, cex=0.5)

#maybe something here?
plot(loansData$Debt.To.Income.Ratio, log10(loansData$Monthly.Income+1), col=loansData$Home.Ownership, pch=19, cex=0.5)
table(loansData$Loan.Purpose)
