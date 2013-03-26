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
