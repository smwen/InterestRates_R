# Setup file

##setwd("~/Users/SMW/Dropbox/Coursera/DA_012213/Homework/HW1/code")
setwd("~/Documents/Sandy/DA_HW1/code")
### load loansData from RDA
load("../data/loansData.rda")

## cluster code
source('~/Documents/Sandy/DA_HW1/code/myplclust.R')

# Data Cleaning
### convert columns to numeric, when applicable
loansData$Interest.Rate <- as.numeric(sub("%", "", loansData$Interest.Rate))
loansData$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loansData$Debt.To.Income.Ratio))
loansData$Amount.Requested <- as.numeric(loansData$Amount.Requested)
loansData$Amount.Funded.By.Investors <- as.numeric(loansData$Amount.Funded.By.Investors)
loansData$Monthly.Income <- as.numeric(loansData$Monthly.Income)
loansData$Open.CREDIT.Lines <- as.numeric(loansData$Open.CREDIT.Lines)
loansData$Revolving.CREDIT.Balance <- as.numeric(loansData$Revolving.CREDIT.Balance)
loansData$Inquiries.in.the.Last.6.Months<- as.numeric(loansData$Inquiries.in.the.Last.6.Months)
loansData$Amount.Requested.log10 <- log10(loansData$Amount.Requested)

### split FICO Range into min/max
loansData$FICO.Range.min <- as.numeric(gsub("-(.*)","",loansData$FICO.Range))
loansData$FICO.Range.max <- as.numeric(gsub("(.*)-","",loansData$FICO.Range))
loansData$FICO.Range.mid <- (loansData$FICO.Range.max+loansData$FICO.Range.min)/2

### convert loan length into numeric
loansData$Loan.Length.months <- as.numeric(gsub(" months","",loansData$Loan.Length))

## convert Employment Length to numeric
## assign 0 to <1 year, 10 to 10+ years
loansData$Employment.Length.num <- gsub("< 1 year","0",loansData$Employment.Length)
loansData$Employment.Length.num <- gsub(" year(.*)","",loansData$Employment.Length.num)
loansData$Employment.Length.num <- gsub("n/a","",loansData$Employment.Length.num)
loansData$Employment.Length.num <- as.numeric(gsub("\\+","",loansData$Employment.Length.num))


### Check loansData table
head(loansData)
names(loansData)
table(is.na(loansData)) # 7 NAs
summary(loansData)
### requested amounts are between 1000-35000. 
# WHy is there a -0.01 min value for amount funded?
# Interest rates range from 5.42 to 24.89
# 2 loan lengths: 36 months, 60 months
# Debt to Income Ratio ranges from 0 to 34.91
# FICO ranges from 640 to 834

### Find missing values, check ranges
table(complete.cases(loansData))

# Exploratory Analysis

## increase palette color size to 51


## Check Interest rate vs FICO
plot(jitter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2),
     jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)
smoothScatter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2,loansData$Interest.Rate)
### plot shows decrease in interest rates when FICO score increases

# try alternate colorings based on Loan.purpose, home.ownership, etc.
### see if anything significant about home ownership
### Mortgage, rent, own, other, none
plot(jitter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2),
     jitter(loansData$Interest.Rate), col=as.numeric(loansData$Home.Ownership), pch=19, cex=0.4)
legend("topright", legend=unique(loansData$Home.Ownership), 
       col=unique(as.numeric(loansData$Home.Ownership)), pch=19)
par(mfcol=c(3,1))
hist(loansData$FICO.Range.min[which(loansData$Home.Ownership=="MORTGAGE")])
hist(loansData$FICO.Range.min[which(loansData$Home.Ownership=="RENT")])
hist(loansData$FICO.Range.min[which(loansData$Home.Ownership=="OWN")])
plot(density(loansData$FICO.Range.min[which(loansData$Home.Ownership=="RENT")]), col="red", lwd=1)
lines(density(loansData$FICO.Range.min[which(loansData$Home.Ownership=="MORTGAGE")]), lwd=1, col="green")
lines(density(loansData$FICO.Range.min[which(loansData$Home.Ownership=="OWN")]), lwd=1, col="blue")
lines(density(loansData$FICO.Range.min), lwd=3, col="black")
### minimal conclusion spread of FICO scores is larger for MORTGAGE and OWN categories
### i.e. larger percentage have high FICO scores compared to RENTers
par(mfcol=c(3,1))
hist(loansData$Interest.Rate[which(loansData$Home.Ownership=="MORTGAGE")])
hist(loansData$Interest.Rate[which(loansData$Home.Ownership=="RENT")])
hist(loansData$Interest.Rate[which(loansData$Home.Ownership=="OWN")])
par(mfcol=c(1,1))
plot(density(loansData$Interest.Rate[which(loansData$Home.Ownership=="RENT")]), col="red", lwd=1)
lines(density(loansData$Interest.Rate[which(loansData$Home.Ownership=="MORTGAGE")]), lwd=1, col="green")
lines(density(loansData$Interest.Rate[which(loansData$Home.Ownership=="OWN")]), lwd=1, col="blue")
lines(density(loansData$Interest.Rate), lwd=3, col="black")

## Let's try Loan.Purpose
palette(rainbow(14))
plot(jitter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2),
     jitter(loansData$Interest.Rate), col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.4)
legend("topright", legend=unique(loansData$Loan.Purpose), 
       col=unique(as.numeric(loansData$Loan.Purpose)), pch=19, cex=0.9)
table(loansData$Loan.Purpose)
### 1307 entries (more than half) are related to debt_consolidation. Let's remove and see
### about the other entries
sub1 <- loansData[loansData$Loan.Purpose!="debt_consolidation",c(3,5,15,16)]
plot(jitter(sub1$FICO.Range.min), jitter(sub1$Interest.Rate), col=as.numeric(sub1$Loan.Purpose), 
            pch=19, cex=0.4)
legend("topright", legend=unique(sub1$Loan.Purpose), 
       col=unique(as.numeric(sub1$Loan.Purpose)), pch=19, cex=0.9)
table(loansData$Loan.Purpose)

par(mfcol=c(2,2))
hist(loansData$Interest.Rate[which(loansData$Loan.Purpose=="debt_consolidation")])
hist(loansData$Interest.Rate[which(loansData$Loan.Purpose=="credit_card")])
hist(loansData$Interest.Rate[which(loansData$Loan.Purpose=="home_improvement")])
hist(loansData$Interest.Rate[which(loansData$Loan.Purpose=="other")])
boxplot(loansData$Interest.Rate ~ loansData$Loan.Purpose, cex=0.5)

par(mfcol=c(1,1))
plot(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="debt_consolidation")]), col="red", lwd=1)
lines(density(loansData$Interest.Rate), lwd=3, col="black")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="vacation")]), lwd=1, col="green")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="medical")]), lwd=1, col="blue")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="home_improvement")]), lwd=1, col="magenta")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="small_business")]), lwd=1, col="gray")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="major_purchase")]), lwd=1, col="yellow")
lines(density(loansData$Interest.Rate[which(loansData$Loan.Purpose=="wedding")]), lwd=1, col="cyan")

### based on density plots, there should be some correlation with loan purpose. Specifically,
### the distribution of interest rates flattens out more and skews towards lower rates for
### medical, small business and home improvement, compared to credit card or debt consolidation.
### however, the sample size is smaller for these cases.

## Check FICO-related parameters
### FICO scores are calculated using revolving credit balance, inquiries in the last 12 months (not 6),
### open credit lines
plot(jitter(loansData$FICO.Range.min),jitter(loansData$Inquiries.in.the.Last.6.Months), col="blue", pch=19, cex=0.5)
smoothScatter(loansData$FICO.Range.min, loansData$Inquiries.in.the.Last.6.Months)
### Lower FICO scores are more likely to have more inquiries
### However, def. of inquiries varies.
plot(jitter(loansData$FICO.Range.min),jitter(log10(loansData$Revolving.CREDIT.Balance+1)), col="blue", pch=19, cex=0.5)
smoothScatter(loansData$FICO.Range.min, log10(loansData$Revolving.CREDIT.Balance+1))
plot(jitter(loansData$FICO.Range.min),jitter(loansData$Open.CREDIT.Lines), col="blue", pch=19, cex=0.5)
smoothScatter(loansData$FICO.Range.min, loansData$Open.CREDIT.Lines)

## Ignore States for now
### 45% of the loans are for 5 states (CA, NY, TX, FL, IL), which makes sense, since they
### are populous states. Several states do not have any loans, which may be related to 
### state laws at the time of the observations.

## Loan Length
### As of this writing, Lending Club only allows 36-month terms for loans of $1000-$15,975,
### so naturally larger loan lengths will tend to be for higher amounts.
plot(jitter(loansData$FICO.Range.min),jitter(loansData$Amount.Requested), col=as.numeric(loansData$Loan.Length), pch=19, cex=0.5)
legend("topright", legend=unique(loansData$Loan.Length), col=unique(as.numeric(loansData$Loan.Length)), pch=19, cex=0.9)

## however, the data shows several long-term loans for smaller values
## In general, though, larger loans are more likely to have a longer loan length.
nrow(loansData[loansData$Loan.Length.months==60 & loansData$Amount.Funded.By.Investors>15975,])

### So far -- will ignore things used to calculate FICO score, on the assumption they are already
### correlated with FICO. Also, ignore states for now. (Note that FICO score does not account
### for location). Loan length is iffy; larger loans are more likely to have the 60-month loan
### length.

### Also, it appears that loan rate is set before the amount is funded, so ignore 
### Amount Funded By Investors.

## Lending Club specifics
### From browsing the website, for people with the same FICO, the following info is listed:
### Amount.Requested, Loan.Purpose, Debt.To.Income.Ratio
### Monthly.Income, Home.Ownership, Employment.Length

### conclusions: maybe ignore debt-to-income-ratio, assume it's in FICO
### 
plot(jitter(loansData$Debt.To.Income.Ratio), jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)
plot(jitter(log10(loansData$Monthly.Income+1)), jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)

palette(rainbow(12))
plot(jitter(loansData$Amount.Requested), jitter(loansData$Interest.Rate), col=loansData$Employment.Length.num, pch=19, cex=0.5)
plot(jitter(loansData$Employment.Length.num), jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)
#legend("topright", legend=unique(loansData$Employment.Length.num), col=unique(as.numeric(loansData$Employment.Length.num)), pch=19, cex=0.8)
plot(jitter(loansData$Employment.Length.num), jitter(log10(loansData$Monthly.Income+1)), col="blue", pch=19, cex=0.5)
plot(jitter(loansData$Monthly.Income[which(loansData$Monthly.Income<30000)]), jitter(loansData$Interest.Rate[which(loansData$Monthly.Income<30000)]),  col="blue", pch=19, cex=0.5)

plot(jitter(loansData$FICO.Range.min), jitter(loansData$Debt.To.Income.Ratio), col="blue", pch=19, cex=0.5)
### slight relationship --> higher FICO scores, more likely to have higher Debt-to-income ratio



# SVD attempt
loansSub <- na.omit(loansData[,c(1,2,3,17,6,9,15,11,12,13,18)])
### large outlier due to entry 54487; High monthly income, so it is removed
loansSub <- loansSub[loansSub$Monthly.Income<100000,]
names(loansSub)

nam#source("http://bioconductor.org/biocLite.R")
#biocLite("impute")
#library(impute)
#loansSub <- impute.knn(loansSub)

svd1 <- svd(scale(loansSub))
plot(svd1$d^2/sum(svd1$d^2), col="blue", pch=19, cex=0.5)
svd1$d^2/sum(svd1$d^2)
plot(svd1$u[,1],col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(svd1$u[,2],col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

plot(svd1$u[,1],col=as.numeric(loansData$Home.Ownership), pch=19, cex=0.5)
plot(svd1$u[,2],col=as.numeric(loansData$Home.Ownership), pch=19, cex=0.5)


# PCA attempt
loansSub_pca <- prcomp(loansSub, center=TRUE, scale=TRUE)
summary(loansSub_pca)
loansSub_pca$rotation[,1]    # look at LOADINGs matrix
biplot(loansSub_pca, col=c("gray","red"), choices=1:2, cex=0.7)
### Plot seems to indicate that FICO.Range and Inquiries.in.the.Last.6.Months are strongly (negatively)
### correlated. There is also a decent negative correlation with Debt.To.Income.Ratio.
### longer vectors are of course Amount Requested

biplot(loansSub_pca, col=c("gray","red"), choices=2:3, cex=0.7)
### second plot (PC3vsPC2) indicates Monthly Income is negatively correlated, with
### Employment Length close by. Also, try Loan length.
### Longer vectors include FICO, Open Credit Lines,

biplot(loansSub_pca, col=c("gray","red"), choices=3:4, cex=0.7)

### Both plots indicate Amount Requested/Funded are pretty well correlated (which makes sense)

plot(loansSub_pca)
predict(loansSub_pca)[,1]
cor(loansSub$Interest.Rate, loansSub_pca$x[,1])

### What if I got rid of FICO.range and redid the PCA?
loansSub_pca2 <- prcomp(loansSub[,c(-7,-5,-10)], center=TRUE, scale=TRUE)
summary(loansSub_pca2)
loansSub_pca2$rotation[,1]    # look at LOADINGs matrix
biplot(loansSub_pca2, col=c("gray","red"), cex=0.7)
predict(loansSub_pca2)[,1]
plot(loansSub_pca2)
cor(loansSub$Interest.Rate, loansSub_pca$x[,1])

### Remove all the FICO-related entries, and Amt funded
loansSub_pca3 <- prcomp(loansSub[,c(1,3,6,11)], center=TRUE, scale=TRUE)
summary(loansSub_pca3)
loansSub_pca3$rotation[,1]    # look at LOADINGs matrix
biplot(loansSub_pca3, col=c("gray","red"), cex=0.7)
predict(loansSub_pca3)[,1]
plot(loansSub_pca3)
cor(loansSub$Interest.Rate, loansSub_pca$x[,1])

### Add factors back in (to try)
loansSub<-loansSub

### 


# Modeling
## Start with basic linear regression model, using FICO score only
loansDataLm1 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min)
summary(loansDataLm1)
plot(loansData$Interest.Rate, loansDataLm1$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

## Start with basic linear regression model, using amount only
loansDataLm2 <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested)
summary(loansDataLm2)
plot(loansData$Interest.Rate, loansDataLm2$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
# Even worse!

## Start with basic linear regression model, using amount only with log10
loansDataLm2 <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested.log10)
summary(loansDataLm2)
plot(loansData$Interest.Rate, loansDataLm2$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
# Even worse!

## add in Amount Requested
loansDataLm3 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Amount.Requested)
summary(loansDataLm3)
plot(loansData$Interest.Rate, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better

## add in Amount Requested log10
loansDataLm3 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Amount.Requested.log10)
summary(loansDataLm3)
plot(loansData$Interest.Rate, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### worse with log10

## check for confounding
plot(jitter(loansData$Amount.Requested), jitter(loansData$FICO.Range.min), col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### No obvious confounding

## add in Income
loansDataLm3 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Monthly.Income)
summary(loansDataLm3)
which(is.na(loansData$Monthly.Income))
plot(loansData$Interest.Rate, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better


## add in Loan length - numeric
loansDataLm4 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Amount.Requested + loansData$Loan.Length.months)
summary(loansDataLm4)
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better results for higher interest rates

### should probably make it into the factor version
loansDataLm4 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Amount.Requested + loansData$Loan.Length.months)
summary(loansDataLm4)
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better results for higher interest rates
loansDataLm4 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min + loansData$Amount.Requested + as.factor(loansData$Loan.Length))
summary(loansDataLm4)
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

###  amount requested and loan length confounded
### adding Employment length seems not as significant
loansDataLm4 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min 
                   + loansData$Amount.Requested 
                   + as.factor(loansData$Loan.Length) 
                   + loansData$Amount.Requested*as.factor(loansData$Loan.Length)
                   + loansData$Inquiries.in.the.Last.6.Months)
summary(loansDataLm4)
plot(loansData$Interest.Rate[c(-367,-1595)], loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

###  amount requested and loan length confounded?
### adding Employment length seems not as significant
loansDataLm4 <- lm(loansData$Interest.Rate ~ loansData$FICO.Range.min 
                   + loansData$Amount.Requested 
                   + as.factor(loansData$Loan.Length) 
                   + loansData$Monthly.Income
                   + loansData$Debt.To.Income.Ratio
                   + loansData$Inquiries.in.the.Last.6.Months)
summary(loansDataLm4)
plot(loansData$Interest.Rate[c(-367,-1595)], loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)