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
loansData$Monthly.Income.log10 <- log10(as.numeric(loansData$Monthly.Income))

### Remove NAs; only 7 entries
table(is.na(loansData))
loansData <- loansData[complete.cases(loansData),]
nrow(loansData) ## removed 2 cases

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

## convert some factor columns into numeric
loansData$State.num <- as.numeric(loansData$State)
loansData$Loan.Purpose.num <- as.numeric(loansData$Loan.Purpose)
loansData$Home.Ownership.num <- as.numeric(loansData$Home.Ownership)

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

## Check correlations

## Check Interest rate vs FICO
plot(jitter(loansData$FICO.Range.mid),
     jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)
smoothScatter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2,loansData$Interest.Rate)
### plot shows decrease in interest rates when FICO score increases
boxplot(Interest.Rate ~ FICO.Range, data=loansData)
boxplot(sqrt(Interest.Rate) ~ FICO.Range, data=loansData)

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

## try PCA with a fuller data set
loansSub2_pca <- prcomp(loansData[,c(1,2,3,19,22,6,21,23,9,18,11,12,13)], 
                        center=TRUE, scale=TRUE)
summary(loansSub2_pca)
## Not very well patterned
loansSub2_pca$rotation[,1]    # look at LOADINGs matrix
biplot(loansSub2_pca, col=c("gray","red"), choices=1:2, cex=0.7)
biplot(loansSub2_pca, col=c("gray","red"), choices=2:3, cex=0.7)



# Modeling

## Check correlations

cor(loansData$Interest.Rate, loansData[,c(1,2,19,22,6,21,23,9,18,11,12,13,20)])
### Correlation indicates the following vars are highly correlated with Interest Rate:
### FICO.Range.mid(-0.7092), Loan.Length.months(0.4235), Amount.Funded.By.Investors(0.336),
### Amount.Requested(0.331), Debt.To.Income.Ratio(0.1722), Inquiries.in.the.Last.6.Months(0.1646)


### See if some values correlate with FICO score
cor(loansData$FICO.Range.mid, loansData[,c(1,2,19,22,6,21,23,9,11,12,13,20)])
### As expected, state does not correlate well (not in FICO score)
### Best vars are
### Debt.To.Income.Ratio(-0.2169), Home.Ownership.num (-0.156), Monthly.Income(0.122)
### Oddly, Inquiries in the last 6 months is low

## Start with basic linear regression model, using FICO score only
loansDataLm1 <- lm(Interest.Rate ~ FICO.Range.min, data=loansData)
summary(loansDataLm1)
plot(loansData$Interest.Rate, loansDataLm1$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm1$fitted.values, loansDataLm1$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
shapiro.test(loansDataLm1$residuals)

## add in Amount Requested
loansDataLm3 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested, data=loansData)
summary(loansDataLm3)
plot(loansData$Interest.Rate, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm3$fitted.values, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better
par(mfrow=c(1,2))
## add in Income
loansDataLm4 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + Monthly.Income, data=loansData)
summary(loansDataLm4)
which(is.na(loansData$Monthly.Income))
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm4$fitted.values, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### better.

## add in Debt to Income Ratio
loansDataLm5 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm5)
which(is.na(loansData$Monthly.Income))
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### not better than LM4

## add in Loan Lengths (numeric)
loansDataLm5 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + Loan.Length.months, data=loansData)
summary(loansDataLm5)
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### Better R values

### should probably make it into the factor version
loansDataLm5 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + as.factor(Loan.Length), data=loansData)
summary(loansDataLm5)
plot(loansData$Interest.Rate, loansDataLm5$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### same as numeric

anova(loansDataLm1, loansDataLm3,loansDataLm5)

## add in Debt to Income Ratio -- p-value is high, no change in R, so not that great
loansDataLm6 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + as.factor(Loan.Length)  + Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm6)
plot(loansData$Interest.Rate, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

### use Inquiries instead
## Slightly better, R changed by 0.01
loansDataLm6 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + as.factor(Loan.Length)  + Inquiries.in.the.Last.6.Months, data=loansData)
summary(loansDataLm6)
plot(loansData$Interest.Rate, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm6$fitted.values, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm5, loansDataLm6)
plot(loansDataLm6,which=2)
plot(loansDataLm6)

par(mfcol=c(1,3))

### add confounding
## Slightly better, R changed by 0.01
loansDataLm7 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length)  + Inquiries.in.the.Last.6.Months + Amount.Requested*as.factor(Loan.Length), data=loansData)
summary(loansDataLm7)
plot(loansData$Interest.Rate, loansDataLm7$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm7$fitted.values, loansDataLm7$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm6, loansDataLm7)
# P value is >0.001 but <0.01, so not sure if OK
plot(loansDataLm7,which=2)
plot(loansDataLm7)
shapiro.test(loansDataLm7$residuals)

confoundlm <- lm(Amount.Requested ~ as.factor(Loan.Length), data=loansData)
summary(confoundlm)
plot(confoundlm)
confint(loansDataLm7)

### Remove Inquiries, attempt confounding
## Model 7 is better
loansDataLm8 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length)  + Amount.Requested*as.factor(Loan.Length), data=loansData)
summary(loansDataLm8)
plot(loansData$Interest.Rate, loansDataLm8$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm8$fitted.values, loansDataLm8$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm8, loansDataLm7)
# P value is >0.001 but <0.01, so not sure if OK
plot(loansDataLm8,which=2)
confint(loansDataLm8)

### Retry using FICO range as factor
loansDataLm9 <- lm(Interest.Rate ~ as.factor(FICO.Range) + Amount.Requested + as.factor(Loan.Length)  + Inquiries.in.the.Last.6.Months + Amount.Requested*as.factor(Loan.Length), data=loansData)
summary(loansDataLm9)
plot(loansData$Interest.Rate, loansDataLm9$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm9$fitted.values, loansDataLm9$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm7, loansDataLm9)
# P value is >0.001 but <0.01, so not sure if OK
plot(loansDataLm9,which=2)
plot(loansDataLm9)
confint(loansDataLm9)

### Retry using FICO range as factor
### with confounder, adjusted R-squared: of 0.796
loansDataLm10 <- lm(Interest.Rate ~ as.factor(FICO.Range) + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months, data=loansData)
summary(loansDataLm10)
plot(loansData$Interest.Rate, loansDataLm10$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm10$fitted.values, loansDataLm10$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm10, loansDataLm9)
# P value is >0.001 but <0.01, so not sure if OK
plot(loansDataLm10,which=2)
plot(loansDataLm10)
shapiro.test(loansDataLm10$residuals)  ## not better than without as.factor

### Go for broke, square root
## Slightly better, R changed by 0.01
loansDataLm11 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months + Amount.Requested*as.factor(Loan.Length), data=loansData)
summary(loansDataLm11)
plot(sqrt(loansData$Interest.Rate), loansDataLm7$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm11$fitted.values, loansDataLm7$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
anova(loansDataLm6, loansDataLm11)
# P value is >0.001 but <0.01, so not sure if OK
plot(loansDataLm7,which=2)
plot(loansDataLm7)