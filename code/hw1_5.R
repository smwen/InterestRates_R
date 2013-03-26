# Setup file

##setwd("~/Users/SMW/Dropbox/Coursera/DA_012213/Homework/HW1/code")
setwd("~/Documents/Sandy/DA_HW1/code")
### load loansData from RDA
loansData <- read.csv("../data/loansData.csv")

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

### Remove NAs
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
table(is.na(loansData)) # 77 NAs
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

## Check distributions
par(mfrow=c(2,1))
hist(loansData$FICO.Range.mid)
hist(log10(loansData$FICO.Range.mid))
table(loansData$FICO.Range)
table(loansData$Loan.Length)

## Check Interest rate vs FICO
par(mfrow=c(1,1))
plot(jitter(loansData$FICO.Range.mid),
     jitter(loansData$Interest.Rate), col="blue", pch=19, cex=0.5)
smoothScatter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2,loansData$Interest.Rate)
### plot shows decrease in interest rates when FICO score increases
boxplot(Interest.Rate ~ FICO.Range, data=loansData)

# try alternate colorings based on Loan.purpose, home.ownership, etc.
### see if anything significant about home ownership
### Mortgage, rent, own, other, none
plot(jitter((loansData$FICO.Range.min+loansData$FICO.Range.max)/2),
     jitter(loansData$Interest.Rate), col=as.numeric(loansData$Home.Ownership), pch=19, cex=0.4)
legend("topright", legend=unique(loansData$Home.Ownership), 
       col=unique(as.numeric(loansData$Home.Ownership)), pch=19)

plot(density(loansData$FICO.Range.mid), lwd=3, col="red")

## Check FICO-related parameters
### FICO scores are calculated using revolving credit balance, inquiries in the last 12 months (not 6),
### open credit lines
plot(jitter(loansData$FICO.Range.min),jitter(loansData$Inquiries.in.the.Last.6.Months), col="blue", pch=19, cex=0.5)

## Loan Length
### As of this writing, Lending Club only allows 36-month terms for loans of $1000-$15,975,
### so naturally larger loan lengths will tend to be for higher amounts.
plot(jitter(loansData$FICO.Range.min),jitter(loansData$Amount.Requested), col=as.numeric(loansData$Loan.Length), pch=19, cex=0.5)
legend("topright", legend=unique(loansData$Loan.Length), col=unique(as.numeric(loansData$Loan.Length)), pch=19, cex=0.9)

plot(jitter(loansData$Amount.Requested), jitter(loansData$Interest.Rate), col=loansData$Employment.Length.num, pch=19, cex=0.5)
plot(jitter(loansData$FICO.Range.min), jitter(loansData$Debt.To.Income.Ratio), col="blue", pch=19, cex=0.5)
### slight relationship --> higher FICO scores, more likely to have higher Debt-to-income ratio

# Modeling

## Check correlations
cor(loansData$Interest.Rate, loansData[,c(1,2,19,22,6,21,23,9,18,11,12,13,20)])
### Correlation indicates the following vars are highly correlated with Interest Rate:
### FICO.Range.mid(-0.7092), Loan.Length.months(0.4235), Amount.Funded.By.Investors(0.336),
### Amount.Requested(0.331), Debt.To.Income.Ratio(0.1722), Inquiries.in.the.Last.6.Months(0.1646)

loansDataCor <- cor(loansData[,c(1,2,3,19,22,6,21,23,9,18,11,12,13)])
loansDataCor

### See if some values correlate with FICO score
cor(loansData$FICO.Range.mid, loansData[,c(1,2,19,22,6,21,23,9,11,12,13,20)])
### As expected, state does not correlate well (not in FICO score)
### Best vars are
### Debt.To.Income.Ratio(-0.2169), Home.Ownership.num (-0.156), Monthly.Income(0.122)
### Oddly, Inquiries in the last 6 months is low

## Start with basic linear regression model, using FICO score only
palette(rainbow(14))
loansDataLm1 <- lm(Interest.Rate ~ FICO.Range.mid, data=loansData)
summary(loansDataLm1)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm1$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm1$fitted.values, loansDataLm1$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

## add in Amount requested
loansDataLm2 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested, data=loansData)
summary(loansDataLm2)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm2$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm2$fitted.values, loansDataLm2$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### same as numeric

## add in Loan Lengths as factor
loansDataLm3 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length), data=loansData)
summary(loansDataLm3)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm3$fitted.values, loansDataLm3$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### same as numeric

### add in Inquiries instead
## Slightly better, R changed by 0.01
loansDataLm4 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length)  + Inquiries.in.the.Last.6.Months, data=loansData)
summary(loansDataLm4)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm4$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm4$fitted.values, loansDataL43$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
legend("bottom",legend=unique(loansData$Loan.Purpose),col=as.numeric(unique(loansData$Loan.Purpose)),pch=19, cex=0.7)
par(mfrow=c(1,1))
plot(loansDataLm3,which=2)
confint(loansDataLm3)

## Check confounding
confoundlm <- lm(Amount.Requested ~ as.factor(Loan.Length), data=loansData)
summary(confoundlm)

### Retry using FICO range as factor
### with confounder, adjusted R-squared: of 0.796
loansDataLm21 <- lm(Interest.Rate ~ as.factor(FICO.Range) + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months, data=loansData)
summary(loansDataLm4)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm21$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm21$fitted.values, loansDataLm21$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
# P value is >0.001 but <0.01, so not sure i OK
par(mfrow=c(1,1))
plot(loansDataLm4,which=2)

## add in Loan Lengths log10 FICO
loansDataLm22 <- lm(Interest.Rate ~ log10(FICO.Range.mid) + Amount.Requested + as.factor(Loan.Length), data=loansData)
summary(loansDataLm22)
anova(loansDataLm3, loansDataLm22)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm5$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm5$fitted.values, loansDataLm5$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
### same as numeric

##
loansDataLm5 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months + Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm5)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm5$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm5$fitted.values, loansDataLm5$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)

## Debt Ratio doesn't tell us much
loansDataLm6 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm6)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm6$fitted.values, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm4, loansDataLm5)
anova(loansDataLm3, loansDataLm6)

## Try interaction
loansDataLm7 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Amount.Requested * as.factor(Loan.Length), data=loansData)
summary(loansDataLm7)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm6$fitted.values, loansDataLm6$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm7)
### Low F and high p-value, so not real significance
### same as numeric

##
loansDataLm8 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + FICO.Range.mid*Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm8)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm8$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm8$fitted.values, loansDataLm8$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm8)
### seems OK actually

## add in 
loansDataLm9 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months + FICO.Range.mid *Debt.To.Income.Ratio, data=loansData)
summary(loansDataLm9)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm9$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm9$fitted.values, loansDataLm9$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm4, loansDataLm9)
confint(loansDataLm9)
### This is probably the best one so far


## add in 
loansDataLm10 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months + FICO.Range.mid * Debt.To.Income.Ratio*Inquiries.in.the.Last.6.Months, data=loansData)
summary(loansDataLm10)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm10$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm10$fitted.values, loansDataLm10$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm4, loansDataLm10)
confint(loansDataLm10)   ### Inquiries.in.the.Last.6.Months fails

## add in 
loansDataLm11 <- lm(Interest.Rate ~ FICO.Range.mid + Amount.Requested + as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months + FICO.Range.mid *Debt.To.Income.Ratio*Inquiries.in.the.Last.6.Months*Home.Ownership.num, data=loansData)
summary(loansDataLm9)
par(mfrow=c(1,2))
plot(loansData$Interest.Rate, loansDataLm11$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
plot(loansDataLm11$fitted.values, loansDataLm11$residuals, col=as.numeric(loansData$Loan.Purpose), pch=19, cex=0.5)
anova(loansDataLm3, loansDataLm4, loansDataLm10, loansDataLm11)
confint(loansDataLm11)