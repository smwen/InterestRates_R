Title
========================================================

Data Preprocessing
-------------------------

### Retrieve the data

```r
setwd("~/Documents/Sandy/DA_HW1/code")
loansData <- read.csv("../data/loansData.csv")
```


### Data Cleaning
####Convert columns to numeric class, when applicable.

```r
loansData$Interest.Rate <- as.numeric(sub("%", "", loansData$Interest.Rate))
loansData$Debt.To.Income.Ratio <- as.numeric(sub("%", "", loansData$Debt.To.Income.Ratio))
loansData$Amount.Requested <- as.numeric(loansData$Amount.Requested)
loansData$Amount.Funded.By.Investors <- as.numeric(loansData$Amount.Funded.By.Investors)
loansData$Monthly.Income <- as.numeric(loansData$Monthly.Income)
loansData$Open.CREDIT.Lines <- as.numeric(loansData$Open.CREDIT.Lines)
loansData$Revolving.CREDIT.Balance <- as.numeric(loansData$Revolving.CREDIT.Balance)
loansData$Inquiries.in.the.Last.6.Months <- as.numeric(loansData$Inquiries.in.the.Last.6.Months)
loansData$Monthly.Income.log10 <- log10(as.numeric(loansData$Monthly.Income))
```


####Remove rows containing NAs.

```r
table(is.na(loansData))
```

```
## 
## FALSE  TRUE 
## 37492     8
```

```r
loansData <- loansData[complete.cases(loansData), ]
nrow(loansData)  ## removed 2 cases
```

```
## [1] 2498
```


#### split FICO Range into min/max

```r
loansData$FICO.Range.min <- as.numeric(gsub("-(.*)", "", loansData$FICO.Range))
loansData$FICO.Range.max <- as.numeric(gsub("(.*)-", "", loansData$FICO.Range))
loansData$FICO.Range.mid <- (loansData$FICO.Range.max + loansData$FICO.Range.min)/2
```

#### convert loan length into numeric

```r
loansData$Loan.Length.months <- as.numeric(gsub(" months", "", loansData$Loan.Length))
```

#### convert Employment Length to numeric
Assign 0 to <1 year, 10 to 10+ years

```r
loansData$Employment.Length.num <- gsub("< 1 year", "0", loansData$Employment.Length)
loansData$Employment.Length.num <- gsub(" year(.*)", "", loansData$Employment.Length.num)
loansData$Employment.Length.num <- gsub("n/a", "", loansData$Employment.Length.num)
loansData$Employment.Length.num <- as.numeric(gsub("\\+", "", loansData$Employment.Length.num))
```

#### convert some factor columns into numeric

```r
loansData$State.num <- as.numeric(loansData$State)
loansData$Loan.Purpose.num <- as.numeric(loansData$Loan.Purpose)
loansData$Home.Ownership.num <- as.numeric(loansData$Home.Ownership)
```

#### Check loansData table


```r
head(loansData)
```

```
##       Amount.Requested Amount.Funded.By.Investors Interest.Rate
## 81174            20000                      20000          8.90
## 99592            19200                      19200         12.12
## 80059            35000                      35000         21.98
## 15825            10000                       9975          9.99
## 33182            12000                      12000         11.71
## 62403             6000                       6000         15.31
##       Loan.Length       Loan.Purpose Debt.To.Income.Ratio State
## 81174   36 months debt_consolidation                14.90    SC
## 99592   36 months debt_consolidation                28.36    TX
## 80059   60 months debt_consolidation                23.81    CA
## 15825   36 months debt_consolidation                14.30    KS
## 33182   36 months        credit_card                18.78    NJ
## 62403   36 months              other                20.05    CT
##       Home.Ownership Monthly.Income FICO.Range Open.CREDIT.Lines
## 81174       MORTGAGE           6542    735-739                14
## 99592       MORTGAGE           4583    715-719                12
## 80059       MORTGAGE          11500    690-694                14
## 15825       MORTGAGE           3833    695-699                10
## 33182           RENT           3195    695-699                11
## 62403            OWN           4892    670-674                17
##       Revolving.CREDIT.Balance Inquiries.in.the.Last.6.Months
## 81174                    14272                              2
## 99592                    11140                              1
## 80059                    21977                              1
## 15825                     9346                              0
## 33182                    14469                              0
## 62403                    10391                              2
##       Employment.Length Monthly.Income.log10 FICO.Range.min FICO.Range.max
## 81174          < 1 year                3.816            735            739
## 99592           2 years                3.661            715            719
## 80059           2 years                4.061            690            694
## 15825           5 years                3.584            695            699
## 33182           9 years                3.504            695            699
## 62403           3 years                3.689            670            674
##       FICO.Range.mid Loan.Length.months Employment.Length.num State.num
## 81174            737                 36                     0        37
## 99592            717                 36                     2        39
## 80059            692                 60                     2         5
## 15825            697                 36                     5        16
## 33182            697                 36                     9        28
## 62403            672                 36                     3         7
##       Loan.Purpose.num Home.Ownership.num
## 81174                3                  1
## 99592                3                  1
## 80059                3                  1
## 15825                3                  1
## 33182                2                  5
## 62403               10                  4
```

```r
names(loansData)
```

```
##  [1] "Amount.Requested"               "Amount.Funded.By.Investors"    
##  [3] "Interest.Rate"                  "Loan.Length"                   
##  [5] "Loan.Purpose"                   "Debt.To.Income.Ratio"          
##  [7] "State"                          "Home.Ownership"                
##  [9] "Monthly.Income"                 "FICO.Range"                    
## [11] "Open.CREDIT.Lines"              "Revolving.CREDIT.Balance"      
## [13] "Inquiries.in.the.Last.6.Months" "Employment.Length"             
## [15] "Monthly.Income.log10"           "FICO.Range.min"                
## [17] "FICO.Range.max"                 "FICO.Range.mid"                
## [19] "Loan.Length.months"             "Employment.Length.num"         
## [21] "State.num"                      "Loan.Purpose.num"              
## [23] "Home.Ownership.num"
```

```r
table(is.na(loansData))  # 77 NAs
```

```
## 
## FALSE  TRUE 
## 57377    77
```

```r
summary(loansData)
```

```
##  Amount.Requested Amount.Funded.By.Investors Interest.Rate  
##  Min.   : 1000    Min.   :    0              Min.   : 5.42  
##  1st Qu.: 6000    1st Qu.: 6000              1st Qu.:10.16  
##  Median :10000    Median :10000              Median :13.11  
##  Mean   :12413    Mean   :12009              Mean   :13.07  
##  3rd Qu.:17000    3rd Qu.:16000              3rd Qu.:15.80  
##  Max.   :35000    Max.   :35000              Max.   :24.89  
##                                                             
##     Loan.Length               Loan.Purpose  Debt.To.Income.Ratio
##  36 months:1950   debt_consolidation:1307   Min.   : 0.00       
##  60 months: 548   credit_card       : 444   1st Qu.: 9.75       
##                   other             : 199   Median :15.32       
##                   home_improvement  : 152   Mean   :15.39       
##                   major_purchase    : 101   3rd Qu.:20.68       
##                   small_business    :  87   Max.   :34.91       
##                   (Other)           : 208                       
##      State       Home.Ownership Monthly.Income     FICO.Range  
##  CA     : 433   MORTGAGE:1148   Min.   :   588   670-674: 171  
##  NY     : 253   NONE    :   0   1st Qu.:  3500   675-679: 166  
##  TX     : 174   OTHER   :   5   Median :  5000   680-684: 157  
##  FL     : 169   OWN     : 200   Mean   :  5685   695-699: 153  
##  IL     : 101   RENT    :1145   3rd Qu.:  6800   665-669: 145  
##  GA     :  98                   Max.   :102750   690-694: 140  
##  (Other):1270                                    (Other):1566  
##  Open.CREDIT.Lines Revolving.CREDIT.Balance Inquiries.in.the.Last.6.Months
##  Min.   : 2.0      Min.   :     0           Min.   :0.000                 
##  1st Qu.: 7.0      1st Qu.:  5586           1st Qu.:0.000                 
##  Median : 9.0      Median : 10962           Median :0.000                 
##  Mean   :10.1      Mean   : 15245           Mean   :0.906                 
##  3rd Qu.:13.0      3rd Qu.: 18889           3rd Qu.:1.000                 
##  Max.   :38.0      Max.   :270800           Max.   :9.000                 
##                                                                           
##  Employment.Length Monthly.Income.log10 FICO.Range.min FICO.Range.max
##  10+ years:653     Min.   :2.77         Min.   :640    Min.   :644   
##  < 1 year :248     1st Qu.:3.54         1st Qu.:680    1st Qu.:684   
##  2 years  :244     Median :3.70         Median :700    Median :704   
##  3 years  :235     Mean   :3.69         Mean   :706    Mean   :710   
##  5 years  :202     3rd Qu.:3.83         3rd Qu.:725    3rd Qu.:729   
##  4 years  :192     Max.   :5.01         Max.   :830    Max.   :834   
##  (Other)  :724                                                       
##  FICO.Range.mid Loan.Length.months Employment.Length.num   State.num   
##  Min.   :642    Min.   :36.0       Min.   : 0.00         Min.   : 1.0  
##  1st Qu.:682    1st Qu.:36.0       1st Qu.: 2.00         1st Qu.: 7.0  
##  Median :702    Median :36.0       Median : 5.00         Median :20.0  
##  Mean   :708    Mean   :41.3       Mean   : 5.39         Mean   :21.1  
##  3rd Qu.:727    3rd Qu.:36.0       3rd Qu.:10.00         3rd Qu.:32.0  
##  Max.   :832    Max.   :60.0       Max.   :10.00         Max.   :46.0  
##                                    NA's   :77                          
##  Loan.Purpose.num Home.Ownership.num
##  Min.   : 1.00    Min.   :1.00      
##  1st Qu.: 3.00    1st Qu.:1.00      
##  Median : 3.00    Median :4.00      
##  Mean   : 4.37    Mean   :3.08      
##  3rd Qu.: 5.00    3rd Qu.:5.00      
##  Max.   :14.00    Max.   :5.00      
## 
```

 requested amounts are between 1000-35000. 

WHy is there a -0.01 min value for amount funded?
Interest rates range from 5.42 to 24.89
2 loan lengths: 36 months, 60 months
Debt to Income Ratio ranges from 0 to 34.91
FICO ranges from 640 to 834

#### Find missing values, check ranges
table(complete.cases(loansData))

Exploratory Analysis
-------------------------
### Check Interest rate vs FICO

```r
plot(jitter(loansData$FICO.Range.mid), jitter(loansData$Interest.Rate), col = "blue", 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) 

```r
smoothScatter((loansData$FICO.Range.min + loansData$FICO.Range.max)/2, loansData$Interest.Rate)
```

```
## KernSmooth 2.23 loaded Copyright M. P. Wand 1997-2009
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) 

```r
# plot shows decrease in interest rates when FICO score increases
boxplot(Interest.Rate ~ FICO.Range, data = loansData)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-93.png) 

### try alternate colorings based on Loan.purpose, home.ownership, etc.
see if anything significant about home ownership

```r
plot(jitter((loansData$FICO.Range.min + loansData$FICO.Range.max)/2), jitter(loansData$Interest.Rate), 
    col = as.numeric(loansData$Home.Ownership), pch = 19, cex = 0.4)
legend("topright", legend = unique(loansData$Home.Ownership), col = unique(as.numeric(loansData$Home.Ownership)), 
    pch = 19)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-101.png) 

```r
plot(density(loansData$FICO.Range.mid), lwd = 3, col = "red")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-102.png) 


#### Check FICO-related parameters
FICO scores are calculated using revolving credit balance, inquiries in the last 12 months (not 6),
open credit lines

```r
plot(jitter(loansData$FICO.Range.min), jitter(loansData$Inquiries.in.the.Last.6.Months), 
    col = "blue", pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


### Loan Length
As of this writing, Lending Club only allows 36-month terms for loans of $1000-$15,975,
so naturally larger loan lengths will tend to be for higher amounts.

```r
plot(jitter(loansData$FICO.Range.min), jitter(loansData$Amount.Requested), col = as.numeric(loansData$Loan.Length), 
    pch = 19, cex = 0.5)
legend("topright", legend = unique(loansData$Loan.Length), col = unique(as.numeric(loansData$Loan.Length)), 
    pch = 19, cex = 0.9)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-121.png) 

```r
plot(jitter(loansData$Amount.Requested), jitter(loansData$Interest.Rate), col = loansData$Employment.Length.num, 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-122.png) 

```r
plot(jitter(loansData$FICO.Range.min), jitter(loansData$Debt.To.Income.Ratio), 
    col = "blue", pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-123.png) 

Slight relationship --> higher FICO scores, more likely to have higher Debt-to-income ratio

### Correlations
#### Check Interest Rates

```r
cor(loansData$Interest.Rate, loansData[, c(1, 2, 19, 22, 6, 21, 23, 9, 18, 11, 
    12, 13, 20)])
```

```
##      Amount.Requested Amount.Funded.By.Investors Loan.Length.months
## [1,]           0.3318                     0.3368             0.4235
##      Loan.Purpose.num Debt.To.Income.Ratio State.num Home.Ownership.num
## [1,]         -0.06073               0.1722  0.008888            0.07503
##      Monthly.Income FICO.Range.mid Open.CREDIT.Lines
## [1,]        0.01292        -0.7092           0.09031
##      Revolving.CREDIT.Balance Inquiries.in.the.Last.6.Months
## [1,]                  0.06111                         0.1646
##      Employment.Length.num
## [1,]                    NA
```

Correlation indicates the following vars are highly correlated with Interest Rate:
FICO.Range.mid(-0.7092), Loan.Length.months(0.4235), Amount.Funded.By.Investors(0.336),
Amount.Requested(0.331), Debt.To.Income.Ratio(0.1722), Inquiries.in.the.Last.6.Months(0.1646)

### Check FICO score

```r
cor(loansData$FICO.Range.mid, loansData[, c(1, 2, 19, 22, 6, 21, 23, 9, 11, 
    12, 13, 20)])
```

```
##      Amount.Requested Amount.Funded.By.Investors Loan.Length.months
## [1,]          0.08338                    0.07428             0.0127
##      Loan.Purpose.num Debt.To.Income.Ratio State.num Home.Ownership.num
## [1,]          0.09926               -0.217  0.001387            -0.1566
##      Monthly.Income Open.CREDIT.Lines Revolving.CREDIT.Balance
## [1,]         0.1227          -0.08944                 0.002917
##      Inquiries.in.the.Last.6.Months Employment.Length.num
## [1,]                       -0.09217                    NA
```

As expected, state does not correlate well (not in FICO score)
Best vars are
Debt.To.Income.Ratio(-0.2169), Home.Ownership.num (-0.156), Monthly.Income(0.122)
Oddly, Inquiries in the last 6 months is low

Modeling
-------------------------
### Start with basic linear regression model, using FICO score only

```r
palette(rainbow(14))
loansDataLm1 <- lm(Interest.Rate ~ FICO.Range.min, data = loansData)
summary(loansDataLm1)
```

```
## 
## Call:
## lm(formula = Interest.Rate ~ FICO.Range.min, data = loansData)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -7.990 -2.136 -0.456  1.835 10.194 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    72.83876    1.19066    61.2   <2e-16 ***
## FICO.Range.min -0.08467    0.00168   -50.3   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 2.95 on 2496 degrees of freedom
## Multiple R-squared: 0.503,	Adjusted R-squared: 0.503 
## F-statistic: 2.53e+03 on 1 and 2496 DF,  p-value: <2e-16
```

```r
par(mfrow = c(1, 2))
plot(loansData$Interest.Rate, loansDataLm1$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
legend("bottomright", legend = unique(loansData$Loan.Purpose), col = as.numeric(unique(loansData$Loan.Purpose)), 
    pch = 19, cex = 0.6)
plot(loansDataLm1$fitted.values, loansDataLm1$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

### add in Loan Lengths
should probably make it into the factor version

```r
loansDataLm2 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + as.factor(Loan.Length), 
    data = loansData)
summary(loansDataLm2)
```

```
## 
## Call:
## lm(formula = Interest.Rate ~ FICO.Range.min + Amount.Requested + 
##     as.factor(Loan.Length), data = loansData)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.761 -1.457 -0.132  1.262 10.295 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      7.26e+01   8.53e-01    85.1   <2e-16 ***
## FICO.Range.min                  -8.77e-02   1.21e-03   -72.4   <2e-16 ***
## Amount.Requested                 1.39e-04   5.96e-06    23.2   <2e-16 ***
## as.factor(Loan.Length)60 months  3.29e+00   1.12e-01    29.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 2.11 on 2494 degrees of freedom
## Multiple R-squared: 0.745,	Adjusted R-squared: 0.745 
## F-statistic: 2.43e+03 on 3 and 2494 DF,  p-value: <2e-16
```

```r
par(mfrow = c(1, 2))
plot(loansData$Interest.Rate, loansDataLm2$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
plot(loansDataLm2$fitted.values, loansDataLm2$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 


### add in Inquiries instead
Slightly better, R changed by 0.01

```r
loansDataLm3 <- lm(Interest.Rate ~ FICO.Range.min + Amount.Requested + as.factor(Loan.Length) + 
    Inquiries.in.the.Last.6.Months, data = loansData)
summary(loansDataLm3)
```

```
## 
## Call:
## lm(formula = Interest.Rate ~ FICO.Range.min + Amount.Requested + 
##     as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months, 
##     data = loansData)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.405 -1.389 -0.162  1.231  9.888 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      7.15e+01   8.44e-01   84.72   <2e-16 ***
## FICO.Range.min                  -8.67e-02   1.19e-03  -72.63   <2e-16 ***
## Amount.Requested                 1.41e-04   5.86e-06   24.00   <2e-16 ***
## as.factor(Loan.Length)60 months  3.25e+00   1.10e-01   29.56   <2e-16 ***
## Inquiries.in.the.Last.6.Months   3.32e-01   3.38e-02    9.81   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 2.07 on 2493 degrees of freedom
## Multiple R-squared: 0.755,	Adjusted R-squared: 0.754 
## F-statistic: 1.92e+03 on 4 and 2493 DF,  p-value: <2e-16
```

```r
par(mfrow = c(1, 2))
plot(loansData$Interest.Rate, loansDataLm3$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
plot(loansDataLm3$fitted.values, loansDataLm3$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-171.png) 

```r
par(mfrow = c(1, 1))
plot(loansDataLm3, which = 2)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-172.png) 

```r
confint(loansDataLm3)
```

```
##                                      2.5 %    97.5 %
## (Intercept)                     69.8540691 73.164334
## FICO.Range.min                  -0.0890403 -0.084359
## Amount.Requested                 0.0001291  0.000152
## as.factor(Loan.Length)60 months  3.0365787  3.468072
## Inquiries.in.the.Last.6.Months   0.2655349  0.398269
```

### Check confounding

```r
confoundlm <- lm(Amount.Requested ~ as.factor(Loan.Length), data = loansData)
summary(confoundlm)
```

```
## 
## Call:
## lm(formula = Amount.Requested ~ as.factor(Loan.Length), data = loansData)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -15979  -5208   -708   4292  24292 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        10708        161    66.5   <2e-16 ***
## as.factor(Loan.Length)60 months     7771        344    22.6   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 7110 on 2496 degrees of freedom
## Multiple R-squared: 0.17,	Adjusted R-squared: 0.17 
## F-statistic:  511 on 1 and 2496 DF,  p-value: <2e-16
```


### Retry using FICO range as factor
with confounder, adjusted R-squared: of 0.796

```r
loansDataLm4 <- lm(Interest.Rate ~ as.factor(FICO.Range) + Amount.Requested + 
    as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months, data = loansData)
summary(loansDataLm4)
```

```
## 
## Call:
## lm(formula = Interest.Rate ~ as.factor(FICO.Range) + Amount.Requested + 
##     as.factor(Loan.Length) + Inquiries.in.the.Last.6.Months, 
##     data = loansData)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.290 -1.183 -0.109  1.044 10.087 
## 
## Coefficients:
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      1.38e+01   8.49e-01   16.29  < 2e-16 ***
## as.factor(FICO.Range)645-649    -9.25e-01   1.38e+00   -0.67  0.50369    
## as.factor(FICO.Range)650-654     2.73e-01   2.07e+00    0.13  0.89533    
## as.factor(FICO.Range)655-659    -8.25e-01   1.27e+00   -0.65  0.51632    
## as.factor(FICO.Range)660-664     1.72e+00   8.65e-01    1.99  0.04695 *  
## as.factor(FICO.Range)665-669     1.11e+00   8.62e-01    1.29  0.19795    
## as.factor(FICO.Range)670-674     4.01e-02   8.60e-01    0.05  0.96280    
## as.factor(FICO.Range)675-679    -6.28e-01   8.61e-01   -0.73  0.46561    
## as.factor(FICO.Range)680-684    -1.40e+00   8.61e-01   -1.63  0.10402    
## as.factor(FICO.Range)685-689    -1.98e+00   8.63e-01   -2.30  0.02152 *  
## as.factor(FICO.Range)690-694    -2.13e+00   8.64e-01   -2.47  0.01374 *  
## as.factor(FICO.Range)695-699    -2.71e+00   8.62e-01   -3.14  0.00169 ** 
## as.factor(FICO.Range)700-704    -3.44e+00   8.64e-01   -3.98  7.2e-05 ***
## as.factor(FICO.Range)705-709    -4.16e+00   8.64e-01   -4.82  1.5e-06 ***
## as.factor(FICO.Range)710-714    -4.34e+00   8.67e-01   -5.00  6.0e-07 ***
## as.factor(FICO.Range)715-719    -5.37e+00   8.71e-01   -6.17  8.0e-10 ***
## as.factor(FICO.Range)720-724    -5.64e+00   8.67e-01   -6.51  9.1e-11 ***
## as.factor(FICO.Range)725-729    -6.03e+00   8.70e-01   -6.93  5.4e-12 ***
## as.factor(FICO.Range)730-734    -6.80e+00   8.71e-01   -7.81  8.3e-15 ***
## as.factor(FICO.Range)735-739    -7.32e+00   8.81e-01   -8.31  < 2e-16 ***
## as.factor(FICO.Range)740-744    -7.31e+00   8.88e-01   -8.24  2.9e-16 ***
## as.factor(FICO.Range)745-749    -7.31e+00   8.87e-01   -8.24  2.7e-16 ***
## as.factor(FICO.Range)750-754    -7.80e+00   8.82e-01   -8.84  < 2e-16 ***
## as.factor(FICO.Range)755-759    -8.00e+00   8.93e-01   -8.96  < 2e-16 ***
## as.factor(FICO.Range)760-764    -7.82e+00   8.93e-01   -8.76  < 2e-16 ***
## as.factor(FICO.Range)765-769    -8.41e+00   9.05e-01   -9.29  < 2e-16 ***
## as.factor(FICO.Range)770-774    -9.29e+00   9.65e-01   -9.63  < 2e-16 ***
## as.factor(FICO.Range)775-779    -8.26e+00   9.40e-01   -8.79  < 2e-16 ***
## as.factor(FICO.Range)780-784    -8.99e+00   9.20e-01   -9.77  < 2e-16 ***
## as.factor(FICO.Range)785-789    -8.57e+00   9.53e-01   -8.99  < 2e-16 ***
## as.factor(FICO.Range)790-794    -9.07e+00   9.48e-01   -9.57  < 2e-16 ***
## as.factor(FICO.Range)795-799    -9.00e+00   9.98e-01   -9.02  < 2e-16 ***
## as.factor(FICO.Range)800-804    -8.79e+00   1.01e+00   -8.71  < 2e-16 ***
## as.factor(FICO.Range)805-809    -9.44e+00   1.02e+00   -9.22  < 2e-16 ***
## as.factor(FICO.Range)810-814    -8.38e+00   1.08e+00   -7.74  1.5e-14 ***
## as.factor(FICO.Range)815-819    -9.10e+00   1.15e+00   -7.92  3.5e-15 ***
## as.factor(FICO.Range)820-824    -7.03e+00   2.07e+00   -3.39  0.00071 ***
## as.factor(FICO.Range)830-834    -9.14e+00   2.08e+00   -4.40  1.1e-05 ***
## Amount.Requested                 1.45e-04   5.41e-06   26.86  < 2e-16 ***
## as.factor(Loan.Length)60 months  3.23e+00   1.01e-01   31.91  < 2e-16 ***
## Inquiries.in.the.Last.6.Months   3.27e-01   3.14e-02   10.44  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 1.89 on 2457 degrees of freedom
## Multiple R-squared: 0.798,	Adjusted R-squared: 0.795 
## F-statistic:  243 on 40 and 2457 DF,  p-value: <2e-16
```

```r
par(mfrow = c(1, 2))
plot(loansData$Interest.Rate, loansDataLm4$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
plot(loansDataLm4$fitted.values, loansDataLm4$residuals, col = as.numeric(loansData$Loan.Purpose), 
    pch = 19, cex = 0.5)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-191.png) 

```r
# P value is >0.001 but <0.01, so not sure if OK
par(mfrow = c(1, 1))
plot(loansDataLm4, which = 2)
```

```
## Warning: Not plotting observations with leverage one: 38, 1662, 2472
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-192.png) 

