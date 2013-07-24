InterestRates_R
===============
* Purpose: Understanding LendingClub Interest Rates using linear regression in R.
* Result: "Decreased FICO score is associated with increased interest rate for Lending Club loans"
* For Coursera's Data Analysis course.


Introduction
------------
[Peer-to-peer lending] (http://en.wikipedia.org/wiki/Peer_to_peer_lending) is the practice of lending money to 
individuals without using conventional means such as through banks, lending companies, or other established 
financial institutions. One website, [Lending Club] (https://www.lendingclub.com/home.action), provides 
peer-to-peer lending services in the United States, allowing individuals to 
invest in peer loans through online transactions. Lending Club sets the interest rates for each loan based 
on the borrower’s creditworthiness score, credit history, employment history, and other information. 
In particular, the [FICO score] (http://www.myfico.com/CreditEducation/WhatsInYourScore.aspx), a measure of credit 
risk, is known to be a major component of the interest 
rate calculation. For the given data sample from Lending Club, we use exploratory analysis and standard multiple 
regression  techniques to show a significant association between loan interest rate and FICO score.

Methods
-------
Loans data set was provided by the Data Analysis course on Coursera; the data set originated from Lending Club, 
although not time period for the original data collection was given. The data set was downloaded from
https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda on February 4, 2013 using the 
[R programming language] (http://www.R-project.org). [Pearson correlation coefficients] 
(http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient) were calculated for the variables. 
Using the terms identified previously, a standard multivariate linear regression model was created to 
relate interest rate to FICO score range. Model selection was based on the results of exploratory analysis 
and Lending Club’s [description of interest rate assignment] 
(http://www.lendingclub.com/public/how-we-set-interest-rates.action). Model coefficients were estimated using 
ordinary 
least-squares method, and residuals were calculated using standard asymptotic approximations. The [95% 
confidence intervals] (http://en.wikipedia.org/wiki/Confidence_interval) were examined to verify the reliability 
of the estimates. The R code was put in an R markdown (.Rmd) file for user consumption.

Results
-------
The initial Lending Club data set complies with Lending Club’s 
[borrower eligibility policies] (http://www.lendingclub.com/kb/index.php?View=entry&amp;EntryID=186), in which FICO 
scores must be above 660 to qualify for loans . Loans range from $1000 to $35000, and with loan lengths of either 
36 months or 60 months and interest rates ranging from5.42% to 24.89%. 

The midpoint of each FICO score range was 
calculated to approximate the FICO score and used in further analyses (to prevent confusion, it is referred 
as “FICO score” hereafter).On examining the correlation coefficients for interest rate (Table 1), it was shown that 
besides FICO score, the 
loan length and amount requested were well-correlated with interest rate, while to a lesser extent, so are 
debt-to-income ratio and number of credit report inquiries in the last six months. 
 
**Table 1.** Pearson correlation coefficients (ρ) for loans interest rate.
<table>
<tr><th>Variable</th><th>ρ</th></tr>
<tr><td>Loan Amount Requested</td><td>0.3318</td></tr>
<tr><td>Loan Length (in months)</td><td>0.4235</td></tr>
<tr><td>Loan Purpose</td><td>-0.0607</td></tr>
<tr><td>Debt To Income Ratio</td><td>0.1722</td></tr>
<tr><td>State</td><td>0.0089</td></tr>
<tr><td>Monthly Income</td><td>0.0129</td></tr>
<tr><td>FICO score</td><td>-0.7092</td></tr>
<tr><td>Open Credit Lines</td><td>0.0903</td></tr>
<tr><td>Revolving Credit Balance</td><td>0.0611</td></tr>
<tr><td>Number of Credit Report Inquiries in the Last 6 Months</td><td>0.1646</td></tr>
<tr><td>Employment Length (in years)</td><td>0.0508</td></tr>
</table>


Adding several of these variables resulted in the final regression model:

<center>IR= β<sub>0</sub>+β<sub>1</sub> FRM+ β<sub>2</sub> AR+ β<sub>3</sub> IN+ β<sub>4</sub> D+ε</center>

* IR: 	interest rate of loan
* FRM: 	FICO score
* AR: 	loan amount requested
* IN: 	number of credit report inquiries in the last 6 months
* LL: 	loan length (either 36 months or 60 months)
* β<sub>0</sub>: 	intercept term
* β<sub>1</sub>: 	change in interest rate associated with a change of 1 point of the FICO score
* β<sub>2</sub>: 	change in interest rate associated with a change of $1 in loan amount requested
* β<sub>3</sub>: 	change in interest rate associated with an addition credit report inquiry in the last 6 months
* β<sub>0</sub>+β<sub>4</sub>: 	interest rate associated with loan lengths of 60 months
* D: 	dummy variable for loan length; =0 if 36 months, =1 if 60 months
* ε: 	error term, includes all sources of unmeasured and unmodeled random variation in interest rate

The association between interest rate and FICO score is highly significant with p<2e-16. A change of one point 
in FICO score corresponds with a interest rate percent change of β<sub>1</sub> = -0.08670 (95% confidence interval: 
-0.0890, -0.0843). The regression coefficients are summarized in Table 2; all coefficients have p<2e-16 
with 95% confidence intervals.

**Table 2.** Regression coefficients and 95% confidence intervals for the final model.
<table>
<tr><th>Regression Coefficient</th><th>Estimate</th><th>2.5%</th><th>97.5%</th></tr>
<tr><td>β0</td><td>71.6826</td><td>70.0228</td><td>73.3424</td></tr>
<tr><td>β1</td><td>-0.08670</td><td>-0.0890</td><td>-0.0844</td></tr>
<tr><td>β2</td><td>1.405e-04</td><td>0.0001</td><td>0.0002</td></tr>
<tr><td>β3</td><td>0.3319</td><td>0.2655</td><td>0.3982</td></tr>
<tr><td>β4</td><td>3.2523</td><td>3.0366</td><td>3.4681</td></tr>
</table>

The final model has an adjusted R<sup>2</sup> value of 0.7543. Further analyses by exploring possible interactions or 
adding other possible confounders did not yield much improvement in R<sup>2</sup> (<0.05 increase); these additions 
either added excessive complexity to the model or had 95% confidence intervals which include the zero point.

Conclusions 
------------
This analysis suggests that there is a significant association between Lending Club loan interest rate and 
FICO score. Other variables including the loan amount requested, the number of credit inquiries in the last 
6 months, and the loan length are confounded with both interest rate and FICO score. Adding these variables 
to the regression model improves its fit.


Additional References
---------------------
 Box, George E.P., Hunter, William G., and Hunter, J. Stuart. Statistics for Experimenters: An Introduction to Design, Data Analysis, and Model Building. John Wiley & Sons, 1976. 
