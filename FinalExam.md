Econometrics B2000 Final Exam
================
Patrick Sinclair

#### Question 1

``` r
# 1a
pnorm(1.65, 1, 6.5, lower.tail=TRUE)
```

    ## [1] 0.5398278

``` r
# 1b
2*pnorm(13.67, 8, 2.7, lower.tail = FALSE)
```

    ## [1] 0.03572884

``` r
# 1c
2*pnorm(-5.4, -11, 4, lower.tail = FALSE)
```

    ## [1] 0.1615133

``` r
# 1d
qnorm((0.158/2), 14, 7.4)
```

    ## [1] 3.552457

``` r
qnorm((0.158/2), 14, 7.4, lower.tail = FALSE)
```

    ## [1] 24.44754

``` r
# 1e
pt(6.56/4.1, 24, lower.tail = FALSE)
```

    ## [1] 0.06134072

``` r
# 1f
pt(-0.24/0.4, 4, lower.tail= TRUE)
```

    ## [1] 0.2904206

#### Question 2

``` r
# Difference in Means
dm <- 0.000145-(-0.0210)
dm
```

    ## [1] 0.021145

``` r
s1 <- 0.0213^2
s2 <- 0.271^2
Test_t <- dm/(sqrt((s1/289)+(s2/174)))
#t-stat
Test_t
```

    ## [1] 1.027323

``` r
#p-value
2*pt(Test_t, 173, lower.tail=FALSE)
```

    ## [1] 0.3057028

There is not a statistically significant difference in the mean of
average daily returns of crude oil.  
\#\#\#\# Question 3

``` r
p1 <- 90/10000
p2 <- 15/10000
pp <- 105/20000
dp <- p1-p2
z <- dp/sqrt((pp*(1-pp))*((1/10000)+(1/10000)))
# z-score
z
```

    ## [1] 7.33854

``` r
# p-value
pnorm(z, lower.tail=FALSE)
```

    ## [1] 1.079685e-13

There is enough evidence at the 99% confidence level to reject the null
hypothesis that p1 - p2 = 0. \#\#\#\# Question 4

``` r
attach(acs2017_ny)
use_varb <- (AGE >= 18) & (AGE <= 65)
dat_use <- subset(acs2017_ny, use_varb)
detach()
```

The subgroup I’ve selected is those between the ages of 18 and 65. I’ve
selected this age range as it comprises recent high school graduates,
looking to enter the work force, perhaps full time or part time to
supplement college studies, those in prime working age and people on the
cusp of retirement. Perhaps those at the upper end of the range are
scaling back their working lives or have already retired. \#\#\#\#
Question 5

``` r
attach(dat_use)
# Sample Stats
mean(UHRSWORK) # overall mean UHRSWORK
```

    ## [1] 29.46035

``` r
menavg <- mean(UHRSWORK[female == 0])
menavg
```

    ## [1] 32.40485

``` r
sdm <- sd(UHRSWORK[female == 0])
mobs <- sum(female == 0)
womenavg <- mean(UHRSWORK[female == 1])
womenavg
```

    ## [1] 26.65248

``` r
fobs <- sum(female == 1)
sdf <- sd(UHRSWORK[female == 1])
diffavg <- menavg-womenavg
# Difference in Means
tstat <- diffavg/(sqrt((sdm^2/mobs)+(sdf^2/fobs)))
#t-stat
tstat
```

    ## [1] 52.17525

``` r
#p-value
2*pt(tstat, 60433, lower.tail=FALSE)
```

    ## [1] 0

There is a statistically significant difference in the average number of
hours worked per week, between mean and women.

``` r
# Men Average Wage
mean(INCWAGE[female == 0])
```

    ## [1] 50730.17

``` r
# Women Average Wage
mean(INCWAGE[female == 1])
```

    ## [1] 33473.05

``` r
# Average Family Size
mean(FAMSIZE)
```

    ## [1] 2.939342

``` r
# Number of Full Time Workers
ftwork <- subset(dat_use, UHRSWORK >= 35)
length(ftwork$UHRSWORK)
```

    ## [1] 73570

``` r
# Number of Part Time Workers
ptwork <- subset(dat_use, ((UHRSWORK > 0) & (UHRSWORK < 35)))
length(ptwork$UHRSWORK)
```

    ## [1] 21357

``` r
detach()
```

N.B., number of part time workers does not include those working 0
hours.  
\#\#\#\# Question 6 For the OLS estimation, I have selected Age, gender,
education levels and family total income. They seem to meet the criteria
of exogeneity though there may be some correlation between level of
education and family total income. Including a polynomial in Age may not
be as important, due to the limited scale of hours worked. Interactions
may be a more useful tool to employ in this regression. I have left out
the no high school education dummy variable, in order to see how
increases in levels of education effect working hours.

``` r
hrsOLS <- lm(UHRSWORK ~ AGE + female + educ_advdeg + educ_college + educ_hs + educ_somecoll + FTOTINC, data = dat_use)
summary(hrsOLS)
```

    ## 
    ## Call:
    ## lm(formula = UHRSWORK ~ AGE + female + educ_advdeg + educ_college + 
    ##     educ_hs + educ_somecoll + FTOTINC, data = dat_use)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -68.771 -14.814   5.376  12.123  80.935 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    2.478e+01  2.589e-01  95.714  < 2e-16 ***
    ## AGE           -2.997e-02  3.889e-03  -7.707  1.3e-14 ***
    ## female        -7.097e+00  1.066e-01 -66.548  < 2e-16 ***
    ## educ_advdeg    1.513e+01  2.397e-01  63.117  < 2e-16 ***
    ## educ_college   1.277e+01  2.238e-01  57.064  < 2e-16 ***
    ## educ_hs        5.754e+00  2.123e-01  27.106  < 2e-16 ***
    ## educ_somecoll  8.179e+00  2.214e-01  36.947  < 2e-16 ***
    ## FTOTINC        1.805e-05  4.416e-07  40.876  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.06 on 115846 degrees of freedom
    ##   (7955 observations deleted due to missingness)
    ## Multiple R-squared:  0.1121, Adjusted R-squared:  0.112 
    ## F-statistic:  2089 on 7 and 115846 DF,  p-value: < 2.2e-16

The estimates do seem plausible. Age has a negative correlation with the
hours worked, which may be explained by larger numbers of people
retiring as they get older than young people who elect not to work while
studying. Increased levels of education have an increasingly large
positive correlation with hours worked. The gender variable indicates
that the female condition has a negative correlation with hours worked.
Total family income has a very small positive correlation. All of the
statistics are significant, however the model has a small R^2 and is
likely missing some key variables that would explain better the number
of hours worked.

``` r
library(AER)
```

    ## Loading required package: car

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: sandwich

    ## Loading required package: survival

``` r
linearHypothesis(hrsOLS, "educ_advdeg + educ_hs + educ_somecoll + educ_college = 0", test = "F")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## educ_advdeg  + educ_college  + educ_hs  + educ_somecoll = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: UHRSWORK ~ AGE + female + educ_advdeg + educ_college + educ_hs + 
    ##     educ_somecoll + FTOTINC
    ## 
    ##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
    ## 1 115847 38685488                                  
    ## 2 115846 37798696  1    886792 2717.9 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
attach(dat_use)
```

    ## The following object is masked from package:survival:
    ## 
    ##     veteran

``` r
predNN <- length(UHRSWORK)
set.seed(56473)
pred_obs <- (runif(predNN) < 0.7)
pred_set <-subset(dat_use, pred_obs)
hrspredict <- data.frame((AGE >= 55) & (AGE <= 60),  female = 0, educ_advdeg = 1, educ_college = 0, educ_hs = 0, educ_somecoll = 0, (FTOTINC < 30000))
hrspredict$yhat <- predict(hrsOLS, newdata = hrspredict)
summary(hrspredict$yhat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   37.96   39.37   40.07   40.62   41.15   75.95    7955

``` r
hrspredvals <- subset(hrspredict$yhat, (AGE >= 55)&(female == 1))
# Predicted values for Women over the age of 55.
head(hrspredvals)
```

    ## [1] 39.02453 41.12188 38.65452 38.32354 39.22601 38.85890

``` r
pred_vals_OLS <- suppressWarnings(predict(hrsOLS, hrspredict))
pred_model_OLS1 <- (pred_vals_OLS > 0.5)
pred1OLStable <- table(pred = pred_model_OLS1, true = dat_use$UHRSWORK)
pred1OLStable
```

    ##       true
    ## pred       0     1     2     3     4     5     6     7     8     9    10    11
    ##   TRUE 24394    80   161   166   271   269   257    93   502    90   806    42
    ##       true
    ## pred      12    13    14    15    16    17    18    19    20    21    22    23
    ##   TRUE   584    76   135  1152   614   101   302    70  3723   135   199   111
    ##       true
    ## pred      24    25    26    27    28    29    30    31    32    33    34    35
    ##   TRUE   829  2157   137   181   428    56  4067    45   981   143   143  6129
    ##       true
    ## pred      36    37    38    39    40    41    42    43    44    45    46    47
    ##   TRUE   884  1408  1381   233 38733   104   768   276   310  5163   204   175
    ##       true
    ## pred      48    49    50    51    52    53    54    55    56    57    58    59
    ##   TRUE  1004    54  7667    27   236    45    88  1608   138    31    62     5
    ##       true
    ## pred      60    61    62    63    64    65    66    67    68    69    70    71
    ##   TRUE  3443     5    29    14    25   436    25     2    18     2   637     2
    ##       true
    ## pred      72    73    74    75    76    77    78    80    82    84    85    86
    ##   TRUE   101     5     6   145     9     7     8   422     0    53    26     6
    ##       true
    ## pred      88    90    91    92    93    94    95    96    97    98    99
    ##   TRUE     3    74     2     1     2     1     4     5     1     8    94

#### Question 7

For the logit model, I’m using a similar set of variables, excluding
FTOTINCOME.

The probabilities implied by the coefficient estimate patterns are
similar to those in the OLS model.

The linear hypothesis test returns a similary outcome - the education
variables are still statistically significant.

``` r
attach(dat_use)
```

    ## The following objects are masked from dat_use (pos = 3):
    ## 
    ##     AfAm, AGE, Amindian, ANCESTR1, ANCESTR1D, ANCESTR2, ANCESTR2D,
    ##     Asian, below_150poverty, below_200poverty, below_povertyline, BPL,
    ##     BPLD, BUILTYR2, CITIZEN, CLASSWKR, CLASSWKRD, Commute_bus,
    ##     Commute_car, Commute_other, Commute_rail, Commute_subway, COSTELEC,
    ##     COSTFUEL, COSTGAS, COSTWATR, DEGFIELD, DEGFIELD2, DEGFIELD2D,
    ##     DEGFIELDD, DEPARTS, EDUC, educ_advdeg, educ_college, educ_hs,
    ##     educ_nohs, educ_somecoll, EDUCD, EMPSTAT, EMPSTATD, FAMSIZE,
    ##     female, foodstamps, FOODSTMP, FTOTINC, FUELHEAT, GQ,
    ##     has_AnyHealthIns, has_PvtHealthIns, HCOVANY, HCOVPRIV, HHINCOME,
    ##     Hisp_Cuban, Hisp_DomR, Hisp_Mex, Hisp_PR, HISPAN, HISPAND,
    ##     Hispanic, in_Bronx, in_Brooklyn, in_Manhattan, in_Nassau, in_NYC,
    ##     in_Queens, in_StatenI, in_Westchester, INCTOT, INCWAGE, IND,
    ##     LABFORCE, LINGISOL, MARST, MIGCOUNTY1, MIGPLAC1, MIGPUMA1,
    ##     MIGRATE1, MIGRATE1D, MORTGAGE, NCHILD, NCHLT5, OCC, OWNCOST,
    ##     OWNERSHP, OWNERSHPD, POVERTY, PUMA, PWPUMA00, RACE, race_oth,
    ##     RACED, RELATE, RELATED, RENT, ROOMS, SCHOOL, SEX, SSMC, TRANTIME,
    ##     TRANWORK, UHRSWORK, UNITSSTR, unmarried, veteran, VETSTAT,
    ##     VETSTATD, white, WKSWORK2, YRSUSA1

    ## The following object is masked from package:survival:
    ## 
    ##     veteran

``` r
WorkFT <- (dat_use$UHRSWORK >= 35)
hrslogit <- glm(WorkFT ~ AGE + female + educ_advdeg + educ_college + educ_hs + educ_somecoll, family = binomial)
summary(hrslogit)
```

    ## 
    ## Call:
    ## glm(formula = WorkFT ~ AGE + female + educ_advdeg + educ_college + 
    ##     educ_hs + educ_somecoll, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0254  -1.1162   0.6803   0.9734   1.7215  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -0.6948481  0.0280381  -24.78   <2e-16 ***
    ## AGE            0.0097793  0.0004311   22.68   <2e-16 ***
    ## female        -0.7050940  0.0123652  -57.02   <2e-16 ***
    ## educ_advdeg    1.9726053  0.0276081   71.45   <2e-16 ***
    ## educ_college   1.6688908  0.0250156   66.71   <2e-16 ***
    ## educ_hs        0.6577738  0.0228853   28.74   <2e-16 ***
    ## educ_somecoll  0.9248665  0.0239554   38.61   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 167213  on 123808  degrees of freedom
    ## Residual deviance: 155056  on 123802  degrees of freedom
    ## AIC: 155070
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
linearHypothesis(hrslogit, "educ_advdeg + educ_hs + educ_somecoll + educ_college = 0", test = "F")
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## educ_advdeg  + educ_college  + educ_hs  + educ_somecoll = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: WorkFT ~ AGE + female + educ_advdeg + educ_college + educ_hs + 
    ##     educ_somecoll
    ## 
    ##   Res.Df Df      F    Pr(>F)    
    ## 1 123803                        
    ## 2 123802  1 3596.1 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*All of the work on this exam is my own, answered honestly as rules
state.* Name: Patrick Sinclair Date: 11/23/2020
