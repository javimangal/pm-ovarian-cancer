### This document contains the R output for the first model fitting attempt. These results do not necessarily match those presented in the paper, but are kept here for our own records.

> library(tidyverse)
> library(caret)
> library(pROC)
> library(glmnet)
> library(rms)
Loading required package: Hmisc

Attaching package: ‘Hmisc’

> dat <- read.csv("ovarian_data.csv")

> dat <- read.txt("ovarian(1).txt")

> dat <- read.delim("ovarian (1).txt", header = TRUE, sep = "\t")

> names(ovarian (1).txt)

> dat <- read.delim(file.choose(), header = TRUE, sep = "\t")
> head(dat)
  Age Ascites papflow lesdmax soldmaxorig
1  42       0       0      91           0
2  29       0       0      38          38
3  70       0       0      99          67
4  55       0       0     129         103
5  46       0       0     234           0
6  39       0       0      70          63
  wallreg Shadows pershistovca famhistovca
1       0       0            0           0
2       0       1            0           0
3       1       0            0           0
4       1       0            0           0
5       0       0            0           0
6       1       0            0           0
  hormtherapy pain colscore CA125 nrloculescat
1           0    1        1    67            1
2           0    1        2    17            0
3           0    0        4    35            1
4           0    0        4   220            5
5           0    0        2    27            2
6           1    0        2     1            1
  papnr oncocenter mal
1     0          0   0
2     0          0   0
3     0          0   1
4     0          1   1
5     0          0   0
6     0          1   0
> 
> names(dat)
 [1] "Age"          "Ascites"     
 [3] "papflow"      "lesdmax"     
 [5] "soldmaxorig"  "wallreg"     
 [7] "Shadows"      "pershistovca"
 [9] "famhistovca"  "hormtherapy" 
[11] "pain"         "colscore"    
[13] "CA125"        "nrloculescat"
[15] "papnr"        "oncocenter"  
[17] "mal"         
> 
> str(dat)
'data.frame':	5914 obs. of  17 variables:
 $ Age         : int  42 29 70 55 46 39 49 42 28 49 ...
 $ Ascites     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ papflow     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ lesdmax     : int  91 38 99 129 234 70 80 59 81 70 ...
 $ soldmaxorig : int  0 38 67 103 0 63 0 59 0 0 ...
 $ wallreg     : int  0 0 1 1 0 1 0 0 0 0 ...
 $ Shadows     : int  0 1 0 0 0 0 0 1 0 0 ...
 $ pershistovca: int  0 0 0 0 0 0 0 0 0 1 ...
 $ famhistovca : int  0 0 0 0 0 0 0 0 0 0 ...
 $ hormtherapy : int  0 0 0 0 0 1 0 1 0 0 ...
 $ pain        : int  1 1 0 0 0 0 1 1 0 0 ...
 $ colscore    : int  1 2 4 4 2 2 1 2 1 1 ...
 $ CA125       : int  67 17 35 220 27 1 14 98 19 12 ...
 $ nrloculescat: int  1 0 1 5 2 1 1 0 1 1 ...
 $ papnr       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ oncocenter  : int  0 0 0 1 0 1 0 1 0 1 ...
 $ mal         : int  0 0 1 1 0 0 0 0 0 0 ...
> library(caret)
> library(pROC)
> vars <- c("mal", "Age", "Ascites", "papflow", "soldmaxorig", "wallreg", "Shadows")
> 
> dat_model <- dat[, vars]
> 
> colSums(is.na(dat_model))
        mal         Age     Ascites 
          0           0           0 
    papflow soldmaxorig     wallreg 
          0           0           0 
    Shadows 
          0 
> 
> dat_model <- na.omit(dat_model)
> 
> table(dat_model$mal)

   0    1 
3983 1931 
> 
> fit <- glm(
+     mal ~ Age + Ascites + papflow + soldmaxorig + wallreg + Shadows,
+     data = dat_model,
+     family = binomial()
+ 
+ )
> summary(fit)

Call:
glm(formula = mal ~ Age + Ascites + papflow + soldmaxorig + wallreg + 
    Shadows, family = binomial(), data = dat_model)

Coefficients:
             Estimate Std. Error z value
(Intercept) -3.662035   0.137333  -26.66
Age          0.027442   0.002395   11.46
Ascites      1.685707   0.126057   13.37
papflow      1.348911   0.116495   11.58
soldmaxorig  0.032913   0.001243   26.47
wallreg      0.851304   0.079870   10.66
Shadows     -1.752753   0.157757  -11.11
            Pr(>|z|)    
(Intercept)   <2e-16 ***
Age           <2e-16 ***
Ascites       <2e-16 ***
papflow       <2e-16 ***
soldmaxorig   <2e-16 ***
wallreg       <2e-16 ***
Shadows       <2e-16 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 7471.5  on 5913  degrees of freedom
Residual deviance: 4548.3  on 5907  degrees of freedom
AIC: 4562.3

Number of Fisher Scoring iterations: 5

> 
> OR <- exp(coef(fit))
> 
> CI <- exp(confint(fit))
Waiting for profiling to be done...
> 
> View(CI)
> results_table <- cbind(OR, CI)
> 
> View(results_table)
> colnames(results_table) <- c("OR", "Lower_95CI", "Upper_95CI")
> 
> round(results_table, 3)
               OR Lower_95CI Upper_95CI
(Intercept) 0.026      0.020      0.034
Age         1.028      1.023      1.033
Ascites     5.396      4.228      6.932
papflow     3.853      3.070      4.848
soldmaxorig 1.033      1.031      1.036
wallreg     2.343      2.003      2.740
Shadows     0.173      0.126      0.234
> 
> dat_model$pred_prob <- predict(fit, type = "response")
> 
> head(dat_model$pred_prob)
[1] 0.07519700 0.03330175 0.78841189 0.88979165
[5] 0.08319548 0.58249499
> 
> roc_obj <- roc(dat_model$mal, dat_model$pred_prob)
Setting levels: control = 0, case = 1
Setting direction: controls < cases
> 
> auc(roc_obj)
Area under the curve: 0.8928
> 
> plot(roc_obj, main = "Apparent ROC curve")
> dat_cv <- dat_model
> 
> View(dat_model)
> dat_cv$mal_factor <- factor(
+     ifelse(dat_cv$mal == 1, "malignant", "benign"),
+     levels = c("benign", "malignant")
+ )
> exp(10 * coef(fit)["soldmaxorig"])
soldmaxorig 
   1.389758 
> val.prob(dat_model$pred_prob, dat_model$mal)
          Dxy       C (ROC)            R2 
 7.856718e-01  8.928359e-01  5.436949e-01 
            D      D:Chi-sq           D:p 
 4.941133e-01  2.923186e+03  0.000000e+00 
            U      U:Chi-sq           U:p 
-3.381806e-04 -5.456968e-12  1.000000e+00 
            Q         Brier     Intercept 
 4.944515e-01  1.161303e-01 -1.215542e-10 
        Slope          Emax           E90 
 1.000000e+00  4.585625e-02  3.512102e-02 
         Eavg           S:z           S:p 
 1.731034e-02 -1.441260e+00  1.495112e-01 
> exp(10 * coef(fit)["Age"])
     Age 
1.315768 
> exp(10 * coef(fit)["soldmaxorig"])
soldmaxorig 
   1.389758
