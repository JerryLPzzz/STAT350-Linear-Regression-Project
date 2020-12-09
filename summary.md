Summary
================
Peiyuan Li
December 8, 2020

### **Method used**

**Predictive mean matching** for missing value imputation.  
**Boxplot** and **Histogram** for univariate analysis.  
**Stepwise regression**, **Backward elimination**, and **Forward
selection** for variable selection.  
**correlation matrix heatmap** and **Variance inflation factor** for
multicollinearity detection.  
**Residual vs Fitted plot** and **Q-Q plot** for normality assumption.  
**Q-Q plot** and **Residual vs Leverage plot** for outlier and
influencial point detection.  

### **Results**

**The final formular is:**  

``` r
WHO_df <- read.csv("WHO_data_filled.csv",header=TRUE)
For_lm_DF <- WHO_df[-c(1:2)]
stepwise_lm_DF <- For_lm_DF[-c(6,7,8,17,16)]
stepwise_Fix.multicol_lm_DF <- stepwise_lm_DF[-c(4, 14)] 
fit_stepwise_Fix.multicol <- lm(Life.expectancy ~ .,
                                data = stepwise_Fix.multicol_lm_DF)
summary(fit_stepwise_Fix.multicol)
```

    ## 
    ## Call:
    ## lm(formula = Life.expectancy ~ ., data = stepwise_Fix.multicol_lm_DF)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -24.2267  -2.4214  -0.0602   2.4458  18.6774 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      5.777e+01  5.908e-01  97.776  < 2e-16 ***
    ## StatusDeveloping                -1.905e+00  2.900e-01  -6.569 5.98e-11 ***
    ## Adult.Mortality                 -2.107e-02  8.479e-04 -24.853  < 2e-16 ***
    ## Alcohol                          1.597e-01  2.589e-02   6.166 7.95e-10 ***
    ## BMI                              5.860e-02  4.926e-03  11.895  < 2e-16 ***
    ## under.five.deaths               -2.911e-03  5.225e-04  -5.570 2.77e-08 ***
    ## Polio                            3.327e-02  4.719e-03   7.050 2.23e-12 ***
    ## Total.expenditure                1.171e-01  3.446e-02   3.398 0.000688 ***
    ## Diphtheria                       4.042e-02  4.682e-03   8.633  < 2e-16 ***
    ## HIV.AIDS                        -4.904e-01  1.869e-02 -26.235  < 2e-16 ***
    ## GDP                              4.884e-05  7.092e-06   6.888 6.91e-12 ***
    ## Income.composition.of.resources  1.204e+01  5.237e-01  22.985  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.353 on 2926 degrees of freedom
    ## Multiple R-squared:  0.792,  Adjusted R-squared:  0.7912 
    ## F-statistic:  1013 on 11 and 2926 DF,  p-value: < 2.2e-16

If we input the new additional data point for Utopia, a life expectancy
of 111.412 will be computed. Which is an ideal age for someone who lives
in a Utopian state. 

### **Conclusion**

  - From the regression model, I realized that GDP per capita (Variable
    GDP) plays an important role on life expectancy. I think this is a
    reflection of the reality.  
  - The final regression model provided has all of its variables
    contributed signficantly. Moreover, the residual plot and normal Q-Q
    confirmed that it passed the normality assumption. It has an
    R-squared value over 0.79, which suggests the model captures almost
    80% of the data points.
