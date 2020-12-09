Regression analysis for WHO data
================
Peiyuan Li
November 24, 2020

### **Introduction**

WHO data keeps track of the health status and many other related factors
for all countries. By using the R command “read.csv”, we can see that
the WHO data consists with 2939 observations and 22 variables. It is
important to notice that this set of data has missing values, which
suggests that we need data cleaning and further investigation to make
sure that each of the data value corresponding to the variable makes
sense.  


### **Abstract**

  - The purpose of this project is to create a linear regression model
    for the WHO dataset. This project has three parts: data cleaning,
    data exploration, and summary.  
  - In the data cleaning part, I will be focusing on missing values in
    the dataset. Predictive mean matching is the method that will be
    used for missing value imputation. Unlike mean imputation,
    predictive mean matching aims to reduce the bias introduced in the
    dataset through imputation. This imputation method allows a smooth
    interpretation of the dataset.  
  - In the data exploration part, I will be examining each variable and
    try to explain the nature of them. Then, I will be focusing on the
    variable selection with various methods and select the best method
    for further interpretation. After that, I will be closely examining
    the dataset for multicollinearity detection and outlier
    interpretation.  
  - In the summary part, I will be explaining some of the common
    questions related to the model. I will also be testing the model
    with the one new additional data point introduced before commencing
    my analysis.  

### **Questions of interest**

  - Find out if there is evidence that all of the variables in the WHO
    data impacted individual’s life expectancy.
  - Estimate life expectancy using a linear model.
  - Find out how does life expectancy change for every addiational
    change in the variables.

### **Contents**

1.  Data cleaning
    1.  Data description
    2.  Missing values imputation
    3.  Data type checking
2.  Data exploration
    1.  Univariate analysis
          - Boxplot
          - Histogram
    2.  Variable selection
          - Forward selection
          - Backward Elimination
          - Stepwise regression
    3.  Multivariate regression analysis
          - Multicollinearity detection
          - Outlier interpretation  
3.  Summary
      - Methods
      - Results
      - Conclusion
