Data cleaning
================
Peiyuan Li
December 8, 2020

**let us import libraries to clean the data**

``` r
library(mice)
library(VIM)
```

**Let us read the dataset using read.csv command**

``` r
WHO_data <- read.csv("Life Expectancy Data.csv",header=TRUE)
```

## **Data description**

**Let us check how the data is structured**

``` r
str(WHO_data)
```

    ## 'data.frame':    2938 obs. of  22 variables:
    ##  $ Country                        : Factor w/ 193 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Year                           : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
    ##  $ Status                         : Factor w/ 2 levels "Developed","Developing": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Life.expectancy                : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
    ##  $ Adult.Mortality                : int  263 271 268 272 275 279 281 287 295 295 ...
    ##  $ infant.deaths                  : int  62 64 66 69 71 74 77 80 82 84 ...
    ##  $ Alcohol                        : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
    ##  $ percentage.expenditure         : num  71.3 73.5 73.2 78.2 7.1 ...
    ##  $ Hepatitis.B                    : int  65 62 64 67 68 66 63 64 63 64 ...
    ##  $ Measles                        : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
    ##  $ BMI                            : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
    ##  $ under.five.deaths              : int  83 86 89 93 97 102 106 110 113 116 ...
    ##  $ Polio                          : int  6 58 62 67 68 66 63 64 63 58 ...
    ##  $ Total.expenditure              : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
    ##  $ Diphtheria                     : int  65 62 64 67 68 66 63 64 63 58 ...
    ##  $ HIV.AIDS                       : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
    ##  $ GDP                            : num  584.3 612.7 631.7 670 63.5 ...
    ##  $ Population                     : num  33736494 327582 31731688 3696958 2978599 ...
    ##  $ thinness..1.19.years           : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
    ##  $ thinness.5.9.years             : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
    ##  $ Income.composition.of.resources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
    ##  $ Schooling                      : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...

As we can see, there are 22 variables and almost 3000 entries. Moreover,
we can see that there are three data types: Factor, int, and num. It
defines the nature of each variable, whether the variable is defined by
a number or a factor.  

**Let us check the percentage missing values in the dataset**

``` r
missing_percent <- function(x){sum(is.na(x))/length(x)*100}
apply(WHO_data, 2, missing_percent)
```

    ##                         Country                            Year 
    ##                       0.0000000                       0.0000000 
    ##                          Status                 Life.expectancy 
    ##                       0.0000000                       0.3403676 
    ##                 Adult.Mortality                   infant.deaths 
    ##                       0.3403676                       0.0000000 
    ##                         Alcohol          percentage.expenditure 
    ##                       6.6031314                       0.0000000 
    ##                     Hepatitis.B                         Measles 
    ##                      18.8223281                       0.0000000 
    ##                             BMI               under.five.deaths 
    ##                       1.1572498                       0.0000000 
    ##                           Polio               Total.expenditure 
    ##                       0.6466984                       7.6923077 
    ##                      Diphtheria                        HIV.AIDS 
    ##                       0.6466984                       0.0000000 
    ##                             GDP                      Population 
    ##                      15.2484683                      22.1919673 
    ##            thinness..1.19.years              thinness.5.9.years 
    ##                       1.1572498                       1.1572498 
    ## Income.composition.of.resources                       Schooling 
    ##                       5.6841389                       5.5479918

There are at most, about 22 percent of missing values in one variable.
It would not be wise to delete the entire row for that missing value.
Sicne this method results at least 22 percent of data loss, which is an
amount we cannot afford.  

**introducing one new additional data point**  
One new additional point is introduced based on the original data
variables. This data point in created based on an imaginary realistic
Utopian state, where the expected quality of life is very high. I
intended to test out the life expectancy in a utopian state.  
Country: Utopia.  
Year: 2020  
Status: Developed.  
Life expectancy: very high   Adult mortality: 50  
Alcohol: 12   BMI: 40  
Death under five: 0  
Polio vaccine rate: 99  
Total expenditure rate: 10  
Diphtheria vaccine rate: 99  
HIV rate: 0.1  
GDP per capita: 80000  
Income composition of resources: 0.9  

## **Missing value imputation**

**We may impute the missing value with functions in “mice” package**

``` r
impute1 <- mice(WHO_data[,1:7], m = 3, seed = 999)
```

    ## 
    ##  iter imp variable
    ##   1   1  Life.expectancy  Adult.Mortality  Alcohol
    ##   1   2  Life.expectancy  Adult.Mortality  Alcohol
    ##   1   3  Life.expectancy  Adult.Mortality  Alcohol
    ##   2   1  Life.expectancy  Adult.Mortality  Alcohol
    ##   2   2  Life.expectancy  Adult.Mortality  Alcohol
    ##   2   3  Life.expectancy  Adult.Mortality  Alcohol
    ##   3   1  Life.expectancy  Adult.Mortality  Alcohol
    ##   3   2  Life.expectancy  Adult.Mortality  Alcohol
    ##   3   3  Life.expectancy  Adult.Mortality  Alcohol
    ##   4   1  Life.expectancy  Adult.Mortality  Alcohol
    ##   4   2  Life.expectancy  Adult.Mortality  Alcohol
    ##   4   3  Life.expectancy  Adult.Mortality  Alcohol
    ##   5   1  Life.expectancy  Adult.Mortality  Alcohol
    ##   5   2  Life.expectancy  Adult.Mortality  Alcohol
    ##   5   3  Life.expectancy  Adult.Mortality  Alcohol

This function allows R to impute missing values based on the second
column to the seventh column. we can specify 3 imputations, with a
random seed ‘999’.  

``` r
print(impute1)
```

    ## Class: mids
    ## Number of multiple imputations:  3 
    ## Imputation methods:
    ##         Country            Year          Status Life.expectancy Adult.Mortality 
    ##              ""              ""              ""           "pmm"           "pmm" 
    ##   infant.deaths         Alcohol 
    ##              ""           "pmm" 
    ## PredictorMatrix:
    ##                 Country Year Status Life.expectancy Adult.Mortality
    ## Country               0    1      1               1               1
    ## Year                  1    0      1               1               1
    ## Status                1    1      0               1               1
    ## Life.expectancy       1    1      1               0               1
    ## Adult.Mortality       1    1      1               1               0
    ## infant.deaths         1    1      1               1               1
    ##                 infant.deaths Alcohol
    ## Country                     1       1
    ## Year                        1       1
    ## Status                      1       1
    ## Life.expectancy             1       1
    ## Adult.Mortality             1       1
    ## infant.deaths               0       1
    ## Number of logged events:  45 
    ##   it im             dep meth
    ## 1  1  1 Life.expectancy  pmm
    ## 2  1  1 Adult.Mortality  pmm
    ## 3  1  1         Alcohol  pmm
    ## 4  1  2 Life.expectancy  pmm
    ## 5  1  2 Adult.Mortality  pmm
    ## 6  1  2         Alcohol  pmm
    ##                                                                                                                                                                                                       out
    ## 1 CountryCook Islands, CountryDominica, CountryMarshall Islands, CountryMonaco, CountryNauru, CountryNiue, CountryPalau, CountrySaint Kitts and Nevis, CountrySan Marino, CountryTuvalu, StatusDeveloping
    ## 2 CountryCook Islands, CountryDominica, CountryMarshall Islands, CountryMonaco, CountryNauru, CountryNiue, CountryPalau, CountrySaint Kitts and Nevis, CountrySan Marino, CountryTuvalu, StatusDeveloping
    ## 3                                                                                                                                                      CountryPalau, CountrySouth Sudan, StatusDeveloping
    ## 4 CountryCook Islands, CountryDominica, CountryMarshall Islands, CountryMonaco, CountryNauru, CountryNiue, CountryPalau, CountrySaint Kitts and Nevis, CountrySan Marino, CountryTuvalu, StatusDeveloping
    ## 5 CountryCook Islands, CountryDominica, CountryMarshall Islands, CountryMonaco, CountryNauru, CountryNiue, CountryPalau, CountrySaint Kitts and Nevis, CountrySan Marino, CountryTuvalu, StatusDeveloping
    ## 6                                                                                                                                                      CountryPalau, CountrySouth Sudan, StatusDeveloping

Since all of the missing values are numeric, it suggests imputation
methods called “Predictive Mean Matching”.  

**Let us check some imputed values**

``` r
head(impute1$imp$Alcohol)
```

    ##        1    2    3
    ## 33  0.01 0.09 0.01
    ## 49  5.39 4.53 0.01
    ## 65  7.38 7.73 8.51
    ## 81  8.56 8.40 7.51
    ## 97  3.41 4.23 3.69
    ## 113 9.84 8.25 9.07

For example, we can check all the rows that has missing values for
variable “Alchol”. And there are 3 imputations
    generated.  

``` r
WHO_data[33, 1:7] #missing value for variable "Alchol"
```

    ##    Country Year     Status Life.expectancy Adult.Mortality infant.deaths
    ## 33 Algeria 2015 Developing            75.6              19            21
    ##    Alcohol
    ## 33      NA

``` r
WHO_data[34, 1:7]
```

    ##    Country Year     Status Life.expectancy Adult.Mortality infant.deaths
    ## 34 Algeria 2014 Developing            75.4              11            21
    ##    Alcohol
    ## 34    0.01

``` r
WHO_data[49, 1:7] #missing value for variable "Alchol"
```

    ##    Country Year     Status Life.expectancy Adult.Mortality infant.deaths
    ## 49  Angola 2015 Developing            52.4             335            66
    ##    Alcohol
    ## 49      NA

``` r
WHO_data[50, 1:7]
```

    ##    Country Year     Status Life.expectancy Adult.Mortality infant.deaths
    ## 50  Angola 2014 Developing            51.7             348            67
    ##    Alcohol
    ## 50    8.33

It appears that Predictive Mean Matching method is doing very well for
the missing
    variables.  

``` r
complete(impute1, 1)[c(33, 49, 65, 81),]
```

    ##                Country Year     Status Life.expectancy Adult.Mortality
    ## 33             Algeria 2015 Developing            75.6              19
    ## 49              Angola 2015 Developing            52.4             335
    ## 65 Antigua and Barbuda 2015 Developing            76.4              13
    ## 81           Argentina 2015 Developing            76.3             116
    ##    infant.deaths Alcohol
    ## 33            21    0.01
    ## 49            66    5.39
    ## 65             0    7.38
    ## 81             8    8.56

Finally, we use “complete” function to create a complete dataset, which
the missing values been replaced with imputed values.  

**we can use the aboved method to fill all missing values**

``` r
impute4 <- mice(WHO_data[,c(1:17, 19:22)], m = 3, seed = 999)
WHO_data_filled <- complete(impute4, 1)
```

I used Predictive Mean Matching method for all variables except for
variable population. Since variable population has more than 22% missing
values, and population distribution across the world vary considerably.
It would be unreasonable to predict population based on other variables
given in the dataset. Therefore, I dropped variable population.  

## **Data type checking**

**Let us check if the data type is making sense**

``` r
str(WHO_data_filled)
```

    ## 'data.frame':    2938 obs. of  21 variables:
    ##  $ Country                        : Factor w/ 193 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Year                           : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
    ##  $ Status                         : Factor w/ 2 levels "Developed","Developing": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Life.expectancy                : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
    ##  $ Adult.Mortality                : int  263 271 268 272 275 279 281 287 295 295 ...
    ##  $ infant.deaths                  : int  62 64 66 69 71 74 77 80 82 84 ...
    ##  $ Alcohol                        : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
    ##  $ percentage.expenditure         : num  71.3 73.5 73.2 78.2 7.1 ...
    ##  $ Hepatitis.B                    : int  65 62 64 67 68 66 63 64 63 64 ...
    ##  $ Measles                        : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
    ##  $ BMI                            : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
    ##  $ under.five.deaths              : int  83 86 89 93 97 102 106 110 113 116 ...
    ##  $ Polio                          : int  6 58 62 67 68 66 63 64 63 58 ...
    ##  $ Total.expenditure              : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
    ##  $ Diphtheria                     : int  65 62 64 67 68 66 63 64 63 58 ...
    ##  $ HIV.AIDS                       : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
    ##  $ GDP                            : num  584.3 612.7 631.7 670 63.5 ...
    ##  $ thinness..1.19.years           : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
    ##  $ thinness.5.9.years             : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
    ##  $ Income.composition.of.resources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
    ##  $ Schooling                      : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...

``` r
WHO_data_filled$Year <- as.factor(WHO_data_filled$Year)
```

One more thing we need to make sure is that we should convert varibale
year as a categorical data, function “as.factor” is what we needed.  
At this point, We can confirm that there is no missing values.
Furthermore, all values are making sense. We may proceed to data
exploration.
