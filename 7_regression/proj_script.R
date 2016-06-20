library(dplyr)

df <- mtcars
df[,c(2,8:11)] <- sapply(df[,c(2,8:11)], as.factor)

print(str(df))

## Observations:
# 1. mpg is the outcome, all other variables are possible predictors
# 2. #Cylinders, Displacement, and hp are highly correlated, important parameter in mpg.
# 3. Weight is independent, but important parameter in mpg.
# 4. "am" is our test variable
# 5. Test influence of other regressors and add/omit accodingly



#Methodoloogy: Nested Model Testing, Multivariate Linear Regression
fam = lm(mpg ~ am, data=df)
fcyl = lm(mpg ~ cyl, data=df)
f3 = lm(mpg ~ cyl + disp + hp, data=df)
f4 = fit1 = lm(mpg ~ cyl + disp + hp + wt, data=df)
f5 = fit1 = lm(mpg ~ cyl + disp + hp + wt + am, data=df)

print(summary(fam))
# print(summary(fcyl))
# print(summary(f3))
# print(summary(f4))
# print(summary(f5))