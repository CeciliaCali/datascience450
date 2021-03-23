#Student Performance 
# Final Project

# GitHub repository URL: https://github.com/CeciliaCali/datascience450



#Hypothesis: Parents' level of education has a greater impact on test scores than any other factor.  This is true for all tests.

studentdf <- read.csv("/Users/personal/Desktop/DataScience/Final Project/StudentsPerformance.csv", header=TRUE)

#Data repository
#Public school scores

# Review data set.  
str(studentdf)
# 'data.frame':	1000 obs. of  8 variables:
#         $ gender                     : chr  "female" "female" "female" "male" ...
# $ race.ethnicity             : chr  "group B" "group C" "group B" "group A" ...
# $ parental.level.of.education: chr  "bachelor's degree" "some college" "master's degree" "associate's degree" ...
# $ lunch                      : chr  "standard" "standard" "standard" "free/reduced" ...
# $ test.preparation.course    : chr  "none" "completed" "none" "none" ...
# $ math.score                 : int  72 69 90 47 76 71 88 40 64 38 ...
# $ reading.score              : int  72 90 95 57 78 83 95 43 64 60 ...
# $ writing.score              : int  74 88 93 44 75 78 92 39 67 50 ...


# Cast predictor variables as factors.

as.factor(studentdf$gender)
as.factor(studentdf$race.ethnicity)
as.factor(studentdf$parental.level.of.education)
as.factor(studentdf$lunch)
as.factor(studentdf$test.preparation.course)

# Explore data set and find range of target variables (test scores).

summary(studentdf)

# gender          race.ethnicity     parental.level.of.education    lunch           test.preparation.course   math.score    
# Length:1000        Length:1000        Length:1000                 Length:1000        Length:1000             Min.   :  0.00  
# Class :character   Class :character   Class :character            Class :character   Class :character        1st Qu.: 57.00  
# Mode  :character   Mode  :character   Mode  :character            Mode  :character   Mode  :character        Median : 66.00  
# Mean   : 66.09  
# 3rd Qu.: 77.00  
# Max.   :100.00  

# reading.score    writing.score   
# Min.   : 17.00   Min.   : 10.00  
# 1st Qu.: 59.00   1st Qu.: 57.75  
# Median : 70.00   Median : 69.00  
# Mean   : 69.17   Mean   : 68.05  
# 3rd Qu.: 79.00   3rd Qu.: 79.00  
# Max.   :100.00   Max.   :100.00  

# Explore data set and find out number of observations for predictor variables.

library(dplyr)


studentdf %>% count(gender)
#   gender   n
# 1 female 518
# 2   male 482

studentdf %>% count(race.ethnicity)
#   race.ethnicity   n
# 1        group A  89
# 2        group B 190
# 3        group C 319
# 4        group D 262
# 5        group E 140

studentdf %>% count(parental.level.of.education)
#   parental.level.of.education   n
# 1          associate's degree 222
# 2           bachelor's degree 118
# 3                 high school 196
# 4             master's degree  59
# 5                some college 226
# 6            some high school 179

studentdf %>% count(lunch)
#          lunch   n
# 1 free/reduced 355
# 2     standard 645

studentdf %>% count(test.preparation.course)
#   test.preparation.course   n
# 1               completed 358
# 2                    none 642


# I reorder the levels of target of parental level of education to reflect the 
# number of education. R's default order for levels is alphabetical which does 
#not make sense in this case.

studentdf$parental.level.of.education <- factor(studentdf$parental.level.of.education, 
                                                   levels =c ("some high school",
                                                     "high school",
                                                     "some college",
                                                     "associate's degree",
                                                     "bachelor's degree",
                                                     "master's degree"))


# ----------------------------------------------
# ----------------------------------------------
# Histograms of Test Scores


# Create histogram of math score distribution. 

par(mar=c(5,5,2,2)) #Set generous margins to fit in all labels and text.

hist(studentdf$math.score,
     main = "Histogram of Math Score",
     xlab="Test Scores",
     ylab = "Number of Students",
     col = "#865858")

# Create histogram of reading score distribution. 
hist(studentdf$reading.score,
     main = "Histogram of Reading Score",
     xlab="Test Scores",
     ylab = "Number of Students",
     col = "#865858")

# Create histogram of writing score distribution. 
hist(studentdf$reading.score,
     main = "Histogram of Writing Score",
     xlab="Test Scores",
     ylab = "Number of Students",
     col = "#865858")

# ----------------------------------------------
# ----------------------------------------------
# Review data for correlation between the test scores.

cor(studentdf[,c(6:8)], method="pearson") 
#               math.score reading.score writing.score
# math.score     1.0000000     0.8175797     0.8026420
# reading.score  0.8175797     1.0000000     0.9545981
# writing.score  0.8026420     0.9545981     1.0000000

# Reading and writing scores are highly correlated at 0.95.  
# Math scores are less correlated to reading scores (0.81) and writing 
# scores (0.80), although their correlation percentages are still high.

pairs(studentdf[,c(6:8)]) 
# Exploratory Data Analysis for a quick visual of correlation between test scores.  
# This is for my benefit.  I do not include in the report.


# ----------------------------------------------
# ----------------------------------------------
# Math Score Boxplots

# Compare math scores of female students and male students
boxplot(studentdf$math.score ~ as.factor(studentdf$gender),
        main = "Math Score Distribution - Females vs. Males",
        xlab = "",
        ylab = "Math Scores",
        col=c("#865858", "#e2d5d5"), 
        names=c("female", "male"), varwidth=TRUE)


# Comparison of math scores by race / ethnicity. 
boxplot(studentdf$math.score ~ as.factor(studentdf$race.ethnicity),
        main = "Math Score Distribution - Race / Ethnicity",
        xlab = "",
        ylab = "Math Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0"), 
        varwidth=TRUE)

# Comparison of math scores by parental level of education.
par(mar=c(8,5,2,2), las=2) # Make the x-axis labels vertical so they all fit on the graph.
boxplot(studentdf$math.score ~ as.factor(studentdf$parental.level.of.education), 
        main = "Math Score Distribution - Parents' Education",
        xlab = "",
        ylab = "Math Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0", "#276678"), 
        varwidth=TRUE)        

# Comparison of math scores by student lunch benefits. 
par(mar=c(5,5,2,2), las=0) # Resize graph and label orientations.

boxplot(studentdf$math.score ~ as.factor(studentdf$lunch), 
        main = "Math Score Distribution - Lunch Benefits",
        xlab = "",
        ylab = "Math Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)

# Comparison of math scores by test preparation course.
boxplot(studentdf$math.score ~ as.factor(studentdf$test.preparation.course),
        main = "Math Score Distribution - Test Preparation",
        xlab = "",
        ylab = "Math Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)


# ----------------------------------------------
# ----------------------------------------------
# Reading and Writing Scores Boxplots

# Because reading and writing are closely related, I analyze reading and writing 
# scores side by side.

# Comparison of reading scores of female students and male students

boxplot(studentdf$reading.score ~ as.factor(studentdf$gender),
        main = "Reading Score Distribution - Females vs. Males",
        xlab = "",
        ylab = "Reading Scores",
        col=c("#865858", "#e2d5d5"), 
        names=c("female", "male"), varwidth=TRUE)

# Comparison of writing scores of female students and male students
boxplot(studentdf$writing.score ~ as.factor(studentdf$gender),
        main = "Writing Score Distribution - Females vs. Males",
        xlab = "",
        ylab = "Writing Scores",
        col=c("#865858", "#e2d5d5"), 
        names=c("female", "male"), varwidth=TRUE)



# Comparison of reading scores of by race / ethnicity. 
boxplot(studentdf$reading.score ~ as.factor(studentdf$race.ethnicity),
        main = "Reading Score Distribution - Race / Ethnicity",
        xlab = "",
        ylab = "Reading Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0"), 
        varwidth=TRUE)

# Comparison of writing scores of by race / ethnicity. 
boxplot(studentdf$writing.score ~ as.factor(studentdf$race.ethnicity),
        main = "Writing Score Distribution - Race / Ethnicity",
        xlab = "",
        ylab = "Writing Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0"), 
        varwidth=TRUE)



# Comparison of reading scores of by parental level of education.
par(mar=c(8,5,2,2), las=2) # Make the x-axis labels vertical so they all fit on the graph.
boxplot(studentdf$reading.score ~ as.factor(studentdf$parental.level.of.education), 
        main = "Reading Score Distribution - Parents' Education",
        xlab = "",
        ylab = "Reading Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0", "#276678"), 
        varwidth=TRUE)        

# Comparison of writing scores of by parental level of education.
boxplot(studentdf$writing.score ~ as.factor(studentdf$parental.level.of.education), 
        main = "Writing Score Distribution - Parents' Education",
        xlab = "",
        ylab = "Writing Scores",
        col=c("#865858", "#8e7f7f", "#bbbbbb", "#e2d5d5", "#94b5c0", "#276678"), 
        varwidth=TRUE)        


par(mar=c(5,5,2,2), las=0) # Resize graph and label orientations.
# Comparison of reading scores of by lunch benefits.
boxplot(studentdf$reading.score ~ as.factor(studentdf$lunch), 
        main = "Reading Score Distribution - Lunch Benefits",
        xlab = "",
        ylab = "Reading Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)

# Comparison of writing scores of by lunch benefits.
boxplot(studentdf$writing.score ~ as.factor(studentdf$lunch), 
        main = "Writing Score Distribution - Lunch Benefits",
        xlab = "",
        ylab = "Writing Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)


# Comparison of reading scores of by test preparation course.
boxplot(studentdf$reading.score ~ as.factor(studentdf$test.preparation.course),
        main = "Reading Score Distribution - Test Preparation",
        xlab = "",
        ylab = "Reading Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)

# Comparison of writing scores of by test preparation course.
boxplot(studentdf$writing.score ~ as.factor(studentdf$test.preparation.course),
        main = "Writing Score Distribution - Test Preparation",
        xlab = "",
        ylab = "Writing Scores",
        col=c("#865858", "#e2d5d5"),
        varwidth=TRUE)




# ----------------------------------------------
# ----------------------------------------------
# Supervised Machine Learning

# I run a linear regression with the target variable of math score and the 
# predictor variables of gender, race/ethnicity, parental level of education, 
# lunch benefits, and test preparation course.

lm1 <- lm(studentdf$math.score ~ studentdf$gender + studentdf$race.ethnicity + 
                  studentdf$parental.level.of.education + studentdf$lunch +
                  studentdf$test.preparation.course)

summary(lm1)
# Call:
#         lm(formula = studentdf$math.score ~ studentdf$gender + studentdf$race.ethnicity + 
#                    studentdf$parental.level.of.education + studentdf$lunch + 
#                    studentdf$test.preparation.course)

# Residuals:
#         Min      1Q  Median      3Q     Max 
#     -50.357  -8.744   0.166   9.001  30.655 
# 
# Coefficients:                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                              47.8872     1.8032  26.557  < 2e-16 ***
# studentdf$gendermale                                      4.9953     0.8390   5.954 3.63e-09 ***
# studentdf$race.ethnicitygroup B                           2.0408     1.6998   1.201 0.230181    
# studentdf$race.ethnicitygroup C                           2.4700     1.5918   1.552 0.121060    
# studentdf$race.ethnicitygroup D                           5.3410     1.6241   3.289 0.001042 ** 
# studentdf$race.ethnicitygroup E                          10.1347     1.8015   5.626 2.41e-08 ***
# studentdf$parental.level.of.educationhigh school         -0.5540     1.3713  -0.404 0.686277    
# studentdf$parental.level.of.educationsome college         3.6660     1.3250   2.767 0.005768 ** 
# studentdf$parental.level.of.educationassociate's degree   4.2487     1.3331   3.187 0.001482 ** 
# studentdf$parental.level.of.educationbachelor's degree    6.2148     1.5660   3.969 7.75e-05 ***
# studentdf$parental.level.of.educationmaster's degree      7.1371     1.9903   3.586 0.000352 ***
# studentdf$lunchstandard                                  10.8768     0.8727  12.463  < 2e-16 ***
# studentdf$test.preparation.coursecompleted                5.4947     0.8756   6.275 5.22e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.17 on 987 degrees of freedom
# Multiple R-squared:  0.2548,	Adjusted R-squared:  0.2457 
# F-statistic: 28.12 on 12 and 987 DF,  p-value: < 2.2e-16



plot(lm1)

# Is a logistic regression better?  Let's see.

formula1 <- studentdf$math.score ~ studentdf$gender + studentdf$race.ethnicity + 
        studentdf$parental.level.of.education + studentdf$lunch +
        studentdf$test.preparation.course

glm1 <- glm(formula1, data = studentdf)
summary(glm1)

# Call:
# glm(formula = formula1, data = studentdf)
# 
# Deviance Residuals: 
#         Min       1Q   Median       3Q      Max  
#     -50.357   -8.744    0.166    9.001   30.655  
# 
# Coefficients:
#                                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                              47.8872     1.8032  26.557  < 2e-16 ***
# studentdf$gendermale                                      4.9953     0.8390   5.954 3.63e-09 ***
# studentdf$race.ethnicitygroup B                           2.0408     1.6998   1.201 0.230181    
# studentdf$race.ethnicitygroup C                           2.4700     1.5918   1.552 0.121060    
# studentdf$race.ethnicitygroup D                           5.3410     1.6241   3.289 0.001042 ** 
# studentdf$race.ethnicitygroup E                          10.1347     1.8015   5.626 2.41e-08 ***
# studentdf$parental.level.of.educationhigh school         -0.5540     1.3713  -0.404 0.686277    
# studentdf$parental.level.of.educationsome college         3.6660     1.3250   2.767 0.005768 ** 
# studentdf$parental.level.of.educationassociate's degree   4.2487     1.3331   3.187 0.001482 ** 
# studentdf$parental.level.of.educationbachelor's degree    6.2148     1.5660   3.969 7.75e-05 ***
# studentdf$parental.level.of.educationmaster's degree      7.1371     1.9903   3.586 0.000352 ***
# studentdf$lunchstandard                                  10.8768     0.8727  12.463  < 2e-16 ***
# studentdf$test.preparation.coursecompleted                5.4947     0.8756   6.275 5.22e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 173.4241)
# 
#     Null deviance: 229689  on 999  degrees of freedom
# Residual deviance: 171170  on 987  degrees of freedom
# AIC: 8008.5
# 
# Number of Fisher Scoring iterations: 2

plot(glm1)

# No.  A logistic regression does not offer a better fit.



# ----------------------------------------------
# ----------------------------------------------
# Create training set and test set to test accuracy of algorithm.
n <- nrow(studentdf)
ntrain <- round(n*0.75)
set.seed(123)           # Set seed for reproducible results.
tindex <- sample(n, ntrain)

traindf <- studentdf[tindex,]
testdf <- studentdf[-tindex,]



# ----------------------------------------------
# ----------------------------------------------
#Linear regression on math score using the training data set.
lm1 <- lm(math.score ~ gender + race.ethnicity +parental.level.of.education + 
                  lunch + test.preparation.course, data = traindf)

summary(lm1)

# Call:
# lm(formula = math.score ~ gender + race.ethnicity + parental.level.of.education + 
#                    lunch + test.preparation.course, data = traindf)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
#     -44.896  -9.225   0.486   9.609  31.106 
#
# Coefficients:                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                    49.9559     2.1217  23.545  < 2e-16 ***
# gendermale                                      4.8107     0.9654   4.983 7.80e-07 ***
# race.ethnicitygroup B                           1.1610     2.0064   0.579  0.56299    
# race.ethnicitygroup C                           2.3909     1.8490   1.293  0.19639    
# race.ethnicitygroup D                           4.4073     1.8911   2.331  0.02005 *  
# race.ethnicitygroup E                           8.8119     2.0780   4.241 2.51e-05 ***
#  parental.level.of.educationhigh school         -1.0843     1.5923  -0.681  0.49613    
# parental.level.of.educationsome college         2.5689     1.5167   1.694  0.09073 .  
# parental.level.of.educationassociate's degree   3.0666     1.5424   1.988  0.04716 *  
# parental.level.of.educationbachelor's degree    5.4318     1.8370   2.957  0.00321 ** 
# parental.level.of.educationmaster's degree      5.9427     2.2428   2.650  0.00823 ** 
# lunchstandard                                  10.2103     1.0120  10.089  < 2e-16 ***
# test.preparation.coursecompleted                5.7036     1.0110   5.641 2.41e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.15 on 737 degrees of freedom
# Multiple R-squared:  0.2337,	Adjusted R-squared:  0.2212 
# F-statistic: 18.73 on 12 and 737 DF,  p-value: < 2.2e-16



# User-Defined Function for R-squared so I can compare accuracy of training model.

rsquared <- function(y_hat, y){
        mu <- mean(y)
        rse <- mean((y_hat - y)^2) / mean ((mu - y)^2)
        rsquared <- (1 - rse)*100
        return(rsquared)
}



# Test how well my model predicts using the test set.

y_hat <-lm1$fitted.values
y <- traindf$math.score
rsquared(y_hat, y)
# [1] 23.36729

y_hat <- predict(lm1, newdata = testdf)
y <- testdf$math.score
rsquared(y_hat, y)
# [1] 29.45597

# The R-squared for the training set and test set are similar, so the training
# model looks valid.

plot(lm1)       # View plots to see if the data is well-behaved.

formula1 <- studentdf$math.score ~ studentdf$gender + studentdf$race.ethnicity + 
        studentdf$parental.level.of.education + studentdf$lunch +
        studentdf$test.preparation.course


# ----------------------------------------------
# ----------------------------------------------

#Linear regressions on reading score

lm2 <- lm(reading.score ~ gender + race.ethnicity +parental.level.of.education + 
                  lunch + test.preparation.course, data = traindf)

summary(lm2)
# Call:
#         lm(formula = reading.score ~ gender + race.ethnicity + parental.level.of.education + 
#                    lunch + test.preparation.course, data = traindf)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
#     -40.163  -8.369   0.548   9.533  29.427 
# 
# Coefficients:        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                    61.4799     2.0523  29.957  < 2e-16 ***
# gendermale                                     -7.2915     0.9338  -7.809 1.99e-14 ***
# race.ethnicitygroup B                           1.0925     1.9407   0.563 0.573643    
# race.ethnicitygroup C                           2.6230     1.7885   1.467 0.142910    
# race.ethnicitygroup D                           3.2025     1.8292   1.751 0.080399 .  
# race.ethnicitygroup E                           4.2117     2.0100   2.095 0.036479 *  
# parental.level.of.educationhigh school         -1.6441     1.5402  -1.067 0.286118    
# parental.level.of.educationsome college         2.1726     1.4670   1.481 0.139041    
# parental.level.of.educationassociate's degree   3.0487     1.4919   2.043 0.041359 *  
# parental.level.of.educationbachelor's degree    5.2406     1.7769   2.949 0.003285 ** 
# parental.level.of.educationmaster's degree      7.5861     2.1694   3.497 0.000499 ***
# lunchstandard                                   6.7038     0.9789   6.848 1.58e-11 ***
# test.preparation.coursecompleted                7.7500     0.9779   7.925 8.45e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.72 on 737 degrees of freedom
# Multiple R-squared:  0.231,	Adjusted R-squared:  0.2185 
# F-statistic: 18.45 on 12 and 737 DF,  p-value: < 2.2e-16


plot(lm2)       # View plots to see if the data is well-behaved.


# Test how well my model predicts using the test set.

y_hat <-lm2$fitted.values
y <- traindf$reading.score
rsquared(y_hat, y)
# [1] 23.09751

y_hat <- predict(lm2, newdata = testdf)
y <- testdf$reading.score
rsquared(y_hat, y)
# [1] 20.13765

# The R-squared for the training set and test set are similar, so the training
# model looks valid.


# ----------------------------------------------
# ----------------------------------------------

#Linear regressions on writing score
lm3 <- lm(writing.score ~ gender + race.ethnicity +parental.level.of.education + 
                  lunch + test.preparation.course, data = traindf)

summary(lm3)
# Call:
# lm(formula = writing.score ~ gender + race.ethnicity + parental.level.of.education + 
#                    lunch + test.preparation.course, data = traindf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -39.476  -7.588   0.423   8.791  28.048 
# 
# Coefficients:                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                   72.8220     2.0839  34.945  < 2e-16 ***
# gendermale                                    -9.1385     0.9102 -10.041  < 2e-16 ***
# race.ethnicitygroup B                          0.8379     1.8916   0.443  0.65792    
# race.ethnicitygroup C                          2.6210     1.7432   1.504  0.13313    
# race.ethnicitygroup D                          4.9000     1.7829   2.748  0.00614 ** 
# race.ethnicitygroup E                          3.7655     1.9591   1.922  0.05499 .  
# parental.level.of.educationbachelor's degree   3.3464     1.6655   2.009  0.04487 *  
# parental.level.of.educationhigh school        -5.8943     1.4224  -4.144 3.81e-05 ***
# parental.level.of.educationmaster's degree     5.5915     2.0618   2.712  0.00684 ** 
# parental.level.of.educationsome college       -0.5405     1.3508  -0.400  0.68916    
# parental.level.of.educationsome high school   -4.5567     1.4542  -3.134  0.00180 ** 
# lunchstandard                                  7.7432     0.9541   8.116 2.02e-15 ***
# test.preparation.coursenone                  -10.2013     0.9532 -10.702  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.4 on 737 degrees of freedom
# Multiple R-squared:  0.3317,	Adjusted R-squared:  0.3208 
# F-statistic: 30.48 on 12 and 737 DF,  p-value: < 2.2e-16


plot(lm3)       # View plots to see if the data is well-behaved.

# Test how well my model predicts using the test set.

y_hat <-lm3$fitted.values
y <- traindf$writing.score
rsquared(y_hat, y)
# [1] 33.17003

y_hat <- predict(lm3, newdata = testdf)
y <- testdf$writing.score
rsquared(y_hat, y)
# [1] 32.84946

# The R-squared for the training set and test set are similar, so the training
# model looks valid.









