# Koka Venkata Sowjanya
#=========================================================================

#Question0. Read the dataset. Make sure that the variable “Area” is a factor.

###Answer:
data <- read.table(file="/Users/sowjanyakoka/Desktop/Fall 2020/Stats Methods/Datasets/assessed_value.csv",
                   header=T,
                   sep=",",
                   colClasses = c("numeric","numeric", "factor", "character"))
dim(data)

# The variable area is a factor and it has 11 levels

#Question1. 1. Formulate an ANOVA model to assess if there is evidence of at least one “Area” that shows a mean
#assessed property value from year 2 which is different from the assessed property value of other areas
#for that same year. Estimate the model and display the summary results

###Answer:

#Formulating Anova model 
m <- lm(Assess_year_2~Area, data=data)
#Displaying the summary results
summary(m)

#Intercept(b0) is east central


#Using boxplots to see variability in each group
library(ggplot2)
g0 <- ggplot(data, aes(factor(Area),Assess_year_2))
g0 + geom_boxplot()

#Question2. Based on the results, is there evidence of a difference in mean assessed property value for
#the various areas in Wisconsin? Use ANOVA, which considers family-wise error. Explain how
#you reached that conclusion (i.e. which statistics did you use and how?).

###Answer:  
#Area far west        72877      19884   3.665 0.000449 ***

#The mean assessed property value(year2) of far west area is different from the mean assessed property value(year2) of intercept area (east central) in our model
# there is a estimated difference of $72877 indicating more or a higher property value at far west and
# the p-value associated with the above estimated difference is 0.000449 which is below the level of significance 0.05 
# indicating the difference observed is statistically significant

# Yes based on the summary results of linear model m, we observe a F-staistic value(2.495) and p-value(0.01184) for the model 
# which is small compared to the level of significance value(0.05) indicating we found evidence for rejecting the null hypothesis(all mean values are same)
# and there is evidence that at least one of the areas mean assessed property values(year_2) is different from any of the other areas in Wisconsin

anova.1 <- aov(m)
summary(anova.1)

#Using Anova the total variance observed in the data is 22.2e+10 
# The test statistic F, which represents a standardized ratio of variability in the sample means relative to the variability within the groups. 
#The p-value is smaller than 0.05, indicating the evidence is strong enough to reject the null hypothesis
#at a significance level of 0.05,the larger F will be and the stronger the evidence against the null hypothesis we have a F-value of 2.495


#library(car)
#Anova (m, type="III")

#Question3. Conduct a post-hoc analysis and display the results. Analyze the results:
#  • Which areas have statistically significant different mean assessed values?
#  • Which areas seem to have greater assessed property values?
library(multcomp)
###Answer:  

postHocs<-glht(anova.1, linfct = mcp(Area = "Tukey"))
summary(postHocs)

confint(postHocs)

#Using post-hoc Tukey test analysis we found evidence for statistically significant different mean assessed values for the following areas 
#at a level significance 0.05 

#  • Which areas have statistically significant different mean assessed values?

###Answer:
#far west - east central      Estimate Std: 72876.67  p value: 0.0169 *
#near east - far west         Estimate Std: -85116.67 p value: 0.0163 *

#  • Which areas seem to have greater assessed property values?

###Answer:
# The estimated difference in comparison of far west - east central mean assessed property values is $72876.67 and has a confidence interval [7496.1638,138257.1695]
# indicating far west has a higher assessed property value

# The estimated difference in comparison of near east - far west  mean assessed property values is $ $-85116.67 and has a confidence interval [161364.7563  -8868.5770]
# indicating near east has a less assessed property value compared to far west

#The area that seems to have greater assessed property value is far west

#Question4. The statistical assessment of coefficients is important in ANOVA. Briefly, analyze the residuals and
#assess the Gauss-Markov Theorem (i.e., mean residual zero? heteroscedasticity?, serial correlation?) and the Normality of residuals. You should show the results of the appropriate
#inferential methods and to make the assessments based on those results. Four sentences should suffice.

###Answer:

#The residuals of the model are
resid_values <- residuals(m)
#Displaying the residual values
head(resid_values)

# checking the mean of residuals
#• mean residual
mean(resid_values)

###Answer:
#The mean residual value(-4.344689e-13) is very close to zero by which we can say that the,
#the errors have an expectation of zero from Gauss Markov Theorem

library(lmtest)
#• heteroscedasticity
#The errors have equal variances
#Ho : Homoscedastic
#Ha : Heteroscedastic
bptest(formula = m)

###Answer:(Homoscedastic)
# The p-value we have from bptest is 0.7112 which is higher than the level of significance(0.05),
# indicating we do not have enough evidence to reject the null hypothesis (Homoscedastic) 
# we did not find evidence for Heteroscedastic (unequal variances in errors) so our estimates for the slopes 
# cannot be effected by the model 

#•serial correlation
#The residuals must not be correlated among themselves
#Ho : No first order correlation
#Ha : First order correlation
dwtest(formula = m,alternative = "two.sided")

###Answer:(No first order correlation)
# The p-value we have from dwtest is 0.9026 which is higher than the level of significance(0.05),
# indicating we do not have enough evidence to reject the null hypothesis(No first order correlation), 
# we did not find evidence for first order autocorrelation (the error of consecutive observations are not correlated) in our model

#• normality of residuals
#Testing for normality
#Ho : Gaussian distributed (normal)
#Ha : Not Gaussian distributed (not normal)

hist(resid_values, col = "lightblue") 
# not completely symmetric

library(car)

qqPlot(x=resid_values) 
#There are departures

shapiro.test(resid_values)

###Answer:(Not Gaussian distributed)
# The p-value we have from shapiro test is 4.203e-10 which is lower than the level of significance(0.05),
# indicating we have enough evidence to reject the null hypothesis(Gaussian distributed (normal)) 
# we have found evidence for Not Gaussian distributed(not normally distributed) in our model and
# proceed with the distribution of residuals is Not Gaussian
