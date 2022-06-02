##################### Project 2 - DTSC 2302 - Spring 2022 ######################
# Research Question:  What are the contributing factors that affect US voters' #
#                     opinion on proposed increased gun control legislation?   #
# Key Independent Variables: birthYear, gender, race, education, famIncome,    #
#                            urbanCity, voteReg, party, partyLevel.            #
# Key Dependent Variables: Response ("Support","Oppose") to 3 statements about # 
#                          gun control legislation.                            #
################################################################################

#             ---------- SET UP ----------

# Load Libraries 
library(readr)
library(visreg)
library(olsrr)
library(stargazer)
library(aod)
library(ggplot2)
library(dplyr)

# set working directory 
setwd("~/Documents/RStudio Projects/DTSC_2302/Project2")

# import Data set
fullData <- read_csv("dataverse_files/fullData.csv")

# Variables of Interest
vars <- c("birthYear", "gender", "race", "education", "famIncome", "stateid", 
          "urbanCity", "voteReg", "party","partyLevel", "gunControlA", "gunControlB", "gunControlC")

#create pdf for plots
plots_pdf = pdf(file = 'Project2_plots.pdf')

# Make new data frame with selected variables
logitData <- data.frame(fullData$birthyr, 
                       fullData$gender, 
                       fullData$race,
                       fullData$educ, 
                       fullData$faminc_new, 
                       fullData$inputstate, 
                       fullData$urbancity, 
                       fullData$votereg, 
                       fullData$pid3, 
                       fullData$pid7, 
                       fullData$CC20_330a, 
                       fullData$CC20_330b, 
                       fullData$CC20_330c)
colnames(logitData) <- vars # rename columns
logitData <- na.omit(logitData) # Remove n/a's
logitData <- subset(logitData, logitData$famIncome != 97) # remove "Prefer not to say" response
logitData <- subset(logitData, logitData$urbanCity != 5) # remove "Other" response
logitData <- subset(logitData, logitData$race != 7) # remove "Other" response
logitData <- subset(logitData, logitData$voteReg != 3) # remove "Don't Know" response
logitData <- subset(logitData, logitData$party < 4) # remove "Don't Know" response

#             ---------- Modify Gun Control Questions ----------
#
# Recode the gun control responses: 0 - 'Opposes increased gun control' 
#                                   1 - 'Support increased gun control'
# On the issue of gun regulation, do you support or oppose each of the following proposals?
# - gunControlA - 'Prohibit state and local governments from publishing the names 
#                  and addresses of all gun owners.'
#   - Re-code 1's to 0 and 2's to 1 - If a respondents supports this proposal, then 
#     they oppose an overall increase in gun control legislation.
logitData$gunControlA<- ifelse(test=logitData$gunControlA == 1, yes=0, no=1)  
# - gunControlB - Ban assault rifles.
#   - Re-code 2's to 0 and 1's to 0 - If a respondent supports this proposal, 
#     then they support an overall increase in gun control legislation.
logitData$gunControlB<- ifelse(test=logitData$gunControlB == 1, yes=1, no=0) 
# - gunControlC - Make it easier for people to obtain concealed-carry permit.
#   - Re-code 1's to 0 and 2's to 1 - If a respondent supports this proposal, 
#     then they oppose an overall increase in gun control legislation.
logitData$gunControlC<- ifelse(test=logitData$gunControlC == 1, yes=0, no=1) 

# Combine the responses from the three gun control statements into a single variable 
#   1 = respondent supported 1/3 gun control statements
#   2 = respondent supported 2/3 gun control statements
#   3 = respondent supported 3/3 gun control statements
logitData$gunControlLevel <- rowSums(cbind(logitData$gunControlA, logitData$gunControlB, logitData$gunControlC))


#             ---------- UrbanCity Dummy Variables ----------

# Make dummy variables for each response - add a new column for
logitData$city <- ifelse(test=logitData$urbanCity == 1, yes = 1, no = 0)
logitData$suburb <- ifelse(test=logitData$urbanCity == 2, yes = 1, no = 0)
logitData$town <- ifelse(test=logitData$urbanCity == 3, yes = 1, no = 0)
logitData$ruralArea <- ifelse(test=logitData$urbanCity == 4, yes = 1, no = 0)


#             ---------- Import State Names ----------
# I noticed the states are coded as integers. Let's fix that
# First, pull out all the values
states <- data.frame(id = unique(fullData$inputstate))
# Then use the dplyr package to sort them
states <- states %>% arrange(id)
# import state names
stnames <- read.csv("statenames.csv")
states$name <- stnames$State
states$newid <- stnames$stateid

logitData <- merge(x=logitData, y=states, by.x="state", by.y="id")
logitData <- rename(logitData, 'stateName' = 'name')

##########----------------------- LINEAR MODELS -----------------------######### 

# Proportion of respondents who support gunControlA for each US State

# Use dplyr package to select the columns I want
tobeaggregated <- logitData %>% select(state, gunControlA) 

allGunControlA <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$gunControlA), margin = 1))

allGunControlA <- allGunControlA %>% rename(state=Var1, gunControlA=Var2, percentofstate=Freq)

support <- filter(allGunControlA, gunControlA %in% c(1))

support <- aggregate(support$percentofstate, by=list(support$state), FUN=sum)

support <- support %>% rename(state=Group.1, percentSupport=x)

support <- merge(x=support, y=states, by.x="state", by.y="id")

ggplot(support, aes(x=percentSupport)) + geom_histogram(bins=20) +
  labs(title="Proportion of Respondents who Support\n gunControlA for Each US State",
       x="Percent of Respondents in 2020 CCES",
       y="Number of states")

# Linear Model Age: 
modelAge <- lm(birthYear ~ gender + race + education + famIncome + 
                 state + voteReg + party + partyLevel + suburb + 
                 town + ruralArea + gunControlA + gunControlB,
               data = logitData)
ols_regress(modelAge) # OLSR model of age
model_age <- ols_step_best_subset(modelAge) # Select the subset of predictors that do the best at meeting some well-defined objective criterion,
                                            # such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
model_age # view the olsrr output
plot(model_age) # plot predicted residuals (red line) vs actual residuals (blue dots)
# View Residual Plots
modelsAge_resid_qq <- ols_plot_resid_qq(modelAge) # Graph for detecting violation of normality assumption. Are predicted values close to actual values?
ols_test_normality(modelAge) # Test for detecting violation of normality assumption.
ols_test_correlation(modelAge) # Correlation between observed residuals and expected residuals under normality.
modelsAge_plot_resid_fit <- ols_plot_resid_fit(modelAge, main = "ModelAge: Resid_fit") # New way using olsrr to view scatter plot of residuals
modelsAge_plot_resid_hist <- ols_plot_resid_hist(modelAge, main = "ModelAge: Resid_fit") # Histogram of residuals 
# visualize the model with visreg
visreg(modelAge, main = "ModelAge: birthYear Vs. gunControlA")
visreg(modelAge, "suburb", main = "ModelAge: Suburb Vs. age")
visreg(modelAge, "gunControlA", main = "ModelAge: age Vs. gunControlA")
stargazer(modelAge, type = "html", out = "ModelAge.html", title = "ModelAge") # Create Table




##########----------------------- LOGIT MODELS ------------------------######### 

## LOGIT MODEL 1: Does support for the gunControlA change as respondents move away 
#                 from the city in all states?

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~gunControlA + urbanCity, data = logitData)
logitData$urbanCity <- as.factor(logitData$urbanCity)
gunControlA_Logit <- glm(gunControlA ~ birthYear + gender + race + education + 
                           famIncome + state + voteReg + party + partyLevel + 
                           urbanCity, 
                         data = logitData, family = "binomial")
summary(gunControlA_Logit)

## CIs using profiled log-likelihood
confint(gunControlA_Logit)

## CIs using standard errors
confint.default(gunControlA_Logit)

# Chi-squared test for urbanCity --> X^2 = 59.8, df = 3, p-value - P(> X^2) = 6.4e-13
# - The overall effect of urbanCity is statistically significant.
wald.test(b = coef(gunControlA_Logit), Sigma = vcov(gunControlA_Logit), Terms =11:13)

## odds ratios only
exp(coef(gunControlA_Logit))

## odds ratios and 95% CI
exp(cbind(OR = coef(gunControlA_Logit), confint(gunControlA_Logit)))

# calculate the predicted probability of gunControlA at each value of urbanCity, 
# holding all other predictors at their means.
gunControlA_Logit_a <- with(logitData, data.frame(birthYear = mean(birthYear),
                                    gender = mean(gender),
                                    race = mean(race),
                                    education = mean(education),
                                    famIncome = mean(famIncome),
                                    state = mean(state),
                                    voteReg = mean(voteReg),
                                    party = mean(party),
                                    partyLevel = mean(partyLevel),
                                    urbanCity = factor(1:4)))

# calculate the predicted probabilities
gunControlA_Logit_a$rankP <- predict(gunControlA_Logit, newdata = gunControlA_Logit_a, type = "response")

# create a data frame of predicted probabilities across varying values of state and urbanCity
gunControlA_Logit_a <- with(logitData, data.frame(state = rep(seq(from = 1, to = 51, length.out = 100),4),
                                               birthYear = mean(birthYear),
                                               gender = mean(gender),
                                               race = mean(race),
                                               education = mean(education),
                                               famIncome = mean(famIncome),
                                               state = mean(state),
                                               voteReg = mean(voteReg),
                                               party = mean(party),
                                               partyLevel = mean(partyLevel),
                                               urbanCity = factor(1:4)))

# generate the predicted probabilities, with standard errors for CI
gunControlA_Logit_b <- cbind(gunControlA_Logit_a, predict(gunControlA_Logit, 
                                                          newdata = gunControlA_Logit_a, 
                                                          type = "link",
                                                          se = TRUE))

# get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
gunControlA_Logit_b <- within(gunControlA_Logit_b, 
                              {PredictedProb <- plogis(fit)
                              LL <- plogis(fit - (1.96 * se.fit))
                              UL <- plogis(fit + (1.96 * se.fit))
                              })

## view first few rows of final dataset
head(gunControlA_Logit_b)

# Plot the final model with ggplot2
gunControlA_Logit_Plot <- ggplot(gunControlA_Logit_b, aes(x = state, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = urbanCity), alpha = 0.2) + geom_line(aes(colour = urbanCity), size = 1)
gunControlA_Logit_Plot
# Add new legend
gunControlA_Logit_Plot <- gunControlA_Logit_Plot + scale_fill_discrete(name="95% Confidence Intervals",
                         breaks=c("1", "2", "3", "4"),
                         labels=c("City", "Suburb", "Town", "Rural Area"))
gunControlA_Logit_Plot
# Add title
gunControlA_Logit_Plot <- gunControlA_Logit_Plot + ggtitle("Does Support for gunControlA Change As Respondents\n Move Away From The City, Across All States?")

gunControlA_Logit_Plot

# Is the model a good fit?
# find the difference in deviance for the two models ---> 4669.597
with(gunControlA_Logit, null.deviance - deviance) 
# The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the model ---> 12
with(gunControlA_Logit, df.null - df.residual) 
# Find the p-value ---> 0
with(gunControlA_Logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) 
# log likelihood ---> -31204.98 (df=13)
logLik(gunControlA_Logit)

stargazer(gunControlA_Logit, type = "html", out = "gunControlA_Logit.html", title = "gunControlA_Logit",
          covariate.labels = c("birthYear", "gender", "race", "education", "famIncome", "state", 
                               "voteReg", "party","partyLevel", "suburb", "town", "rural area")) # Create Table

# ************ gunControlB Logit Models ************ 


## LOGIT MODEL 2: Does support for the gunControlB change as respondents move away 
#                 from the city in all states?

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~gunControlB + urbanCity, data = logitData)

gunControlB_Logit <- glm(gunControlB ~ birthYear + gender + race + education + 
                           famIncome + state + voteReg + party + partyLevel + 
                           urbanCity, 
                         data = logitData, family = "binomial")
summary(gunControlB_Logit)

## CIs using profiled log-likelihood
confint(gunControlB_Logit)

## CIs using standard errors
confint.default(gunControlB_Logit)

# Chi-squared test for urbanCity --> X^2 = 289.8, df = 3, p-value - P(> X^2) = 0.0
# - The overall effect of urbanCity is statistically significant.
wald.test(b = coef(gunControlB_Logit), Sigma = vcov(gunControlB_Logit), Terms =11:13)

## odds ratios only
exp(coef(gunControlB_Logit))

## odds ratios and 95% CI
exp(cbind(OR = coef(gunControlB_Logit), confint(gunControlB_Logit)))

# calculate the predicted probability of gunControlB at each value of urbanCity, 
# holding all other predictors at their means.
gunControlB_Logit_a <- with(logitData, data.frame(birthYear = mean(birthYear),
                                               gender = mean(gender),
                                               race = mean(race),
                                               education = mean(education),
                                               famIncome = mean(famIncome),
                                               state = mean(state),
                                               voteReg = mean(voteReg),
                                               party = mean(party),
                                               partyLevel = mean(partyLevel),
                                               urbanCity = factor(1:4)))

## view data frame
gunControlB_Logit_a
# calculate the predicted probabilities
gunControlB_Logit_a$rankP <- predict(gunControlB_Logit, newdata = gunControlB_Logit_a, type = "response")
gunControlB_Logit_a

# create a table of predicted probabilities across varying values of state and urbanCity
gunControlB_Logit_a <- with(logitData, data.frame(state = rep(seq(from = 1, to = 51, length.out = 100),4),
                                               birthYear = mean(birthYear),
                                               gender = mean(gender),
                                               race = mean(race),
                                               education = mean(education),
                                               famIncome = mean(famIncome),
                                               state = mean(state),
                                               voteReg = mean(voteReg),
                                               party = mean(party),
                                               partyLevel = mean(partyLevel),
                                               urbanCity = factor(1:4)))

# generate the predicted probabilities, with standard errors for CI
gunControlB_Logit_b <- cbind(gunControlB_Logit_a, predict(gunControlB_Logit, 
                                                          newdata = gunControlB_Logit_a, 
                                                          type = "link",
                                                          se = TRUE))

# get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
gunControlB_Logit_b <- within(gunControlB_Logit_b, 
                              {PredictedProb <- plogis(fit)
                              LL <- plogis(fit - (1.96 * se.fit))
                              UL <- plogis(fit + (1.96 * se.fit))
                              })

## view first few rows of final dataset
head(gunControlB_Logit_b)

# Plot the final model with ggplot2
gunControlB_Logit_Plot <- ggplot(gunControlB_Logit_b, aes(x = state, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = urbanCity), alpha = 0.2) + geom_line(aes(colour = urbanCity), size = 1)
gunControlB_Logit_Plot
# Add new legend
gunControlB_Logit_Plot <- gunControlB_Logit_Plot + scale_fill_discrete(name="95% Confidence Intervals",
                                                                       breaks=c("1", "2", "3", "4"),
                                                                       labels=c("City", "Suburb", "Town", "Rural Area"))
gunControlB_Logit_Plot
# Add title
gunControlB_Logit_Plot <- gunControlB_Logit_Plot + ggtitle("Does Support for gunControlB Change As Respondents\n Move Away From The City, Across All States?")
gunControlB_Logit_Plot

# Is the model a good fit?
# find the difference in deviance for the two models ---> 14521.38
with(gunControlB_Logit, null.deviance - deviance) 
# The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the model ---> 12
with(gunControlB_Logit, df.null - df.residual) 
# Find the p-value ---> 0
with(gunControlB_Logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) 
# log likelihood ---> -23727.35 (df=13)
logLik(gunControlB_Logit)

stargazer(gunControlB_Logit, type = "html", out = "gunControlB_Logit.html", title = "gunControlB_Logit",
          covariate.labels = c("birthYear", "gender", "race", "education", "famIncome", "state", 
                               "voteReg", "party","partyLevel", "suburb", "town", "rural area")) # Create Table
# ************ gunControlC Logit Models ************ 


## LOGIT MODEL 3: Does support for the gunControlC change as respondents move away 
#                 from the city in all states?

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~gunControlC + urbanCity, data = logitData)

gunControlC_Logit <- glm(gunControlC ~ birthYear + gender + race + education + 
                           famIncome + state + voteReg + party + partyLevel + 
                           urbanCity, 
                         data = logitData, family = "binomial")
summary(gunControlC_Logit)

## CIs using profiled log-likelihood
confint(gunControlC_Logit)

## CIs using standard errors
confint.default(gunControlC_Logit)

# Chi-squared test for urbanCity --> X^2 = 170.9, df = 3, p-value - P(> X^2) = 0.0
# - The overall effect of urbanCity is statistically significant.
wald.test(b = coef(gunControlC_Logit), Sigma = vcov(gunControlC_Logit), Terms =11:13)

## odds ratios only
exp(coef(gunControlC_Logit))

## odds ratios and 95% CI
exp(cbind(OR = coef(gunControlC_Logit), confint(gunControlC_Logit)))

# calculate the predicted probability of gunControlC at each value of urbanCity, 
# holding all other predictors at their means.
gunControlC_Logit_a <- with(logitData, data.frame(birthYear = mean(birthYear),
                                               gender = mean(gender),
                                               race = mean(race),
                                               education = mean(education),
                                               famIncome = mean(famIncome),
                                               state = mean(state),
                                               voteReg = mean(voteReg),
                                               party = mean(party),
                                               partyLevel = mean(partyLevel),
                                               urbanCity = factor(1:4)))

## view data frame
gunControlC_Logit_a
# calculate the predicted probabilities
gunControlC_Logit_a$rankP <- predict(gunControlC_Logit, newdata = gunControlC_Logit_a, type = "response")
gunControlC_Logit_a

# create a table of predicted probabilities across varying values of state and urbanCity
gunControlC_Logit_a <- with(logitData, data.frame(state = rep(seq(from = 1, to = 51, length.out = 100),4),
                                               birthYear = mean(birthYear),
                                               gender = mean(gender),
                                               race = mean(race),
                                               education = mean(education),
                                               famIncome = mean(famIncome),
                                               state = mean(state),
                                               voteReg = mean(voteReg),
                                               party = mean(party),
                                               partyLevel = mean(partyLevel),
                                               urbanCity = factor(1:4)))

# generate the predicted probabilities, with standard errors for CI
gunControlC_Logit_b <- cbind(gunControlC_Logit_a, predict(gunControlC_Logit, 
                                                          newdata = gunControlC_Logit_a, 
                                                          type = "link",
                                                          se = TRUE))

# get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
gunControlC_Logit_b <- within(gunControlC_Logit_b, 
                              {PredictedProb <- plogis(fit)
                              LL <- plogis(fit - (1.96 * se.fit))
                              UL <- plogis(fit + (1.96 * se.fit))
                              })

## view first few rows of final dataset
head(gunControlC_Logit_b)

# Plot the final model with ggplot2
gunControlC_Logit_Plot <- ggplot(gunControlC_Logit_b, aes(x = state, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = urbanCity), alpha = 0.2) + geom_line(aes(colour = urbanCity), size = 1)
gunControlC_Logit_Plot
# Add new legend
gunControlC_Logit_Plot <- gunControlC_Logit_Plot + scale_fill_discrete(name="95% Confidence Intervals",
                                                                                           breaks=c("1", "2", "3", "4"),
                                                                                           labels=c("City", "Suburb", "Town", "Rural Area"))
gunControlC_Logit_Plot
# Add title
gunControlC_Logit_Plot <- gunControlC_Logit_Plot + ggtitle("Does Support for gunControlC Change As Respondents\n Move Away From The City, Across All States?")
gunControlC_Logit_Plot

# Is the model a good fit?
# find the difference in deviance for the two models ---> 10405.1
with(gunControlC_Logit, null.deviance - deviance) 
# The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the model ---> 12
with(gunControlC_Logit, df.null - df.residual) 
# Find the p-value ---> 0
with(gunControlC_Logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) 
# log likelihood ---> -26124.36 (df=13)
logLik(gunControlC_Logit)

stargazer(gunControlC_Logit, type = "html", out = "gunControlC_Logit.html", title = "gunControlC_Logit",
          covariate.labels = c("birthYear", "gender", "race", "education", "famIncome", "state", 
                               "voteReg", "party","partyLevel", "suburb", "town", "rural area")) # Create Table

dev.off()
