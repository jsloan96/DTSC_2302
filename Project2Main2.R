##################### Project 2 - DTSC 2302 - Spring 2022 ######################
# Research Question:  What are the contributing factors that affect US voters' #
#                     opinion on increased gun control legislation?            #
# Key Independent Variables: birthYear, gender, race, education, famIncome,    #
#                            urbanCity, voteReg, party, partyLevel.            #
# Key Dependent Variables: Response ("Support","Oppose") to 3 statements about # 
#                          gun control legislation.--> Combine all 3 responses #
#                          to new variable gunControlLevel.                    #
################################################################################

# Load Libraries 
library(readr)
library(visreg)
library(olsrr)
library(stargazer)
library(dplyr)
library(ggplot2)
# set working dirctory 


# import Data set
CES20_Common_OUTPUT_vv <- read_csv("~/Downloads/dataverse_files/CES20_Common_OUTPUT_vv.csv")

# Variables of Interest
vars <- c("birthYear", "gender", "race", "education", "famIncome", "state", "urbanCity", "voteReg", "party","partyLevel", "gunControlA", "gunControlB", "gunControlC")

#create pdf for plots
plots_pdf = pdf(file = 'Project2_plots.pdf')

# Make new data frame with selected variables
myData <- data.frame(CES20_Common_OUTPUT_vv$birthyr, 
                     CES20_Common_OUTPUT_vv$gender, 
                     CES20_Common_OUTPUT_vv$race,
                     CES20_Common_OUTPUT_vv$educ, 
                     CES20_Common_OUTPUT_vv$faminc_new, 
                     CES20_Common_OUTPUT_vv$inputstate, 
                     CES20_Common_OUTPUT_vv$urbancity, 
                     CES20_Common_OUTPUT_vv$votereg, 
                     CES20_Common_OUTPUT_vv$pid3, 
                     CES20_Common_OUTPUT_vv$pid7, 
                     CES20_Common_OUTPUT_vv$CC20_330a, 
                     CES20_Common_OUTPUT_vv$CC20_330b, 
                     CES20_Common_OUTPUT_vv$CC20_330c)
colnames(myData) <- vars # rename columns
myData <- na.omit(myData) # Remove n/a's
myData <- subset(myData, myData$famIncome != 97) # remove "Prefer not to say" response
myData <- subset(myData, myData$urbanCity != 5) # remove "Other" response


#             ---------- Modify Gun Control Questions ----------
#
# Recode the gun control responses: 0 - 'Opposes increased gun control' 
#                                   1 - 'Support increased gun control'
# On the issue of gun regulation, do you support or oppose each of the following proposals?:
# - gunControlA - 'Prohibit state and local governments from publishing the names 
#                  and addresses of all gun owners.'
#   - Re-code 1's to 0 and 2's to 1 - If a respondents supports this proposal, then 
#     they oppose an overall increase in gun control legislation.
myData$gunControlA<- ifelse(test=myData$gunControlA == 1, yes=0, no=1)  
# - gunControlB - Ban assault rifles.
#   - Re-code 2's to 0 and 1's to 0 - If a respondent supports this proposal, 
#     then they support an overall increase in gun control legislation.
myData$gunControlB<- ifelse(test=myData$gunControlB == 1, yes=1, no=0) 
# - gunControlC - Make it easier for people to obtain concealed-carry permit.
#   - Re-code 1's to 0 and 2's to 1 - If a respondent supports this proposal, 
#     then they oppose an overall increase in gun control legislation.
myData$gunControlC<- ifelse(test=myData$gunControlC == 1, yes=0, no=1) 

# Combine the responses from the three gun control statements into a single variable 
#   1 = respondent supported 1/3 gun control statements
#   2 = respondent supported 2/3 gun control statements
#   3 = respondent supported 3/3 gun control statements
myData$gunControlLevel <- rowSums(cbind(myData$gunControlA, myData$gunControlB, myData$gunControlC))


#             ---------- UrbanCity Dummy Variables ----------

# Make dummy variables for each response - add a new column for
myData$city <- ifelse(test=myData$urbanCity == 1, yes = 1, no = 0)
myData$suburb <- ifelse(test=myData$urbanCity == 2, yes = 1, no = 0)
myData$town <- ifelse(test=myData$urbanCity == 3, yes = 1, no = 0)
myData$ruralArea <- ifelse(test=myData$urbanCity == 4, yes = 1, no = 0)


##########----------------------- LINEAR MODELS -----------------------######### 


# Linear Model Age: 
modelAge <- lm(birthYear ~ gender + race + education + famIncome + 
                 state + voteReg + party + partyLevel + suburb + 
                 town + ruralArea + gunControlA + gunControlB,
               data = myData)
ols_regress(modelAge) # OLSR model of age
model_age <- ols_step_best_subset(modelAge) # Select the subset of predictors that do the best at meeting some well-defined objective criterion,
# such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
model_age # view the olsrr output
plot(model_age) # plot predicted residuals (red line) vs actual residuals (blue dots)
# View Residual Plots
modelsAge_resid_qq <- ols_plot_resid_qq(modelAge) # Graph for detecting violation of normality assumption. Are predicted values close to actual values?
ols_test_normality(modelAge) # Test for detecting violation of normality assumption.
ols_test_correlation(modelAge) # Correlation between observed residuals and expected residuals under normality.
modelsAge_plot_resid_fit <- ols_plot_resid_fit(modelAge) # New way using olsrr to view scatter plot of residuals
modelsAge_plot_resid_hist <- ols_plot_resid_hist(modelAge) # Histogram of residuals 
# visualize the model with visreg
visreg(modelAge, "birthYear", main = "Model_1: birthYear Vs. gunControlA")
visreg(modelAge, "suburb", main = "ModelAge: Suburb Vs. age")
visreg(modelAge, "gunControlA", main = "ModelAge: age Vs. gunControlA")
stargazer(modelAge, type = "html", out = "ModelAge.html", title = "ModelAge") # Create Table



# Linear Model 1: Hypothesis: As respondents move further away from the city, 
#                             their support towards gunControlA will rise.
#                 Ho: The variance is constant.
#                 Ha: The variance is inconstant.
#                 Dependent Variable: gunControlA 
#                 Independent Variables: birthyear, gender, race, education, famIncome,
#                                        state, voteReg, party, partyLevel, suburb, town, ruralArea.
model_1 <- lm(gunControlA ~  birthYear + gender + race + education + famIncome + 
                state + voteReg + party + partyLevel + suburb + town + ruralArea,
              data=myData )
summary(model_1)

model_1a <- ols_step_best_subset(model_1) # new olsrr console output for linear models
model_1a # view the olsrr output
plot(model_1a) # plot predicted residuals (red line) vs actual residuals (blue dots)

# View Residual Plots
ols_plot_resid_qq(model_1) # plot does not looks good, residuals are  not close to prediction
ols_test_normality(model_1)
ols_test_correlation(model_1) # correlation between residuals we should get vs residuals we actually got

# New way using olsrr to view scatter plot of residuals
ols_plot_resid_fit(model_1)

# Histogram of residuals 
ols_plot_resid_hist(model_1_olsr)

# visualize the model with visreg
visreg(model_1, "birthYear", main = "Model_1: birthYear Vs. gunControlA")
visreg(model_1, "suburb", main = "Model_1: Suburb Vs. gunControlA")
visreg(model_1, "town", main = "Model_1: Town Vs. gunControlA")
visreg(model_1, "ruralArea", main = "Model_1: Rural Area Vs. gunControlA")
# plot residuals to check if the model is valid
plot(model_1$fitted.values, model_1$residuals, main = "Model_1: Fitted Values vs. Residuals")
# -> Residual plot shows a stripping pattern ---> Model may not be accurate

# Conclusion: As the respondents move farther away from the city, there is a slight
#             increase in the gunControlA proposal. 



# Linear Model 2: Hypothesis: As respondents move further away from the city, 
#                             their support towards gunControlB will fall.

model_2 <- lm(gunControlB ~  birthYear + gender + race + education + famIncome + 
                state + voteReg + party + partyLevel + suburb + town + ruralArea,
              data=myData )
summary(model_2)
# visualize the model with visreg
visreg(model_2, "suburb", main = "Model_2: Suburb Vs. gunControlB")
visreg(model_2, "town", main = "Model_2: Town Vs. gunControlB")
visreg(model_2, "ruralArea", main = "Model_2: Rural Area Vs. gunControlB")
# plot residuals to check if the model is valid
plot(model_2$fitted.values, model_2$residuals, main = "Model_2: Fitted Values vs. Residuals")
# -> Residual plot shows a stripping pattern ---> Model may not be accurate

# Conclusion: As the respondents move farther way from the city, there is a slight
#             decrease in support for the gunControlB proposal. 



# Linear Model 3: Hypothesis: As respondents move further away from the city, 
#                             their support towards gunControlC will rise.

model_3 <- lm(gunControlC ~  birthYear + gender + race + education + famIncome + 
                state + voteReg + party + partyLevel + suburb + town + ruralArea,
              data=myData )
summary(model_3)
visreg(model_3, "education", main = "Model_3: Education Vs. gunControlC")
visreg(model_3, "birthYear", main = "Model_3: Birth Year Vs. gunControlC")
visreg(model_3, "suburb", main = "Model_3: Suburb Vs. gunControlC")
visreg(model_3, "town", main = "Model_3: Town Vs. gunControlC")
visreg(model_3, "ruralArea", main = "Model_3: Rural Area Vs. gunControlC")
# plot residuals to check if the model is valid
plot(model_3$fitted.values, model_3$residuals, main = "Model_3: Fitted Values vs. Residuals")
# -> Residual plot shows a stripping pattern ---> Model may not be accurate

# Conclusion: There is no relationship between how far from the city a respondent
#             lives, and their support for the gunControlC proposal.



# Linear Model 4: Hypothesis: As respondents get older, their support towards 

model_4 <- lm(birthYear ~ gender + race + education + famIncome + 
                state + voteReg + party + partyLevel + gunControlA + gunControlB,
              data=myData )
summary(model_4)
# visualize the model with visreg
visreg(model_4, "partyLevel", main = "Model_4: partyLevel Vs. Birth Year")
visreg(model_4, "education", main = "Model_4: Education Vs. Birth Year")
visreg(model_4, "state", main = "Model_4: state Vs. Birth Year")
visreg(model_4, "gunControlA", main = "Model_4: gunControlA Vs. Birth Year")
visreg(model_4, "gunControlB", main = "Model_4: gunControlB Vs. Birth Year")
# plot residuals to check if the model is valid
plot(model_4$fitted.values, model_4$residuals, main = "Model_4: Fitted Values vs. Residuals")
# -> Residual plot shows  ---> Model may not be accurate
# Conclusion:


################################################################################

# Extra Plots ----------------------------------------------------------------->

# Visualize model_1 with visreg
visreg(model_1, "birthYear", main = "birthYear Vs. Party Residuals")
visreg(model_1, "gender", main = "Gender Vs. Party Residuals")
visreg(model_1, "race", main = "Race Vs. Party Residuals")
visreg(model_1, "education", main = "Education Vs. Party Residuals")
visreg(model_1, "famIncome", main = "FamIncome Vs. Party Residuals")
visreg(model_1, "state", main = "State Vs. Party Residuals")
visreg(model_1, "voteReg", main = "VoteReg Vs. Party Residuals")
visreg(model_1, "party", main = "BirthYear Vs. Party Residuals")
visreg(model_1, "partyLevel", main = "PartyLevel Vs. Party Residuals")



table(myData$birthYear)
hist(myData$birthYear, main = "Histogram of Birthyear" ,  xlab = "Birthyear")

myData$gender <- as.factor(myData$gender)
table(myData$gender)
plot(myData$gender, main = "Histogram of Gender",  xlab = "Gender")

table(myData$race)
hist(myData$race, main = "Histogram of Race" ,  xlab = "Race")

table(myData$education)
hist(myData$education, main = "Histogram of Education" ,  xlab = "Education")

table(myData$fam_income)
hist(myData$fam_income[myData$fam_income != 97], main = "Histogram of Famliy Income",  
     xlab = "Family Income")

table(myData$state)
hist(myData$state, main = "Histogram of State" ,  xlab = "State")

table(myData$urbanCity)
hist(myData$urbanCity, main = "Histogram of UrbanCity" ,  xlab = "Home Location")

table(myData$votereg)
hist(myData$votereg, main = "Histogram of Voter Registration Status" ,  xlab = "Voter Registration Status")

table(myData$party)
hist(myData$party, main = "Histogram of Party" ,  xlab = "Party")

table(myData$partyLevel)
hist(myData$partyLevel, main = "Histogram of Level of Support of Political Party" ,  
     xlab = "Voter Registration Status")

table(myData$gunControlA)
hist(myData$gunControlA, main = "Histogram of gunControlA",
     xlab = "Prohibit state and local governments from publishing\n the names and addresses of all gun owners")

table(myData$gunControlB)
hist(myData$gunControlB, main = "Histogram of gunControlB",
     xlab = "Ban assault rifles")

table(myData$gunControlC)
hist(myData$gunControlC, main = "Histogram of gunControlC",
     xlab = "Make it easier for people to obtain concealed-carry permit")
# <-----------------------------------------------------------------------------
state1 <- subset(myData, state == 1)
state2 <- subset(myData, state == 2)
state4 <- subset(myData, state == 4)
state5 <- subset(myData, state == 5)
state6 <- subset(myData, state == 6)
state8 <- subset(myData, state == 8)
state9 <- subset(myData, state == 9)
state10 <- subset(myData, state == 10)
state11 <- subset(myData, state == 11)
state12 <- subset(myData, state == 12)
state13 <- subset(myData, state == 13)
state15 <- subset(myData, state == 15)
state16 <- subset(myData, state == 16)

state <-read.csv("statenames.csv")

length(myData$state[myData$state == 1] && myData$gunControlA[myData$gunControlA == 0])

tobeaggregated <- myData %>% select(state, gunControlA, gunControlB, gunControlC, urbanCity, education, partyLevel,birthYear)

guna <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$gunControlA), margin = 1))
guna <- guna %>% rename(stateid = Var1, gunA = Var2, stateperA = Freq)
guna <- merge(x = state, y = guna, by.x = "stateid", by.y = "stateid")
supa <- filter(guna, gunA %in% c(1))
oppa <- filter(guna, gunA %in% c(0))

supOpp <-c(supa$stateperA, oppa$stateperA)
barplot(supOpp, xlab = "state")

gunb <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$gunControlB), margin = 1))
gunb <- gunb %>% rename(stateid = Var1, gunB = Var2, stateperB = Freq)
gunb <- merge(x = state, y = gunb, by.x = "stateid", by.y = "stateid")
supb <- filter(gunb, gunB %in% c(1))

gunc <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$gunControlC), margin = 1))
gunc <- gunc %>% rename(stateid = Var1, gunC = Var2, stateperC = Freq)
gunc <- merge(x =  state, y = gunc, by.x = "stateid", by.y = "stateid")
supc <- filter(gunc, gunC %in% c(1))

ggplot(supa, aes(x=stateperA)) + geom_histogram(bins=30) + labs(title="Proportion of Respondents who support GunA for Each US State",
       x="Percent of Respondents in 2020 CCES",
       y="Number of states")

allparty <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$partyLevel), margin = 1))
allparty <- allparty %>% rename(stateid = Var1, partylevel = Var2, perstate = Freq)
allparty <- merge(x =  state, y = allparty , by.x = "stateid", by.y = "stateid")
gop <- filter(allparty, partylevel %in% c(5,6,7))
gop <- aggregate(gop$perstate, by=list(gop$stateid), FUN = sum)
gop <- gop %>% rename(stateid = Group.1, percentrep = x)
dem <- filter(allparty, partylevel %in% c(1,2,3))
dem <- aggregate(dem$perstate, by=list(dem$stateid), FUN = sum)
dem <- dem %>% rename(stateid = Group.1, percentdem = x)

urban <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$urbanCity), margin = 1))
urban <- urban %>% rename(stateid = Var1, urbancity = Var2, perstate = Freq)
city <- filter(urban, urbancity %in% c(1))
city <- city %>% rename( percentcity = perstate)
rural <- filter(urban, urbancity %in% c(4))
rural <- rural %>% rename( percentrural = perstate)

educ <- data.frame(prop.table(table(tobeaggregated$state, tobeaggregated$education), margin = 1))
educ <- educ %>% rename(stateid = Var1, educationlevel = Var2, perstate = Freq)
coldeg <- filter(educ, educationlevel %in% c(5,6))
coldeg <- aggregate(coldeg$perstate, by=list(coldeg$stateid), FUN = sum)
coldeg <- coldeg %>% rename(stateid = Group.1, percentcoldeg = x)

avgbirthyear <- aggregate(tobeaggregated$birthYear,by=list(tobeaggregated$state), FUN = mean)
avgbirthyear$avgAge <- (2020- avgbirthyear$x)
avgbirthyear <- avgbirthyear %>% rename(stateid = Group.1, avgby = x)

finaldata <- data.frame(supa)
finaldata$stateperB <- supb$stateperB
finaldata$stateperC <- supc$stateperC
finaldata$percentrep <- gop$percentrep
finaldata$percentdem <- dem$percentdem
finaldata$percentcity <- city$percentcity
finaldata$percentrural <- rural$percentrural
finaldata$percentcoldeg <- coldeg$percentcoldeg
finaldata$avgAge <- avgbirthyear$avgAge

finalmodela <- lm(stateperA ~ percentrep + percentdem + percentcity + percentrural + percentcoldeg + avgAge, data = finaldata)
summary(finalmodela)

visreg(finalmodela)

plot(finalmodela$fitted.values, finalmodela$residuals)

finalmodelb <- lm(stateperB ~ percentrep + percentdem + percentcity + percentrural + percentcoldeg + avgAge, data = finaldata)
summary(finalmodelb)

visreg(finalmodelb)
plot(finalmodelb$fitted.values, finalmodelb$residuals)

finalmodelc <- lm(stateperC ~ percentrep + percentdem + percentcity + percentrural + percentcoldeg + avgAge, data = finaldata)
summary(finalmodelc)

plot(finalmodelc$fitted.values, finalmodelc$residuals)

visreg(finalmodelc)

ols_step_all_possible(finalmodelc)





