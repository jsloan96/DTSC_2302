
#create pdf for plots
plots_pdf = pdf(file = 'Distribution_plots.pdf')

# Make new data frame with selected variables
myData <- data.frame(fullData$birthyr, 
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
colnames(myData) <- vars # rename columns
myData <- na.omit(myData) # Remove n/a's
myData <- subset(myData, myData$famIncome != 97) # remove "Prefer not to say" response
myData <- subset(myData, myData$urbanCity != 5) # remove "Other" response
myData <- subset(myData, myData$race != 7) # remove "Other" response
myData <- subset(myData, myData$voteReg != 3) # remove "Don't Know" response
myData <- subset(myData, myData$party < 4) # remove "Don't Know" response
# myData <- subset(myData, myData$partyLevel == c(1,2,6,7)) # remove "Don't Know" response


# Recode data according to code book 
myData$gender <- recode(myData$gender, '1'='Male', '2'='Female')
myData$race <- recode(myData$race, '1'='White', '2'='Black or African-American',
                      '3'='Hispanic or Latino', '4'='Asian or Asian-American',
                      '5'='Native American', '6'='Two or more races', '8'='Middle Eastern')
myData$education <- recode(myData$education, '1'='Did not graduate from high school', '2'='High school graduate',
                           '3'='Some college, but no degree (yet)', '4'='2-year college degree',
                           '5'='4-year college degree', '6'='Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)')
myData$famIncome <- recode(myData$famIncome, '1'='Less than $10,000', '2'='$10,000 - $19,999',
                           '3'='$20,000 - $29,999', '4'='$30,000 - $39,999',
                           '5'='$40,000 - $49,999', '6'='$50,000 - $59,999', '7'='$60,000 - $69,999',
                           '8'='$70,000 - $79,999','9'='$80,000 - $99,999','10'='$100,000 - $119,999',
                           '11'='$120,000 - $149,999','12'='$150,000 - $199,999','13'='$200,000 - $249,999',
                           '14'='$250,000 - $349,999','15'='$350,000 - $499,999','16'='$500,000 or more')
myData$urbanCity <- recode(myData$urbanCity, '1'='City', '2'='Suburb',
                           '3'='Town', '4'='Rural Area')
myData$voteReg <- recode(myData$voteReg, '1'='Yes', '2'='No')
myData$party <- recode(myData$party, '1'='Democrat', '2'='Republican',
                       '3'='Independent')
myData$partyLevel <- recode(myData$partyLevel, '1'='Strong Democrat', '2'='Not very strong Democrat',
                            '6'='Not very strong Republican', '7'='Strong Republican')


#             ---------- Modify Gun Control Questions ----------
#
# Recode the gun control responses to reflect the respondents position on an increase
# in gun control legislation.
# On the issue of gun regulation, do you support or oppose each of the following proposals?

# - gunControlA - 'Prohibit state and local governments from publishing the names 
#                  and addresses of all gun owners.'
#   - If a respondents supports this proposal, then they oppose an overall increase 
#     in gun control legislation.
myData$gunControlA <- recode(myData$gunControlA, '1'='Oppose', '2'='Support')
# myData$gunControlA<- ifelse(test=myData$gunControlA == 1, yes="Oppose", no="Support") 

# - gunControlB - Ban assault rifles.
#   - If a respondent supports this proposal, then they support an overall increase 
#     in gun control legislation.
myData$gunControlB <- recode(myData$gunControlB, '1'='Support', '2'='Oppose')
# myData$gunControlB<- ifelse(test=myData$gunControlB == 1, yes="Support", no="Oppose") 

# - gunControlC - Make it easier for people to obtain concealed-carry permit.
#   - If a respondent supports this proposal, then they oppose an overall increase 
#     in gun control legislation.
myData$gunControlC <- recode(myData$gunControlC, '1'='Oppose', '2'='Support')
# myData$gunControlC<- ifelse(test=myData$gunControlC == 1, yes="Oppose", no="Support") 

# Combine the responses from the three gun control statements into a single variable 
#   1 = respondent supported 1/3 gun control statements
#   2 = respondent supported 2/3 gun control statements
#   3 = respondent supported 3/3 gun control statements
myData$gunControlSupportLevel <- rowSums(cbind(myData$gunControlA, myData$gunControlB, myData$gunControlC) == "Support")

#             ---------- Import State Names ----------
# First, pull out all the unique values
states <- data.frame(id = unique(fullData$inputstate))
# Then use the dplyr package to sort them
states <- states %>% arrange(id)
# import state names
stnames <- read.csv("statenames.csv")
states$name <- stnames$State
states$newid <- stnames$stateid

myData <- merge(x=myData, y=states, by.x="state", by.y="id")

myData <- rename(myData, 'stateName' = 'name') # rename state column



#------------------ Models for Support from gunConttrolA -----------------------

# Subset data for only respondents that support gunControlA
supportGunControlA <- subset(myData, myData$gunControlA == "Support")

# Plot the proportions of each urban zone across all states
supportGunControlA_plot <- ggplot(supportGunControlA, aes(x=stateName, fill=urbanCity))
supportGunControlA_plot
supportGunControlA_plot <- supportGunControlA_plot + geom_bar(stat = "count", position = "fill") +
  labs(title="Proportion Of Respondents Who Support GunControlA In Each Urban Zone For Each US State",
       caption = 'GunControlA: Prohibit state and local governments from publishing the names and addresses of all gun owners.',
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent)

supportGunControlA_plot

#------------------ Models for Support from gunConttrolB -----------------------

# Subset data for only respondents that support gunControlB
supportGunControlB <- subset(myData, myData$gunControlB == 1)

# Plot Distribution of Support for gunControlB in each urban zone for each state
supportGunControlB_plot <- ggplot(supportGunControlB, aes(x=stateName, fill=urbanCity))
supportGunControlB_plot
supportGunControlB_plot <- supportGunControlB_plot + geom_bar(stat = "count", position = "fill") +
  labs(title="Proportion Of Respondents Who Support GunControlB In Each Urban Zone For Each US State",
       caption = "GunControlB: Ban assault rifles.",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent)

supportGunControlB_plot




#------------------ Models for Support from gunConttrolC -----------------------

# Subset data for only respondents that support gunControlC
supportGunControlC <- subset(myData, myData$gunControlC == "Support")

# Plot Distribution of Support for gunControlC in each urban zone for each state
supportGunControlC_plot <- ggplot(supportGunControlC, aes(x=stateName, fill=urbanCity))
supportGunControlC_plot
supportGunControlC_plot <- supportGunControlC_plot + geom_bar(stat = "count", position = "fill") +
  labs(title="Proportion Of Respondents Who Support GunControlC In Each Urban Zone For Each US State",
       caption = 'GunControlC: Make it easier for people to obtain concealed-carry permit.',
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent)

supportGunControlC_plot


# -- LINEAR MODEL --
modelAge <- lm(urbanCity ~ gender + race + education + famIncome + 
                 stateName + voteReg + party + partyLevel + gunControlA +
                 gunControlB, data = logitData)
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
visreg(modelAge, main = "ModelAge: birthYear Vs. gunControlC")
visreg(modelAge, "suburb", main = "ModelAge: Suburb Vs. age")
visreg(modelAge, "gunControlC", main = "ModelAge: age Vs. gunControlC")
stargazer(modelAge, type = "html", out = "ModelAge.html", title = "ModelAge") # Create Table


# New Model
modelAge <- lm(gunControlC ~ urbanCity + stateName,
               data = myData)
ols_regress(modelAge) # OLSR model of age
model_age <- ols_step_best_subset(modelAge) # Select the subset of predictors that do the best at meeting some well-defined objective criterion,
# such as having the largest R2 value or the smallest MSE, Mallow’s Cp or AIC.
model_age # view the olsrr output
plot(model_age) # plot predicted residuals (red line) vs actual residuals (blue dots)
# View Residual Plots
modelsAge_resid_qq <- ols_plot_resid_qq(modelAge) # Graph for detecting violation of normality assumption. Are predicted values close to actual values?
ols_test_normality(modelAge) # Test for detecting violation of normality assumption.
ols_test_correlation(modelAge) # Correlation between observed residuals and expected residuals under normality. - 0.9924921
modelsAge_plot_resid_fit <- ols_plot_resid_fit(modelAge, main = "ModelAge: Resid_fit") # New way using olsrr to view scatter plot of residuals
modelsAge_plot_resid_hist <- ols_plot_resid_hist(modelAge, main = "ModelAge: Resid_fit") # Histogram of residuals 
# visualize the model with visreg
visreg(modelAge, main = "ModelAge: birthYear Vs. gunControlC")
visreg(modelAge, "suburb", main = "ModelAge: Suburb Vs. age")
visreg(modelAge, "gunControlC", main = "ModelAge: age Vs. gunControlC")
stargazer(modelAge, type = "html", out = "ModelAge2.html", title = "ModelAge") # Create Table



##########----------------------- Extra Plots ------------------------########## 

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
dev.off()
