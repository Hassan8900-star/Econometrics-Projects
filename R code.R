# All code used (meaningful and progressive)
load("C:/Users/syedh/Documents/ECO375 winter/CondoData.Rda") #this directory will be different for everyone, depending on where your data is saved
head(D)
dim(D)
names(D)
summary(D) # Better understanding the dataset


D$den1 <- as.numeric(D$den)
sum(D$den1)

D$pool1 <- as.numeric(D$pool)
sum(D$pool1)

D$hottub1 <- as.numeric(D$hottub)
sum(D$hottub1)

D$gym1 <- as.numeric(D$gym)
sum(D$gym1)

D$movieroom1 <- as.numeric(D$movieroom)
sum(D$movieroom1)

D$pet1 <- as.numeric(D$pet)
sum(D$pet1)

D$parking1 <- as.numeric(D$parking)
sum(D$parking1)

D$studio1 <- as.numeric(D$studio)
sum(D$studio1)

D$footage1 <- as.numeric(D$footage)
sum(D$footage1)

D$age1 <- as.numeric(D$age)
sum(D$age1)

D$bathrooms1 <- as.numeric(D$bathrooms)
sum(D$bathrooms1)

D$bedrooms1 <- as.numeric(D$bathrooms)
sum(D$bedrooms1)

# Since most variables in my data set are character variables, I convert them to numeric form by using the function (for the Den variable, for example): D$den_numeric <- as.numeric(D$den) and then sum(D$den_numeric) to count the number of condos that have a den from the sample.

#no of dens = 153/315
#no of pools = 216/315
#no of hottubs = 216/315
#no of gyms = 307/315
#no of movierooms = 312/315
#no of pets = 315/315 #excluding this variable because this maximum occurrence means that MLR3 is violated
#no of parkings = 169/315
#no of studios = 35/315

# I will not be adding studio1 because (1) it is not an amenity; (2) I am not interested in analyzing that variable; (3) even though CLT applies as sample size is over 30, it only represents a small proportion of my sample (35/315).

# Running a preliminary model with select variables, variable selection justified in report (general model: capturing the effect of amenities on price of Toronto condos, with respect to age).
mod0 <- lm(D$price~D$footage1+D$maintenance+D$den1+D$bathrooms1+D$hottub1+D$bedrooms1+D$parking1+D$age1+D$pool1+D$pet1+D$movieroom1)
summary(mod0)

# The bedrooms1 variable did not return a coefficient upon regressing against price, so it will be taken out; so will the pet1 variable for the same reason.
# Single variable regression results. I will not include the footage1 variable because the den1 variable captures the concept of 'extra space' that the footage1 variable captures.

# I will now specify amenities. From the sample, I will assume there are 8 variables for amenities: 
# den1, pool1, hottub1, gym1, movieroom1, pet1, parking1, bathrooms1 (I include bathrooms1 because there are some units with more than 1 bathrooms, which becomes a kind of an amenity and its effect is crucial to study).
# The maintenance1 variable will be included in my regression considering it is the cost of maintaining these amenities.

# Now, I will be running regressions and simultaneously correcting my model
mod1=lm(D$price~D$age1) #(restricted_model)
summary(mod1)
mod2=lm(D$price~D$age1+D$maintenance+D$bathrooms1+D$den1+D$pool1+D$hottub1+D$gym1+D$movieroom1+D$parking1) #(unrestricted_model)
summary(mod2)

# I found all of my variables to be significant with three stars, except hottub1, gym1 and movieroom1  variables. So I exclude them from my regression model.
# The den1 variable is only significant with 1 star, however I will keep the variable as it can be assumed to capture the effect of both the footage1 and den1 variables.

# The hottub1 variable was taken out due to its perfect correlation with pool1 (all condos with a pool had a hottub as well). Thus, I chose to keep pool1 and remove hottub1.
# It appears that there is also multicollinearity between gym1 and movieroom1. Thus I plan on excluding movieroom1 after testing for correlation between them.

# Running the correlation analysis now.

corr_vars <- D[, c("price", "maintenance", "parking1", "bathrooms1", "den1", "pool1", "gym1", "age1", "movieroom1", "hottub1")]
corr_matrix <- cor(corr_vars)
print(corr_matrix)

# gym1 & movieroom1, pool1 & hottub1 had significant correlations so I will remove gym1 now.
# I have 5 regressors left
# Now I have all significant regressors, so I can perform an F test for joint significance

mod1=lm(D$price~D$age1)
summary(mod1)

mod2 = lm(D$price~D$maintenance+D$den1+D$bathrooms1+D$parking1+D$age1+D$pool1) # New unrestricted regression without gym1, hottub1, movieroom1
summary(mod2)

# Measuring an enhanced effect of the age1 variable on price by squaring age1 and adding it to the model.

D$age1_sq <- D$age1^2 # Creating an age1 squared variable.

mod3=lm(D$price~D$age1_sq+D$age1+D$maintenance+D$den1+D$bathrooms1+D$parking1+D$pool1)
summary(mod3)

# The model is significant, however den1 loses its significance at the 5% alpha level.
# I can exclude it but I am hesitant because the variable incorporates the effect of both footage and den given how I am interpreting it.
# However, I believe that there might be multicollinearity between age1 and age1_sq so I will test that.

install.packages("car") # Downloading the package to perform the VIF test.
library(car) # To load the package onto R.

vif(mod3) # Input the command.

# As I suspected, the vif between age1 and age1_sq is above 5, which will be my cutoff (VIF > 5) for excluding variables from the model.
# However, it is important to recognize that age1 and age_1 sq will have a high VIF by construction. Foundationally, this is not a concern because when the regression is performed, you only interpret these variables one at a time.


f_test1 <- anova(mod3, mod1)
print(f_test1)

# With a significant p value of 3 stars, the test proves that my model is jointly significant, and that all regressors have a significant effect on predicting price.

# Now moving on to the MLR assumptions.

# 1. Linearity of the model
# We can assume the model is linear considering the OLS produced significant regressors.

# 2. Randomness of the sample, which can be assumed as the data collection process was managed externally.

# 3. No perfect multicollinearity, verified by the VIF test in code line 111 as vif(mod3); pet1 was excluded due to issues for non-variation;
# The high VIF between age1 and age1_sq will be considered as an exception, as stated on line 114.

# 4. To test MLR4, producing a residual plot 

predictors <- data.frame(D$age1, D$age1_sq, D$den1, D$bathrooms1, D$parking1, D$pool1, D$maintenance) # Creating a data frame of predictors (fitted values).
with(predictors, plot(mod3$fitted.values, mod3$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot")) # The with function allows R to use the variables of the 'predictors' data frame as distinct colomns.
abline(lm(mod3$residuals ~ mod3$fitted.values), col = "red") # To fit a line of best fit in red color.

# The line of best fit is centered on zero, which means that the residuals are dispersed fairly on either side of the line, proving MLR4.

# 5. Heteroskedasticity of the residuals (test performed using lmtest package).

install.packages("lmtest")
library(lmtest)
bptest(mod3) # The Breusch-Pagan test facilitates the testing of heretoskedasticity of the errors. This is important because under the Gauss-Markov theorem, OLS is not efficient when the model's errors are heteroskedastic.

# A p-value close to zero suggests the null hypothesis of homoskedasticity is rejected, suggesting heteroskedasticity. 
# Need to correct this by transforming all regressors.

hist(mod3$residuals) # Checking the direction of the skew.
qqnorm(mod3$residuals)
qqline(mod3$residuals)

# Residuals appear to be negatively skewed (left skew).
# To fix this, I will log transform all my x variables except age1_sq, as it is already a transformation.

D$log_maintenance <- log(D$maintenance)
D$log_den1 <- log(D$den1)
D$log_parking1 <- log(D$parking1)
D$log_bathrooms1 <- log(D$bathrooms1)
D$log_age1 <- log(D$age1)
D$log_pool1 <- log(D$pool1)
D$log_age1_sq <- log(D$age1_sq)

# I ran the regression with all above mentioned variables, however some log-transformed variables returned negative infinity values
# To correct this, I selectively choose x values to be transformed. Now running the regression again.

logged_mod2 = lm(D$price~D$log_maintenance+D$den1+D$parking1+D$bathrooms1+D$log_age1+D$pool1+D$log_age1_sq) 
summary(logged_mod2) # This checks out (although den1 lost its significance); now I will test if this fixed the issue of heteroskedasticity.
bptest(logged_mod2) # Fails the test.
hist(logged_mod2$residuals)
qqnorm(logged_mod2$residuals)
qqline(logged_mod2$residuals) # The residuals still appear to be skewed.

# Thus I will use robust standard errors.

# Install and load the sandwich package.

install.packages("sandwich")
library(sandwich)

mod3_robust <- vcovHC(mod3, type="HC0") # This gives me a heteroskedasticity-consistent covariance  matrix, which has robust standard errors.
coeftest(mod3, vcov=mod3_robust) # The coeftest tests if the estimators are zero as its null. Since all my estimators are significant, I will check if they are identical to the previous model.
# Closely mirrors mod3, with adjusted standard errors, MLR5 satisfied.

# 6. Normality of the error term (MLR6), testing using a density plot.

mod3_res <- resid(mod3)
qqnorm(mod3_res)
qqline(mod3_res)
plot(density(mod3_res), main = "Density Plot of mod3's residuals")

# With a sample size of 315, it appears that the residuals are almost normally distributed. As the n -> infinity, we can argue that MLR6 holds.

# IMPORTANT TO NOTE THAT SINCE MLR 1 - 4 HOLD, MODEL IS UNBIASED (efficient).
# AS n -> infinity, CONSISTENCY AND ASYMPTOTIC NORMALITY CAN BE ACHIEVED.

