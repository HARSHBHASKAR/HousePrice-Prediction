# Importing data and Pre-processing
df = read.csv("C:/Users/HP/OneDrive/Desktop/Machine Learning(u)/Data Files/1. ST Academy - Crash course and Regression files/House_Price.csv", header = TRUE)
# here we have imported data onto dataframe.
View(df)
str(df) # this helps us to know the structure of df.
summary(df) # most imp to check for outliers/skewness and also whether categorical variables are present and also helps us to check for missing values.

#----------------------EDD ON DATA------------------------
summary(df)
#---- to see the distribution of numerical variables we use histogram(hist) or scatter plots
hist(df$crime_rate)
# to run scatter plots for all doubtful variables with primary variable.
pairs(~price + crime_rate + rainfall + n_hot_rooms, data = df)

# to run barplot on categorical variables
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

# Observations
# 1. Presence of Outliers in n_hot_rooms and rainfall.
# 2. crime_rate doesn't have a linear relationship with primary variable price.
# 3. bus_ter is a useless variable having common observation for all.
# 4. n_hos_beds: Shows less observations which arises 'NA' values.

#------------------------------ to handle the outliers-------------
pairs(~price + n_hot_rooms, data = df)
quantile(df$n_hot_rooms, 0.99)
uv = 3* quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[(df$n_hot_rooms>3*uv)] = 3*uv
str(df)
summary(df)

lv = quantile(df$rainfall, 0.01)
df$rainfall[(df$rainfall< 0.3*lv)] = 0.3*lv
summary(df$rainfall)

# ------------------------- now to handle our 4th observation i.e missing values of n_hos_beds ------------
str(df)
summary(df)

# Two things to check 1) where NA values is present, how to take mean of NA values
mean(df$n_hos_beds) # we get NA as it contains NA values.
mean(df$n_hos_beds, na.rm = TRUE) # this ignores NA values and then calculates mean and gives mean.
which(is.na(df$n_hos_beds)) # this gives the positions where 'NA' values are present.

df$n_hos_beds[is.na(df$n_hos_beds)] = mean(df$n_hos_beds, na.rm = TRUE)
summary(df)
str(df)
which(is.na(df$n_hos_beds)) #check

# Setting a Linear relationship between crime_rate and price.
pairs(~price + crime_rate, data = df)
#to plot scatter graph for only 2 variables we can use:
plot(df$price,df$crime_rate)
# here we see that the graph is more of like a Logarithmic graph.
df$crime_rate = log(1 + df$crime_rate)
#change: more linear relationship than before.
plot(df$price,df$crime_rate)

#now we will take avg_dist of dist1,dist2,dist3,dist4 and del them.
df$avg_dist = (df$dist1 + df$dist2+ df$dist3 + df$dist4)/4
# del dist1, dist2, dist3, dist4 : identifying their col no. in data.
#df[no. of rows,no. of col] , -col/-row means del that row.

df2 = df[,-7:-10]
df = df2
rm(df2) # remove df2

# now we will also del bus_ter (col no.: 14) as it does not add value to our model.
df = df[,-14]
View(df)

# Finally, we have solved all our 4 observations.

# Now we will deal with dummy values as Regression analysis works on only Numeric values.
# We will have to install a package of dummies for that!

install.packages("dummies")

df = dummy.data.frame(df)
df = df[,-9]
df = df[,-14]
# good to go, done with dummy creation.

# now we will create correlation matrix
cor(df)
# we will round to tidy up the matrix
round(cor(df),2)
# observation: 'parks' & 'air_qual' have high correlation coeff. so we will remove one after comparing both with our primary variable

df = df[,-16]
View(df)

# -------------------------- Linear Regression Model ----------------------------------------------------------------------

simple_model = lm(price~room_num, data = df)
summary(simple_model)

plot(df$room_num, df$price)
abline(simple_model)

str(df)

# ---------------------------- Multiple Regression Model --------------------------------------------------------------------

multiple_model = lm(price~., data = df) 
summary(multiple_model)

# ------------------------------- Test- Train split -----------------------------------------------------------

# to split data into test & train using a package caTools
install.packages("caTools")

# set.seed is used to observe random generators within our observation if that is equal to zero we get specific sequence always

set.seed(0)
split = sample.split(df, SplitRatio = 0.8)

# we have a variable named split with FALSE & TRUE we will assign TRUE to Train_set & FALSE..
# to test_set because we gave SplitRatio as 0.8 so all under 0.8 will be TRUE and vice versa

# THE SUBSET OF DF HAVING SPLIT AS TRUE MOVE TO TRAINING SET VARIABLE
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

lm_a = lm(price~., data = training_set)
summary(lm_a) # for reference

# we will find out the Mean Squared Error of training_set & train_set

train_a = predict(lm_a, training_set) # this will take all independent variable from training_set put it in lm_a and predict and store in train_a.
test_a = predict(lm_a, test_set)

# mean squared error (MSE) = [y(original) - y(predicted)]^2

mean((training_set$price - train_a)^2)
mean((test_set$price - test_a)^2)

# This estimated error is used when comparing models for better performance.

# Subset selection to improve prediction accuracy and model interpretability.
install.packages("leaps")

lm_best = regsubsets(price~., data = df, nvmax = 15)
summary(lm_best) # shows us all models of best subset
# to compare and get the best adj r2 model:
summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)
coef(lm_best, 7)

# now to run forward sub-selection

lm_forward = regsubsets(price~., data = df, nvmax = 15, method = "forward")
summary(lm_forward)
summary(lm_forward)$adjr2
which.max(summary(lm_forward)$adjr2)
coef(lm_forward, 7)

#now to run backward sub-selection

lm_backward = regsubsets(price~., data = df, nvmax = 15, method = "backward")
summary(lm_backward)

summary(lm_backward)$adjr2
which.max(summary(lm_backward)$adjr2)
coef(lm_backward, 7)

# ridge and lasso shrinkage method
# for ridge regression package is glmnet
install.packages("glmnet")

# for ridge regression segregate variable into dependent & independent variables
x = model.matrix(price~., data = df)[,-1] # for independent
y = df$price # for dependent variable

grid = 10^seq(10,-2,length = 100) # for lambda values
grid

lm_r = glmnet(x,y,alpha = 0, lambda = grid) # alpha =0 for Ridge & 1 for Lasso
summary(lm_r)

cv_fit = cv.glmnet(x,y,alpha = 0, lambda = grid) # here cv is cross validation to get best lambda value
plot(cv_fit)
# we want least MSE to get optimum value of lambda 

opt_lambda = cv_fit$lambda.min
# for finding r2 we need to find total sum of square (TSS)

tss = sum((y - mean(y))^2)
# now to find residual sum of square(RSS) we need predicted values of y
y_a = predict(lm_r, s = opt_lambda, newx = x)

rss = sum((y_a - y)^2)
 # --------------------------IMP R2 = (TSS - RSS)/TSS ----------------------
rsq = 1 - rss/tss # can be used to compare with other models

# now we do Lasso regression which is similiar to ridge

lm_l = glmnet(x,y,alpha = 1, lambda = grid) # alpha =0 for Ridge & 1 for Lasso
summary(lm_l)

cv_fitl = cv.glmnet(x,y,alpha = 1, lambda = grid) # here cv is cross validation to get best lambda value
plot(cv_fitl)
# we want least MSE to get optimum value of lambda 

op_lambda = cv_fitl$lambda.min
# for finding r2 we need to find total sum of square (TSS)

tss1 = sum((y - mean(y))^2)
# now to find residual sum of square(RSS) we need predicted values of y
y_a1 = predict(lm_l, s = op_lambda, newx = x)

rss1 = sum((y_a1 - y)^2)
rsq1 = 1 - rss1/tss












