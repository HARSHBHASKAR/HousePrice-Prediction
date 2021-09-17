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


















