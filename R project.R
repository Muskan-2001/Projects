#----------------------------------Multiple Linear Regression Modelling--------------------------------#

#Problem Statement: 

#To examine which are the factors which influene,  Customer Lifetime Value (CLV) of Insurance Premium Company, based on the given attributes of the customer
#Business Context: What are the  attributes of customer, who have a higher Customer Lifetime Value

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#--------------------------------Setting the Working Directory-----------------------------------------#

data=read.csv("C:/Users/Sushil/Downloads/Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
View(data)
data1=data#To create a backup of original data

#-------------------------------Basic Exploration of data --------------------------------------------#
str(data1)
summary(data1)
dim(data1)

attach(data1)
rename(data1,"clv"=Customer.Lifetime.Value)

#-------------->Outlier Treatment through quantile method --------------------------------------------#

# Outliers with quantile
# Visualization of outliers
boxplot(data1$clv,main="Checking Outliers",col="blue")

quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

data2=data1[data1$clv <36000,]

nrow(data1)

nrow(data2)

nrow(data1)-nrow(data2)

quantile(data2$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

boxplot(data2$clv,main="After removing some outliers",col="red")

data3=data2[data2$clv < 14722,]

quantile(data3$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

boxplot(data3$clv, main="Removing the outliers",col="pink")

nrow(data1)-nrow(data3)
str(data3)

#-------------->Missing values Identification and Treatment----------------------------------#

as.data.frame(colSums(is.na(data3)))

#-------------->Droping the rendundant variables from the data frame--------------------------#
as.data.frame(colnames(data3))
data4=select(data3,-c(Customer,State,Effective.To.Date))
View(data4)
str(data4)#final dataset for modelling

# Changing the data format
colnames(data4)
str(data4)

# Changing
data4$Reponse<-as.factor(data4$Response)
data4$Coverage<-as.factor(data4$Coverage)
data4$Education<-as.factor(data4$Education)
data4$EmploymentStatus<-as.factor(data4$EmploymentStatus)
data4$Location.Code<-as.factor(data4$Location.Code)
data4$Marital.Status<-as.factor(data4$Marital.Status)
data4$Policy<-as.factor(data4$Policy)
data4$Policy.Type<-as.factor(data4$Policy.Type)
data4$Renew.Offer.Type<-as.factor(data4$Renew.Offer.Type)  
data4$Sales.Channel<-as.factor(data4$Sales.Channel)
data4$Vehicle.Class<-as.factor(data4$Vehicle.Class)
data4$Vehicle.Size<-as.factor(data4$Vehicle.Size)
str(data4)

# checking the level  of numeric data types
table(data4$Number.of.Open.Complaints)

#Exporting the treated data into csv file

write.csv(data4,"MLdata.csv")

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(123)
spl = sample.split(data4$clv, 0.7)

original.data = subset(data4, spl == TRUE)
str(original.data)

dim(original.data)

test.data = subset(data4, spl == FALSE)
str(test.data)
dim(test.data)

#------------------------------------------Fitting the model---------------------------------------#
#Iteration.1 We start with testing all variables

# y = B + B1x1+b2x2
LinearModel0=lm(clv~.,data=original.data)
summary(LinearModel0)
colnames(original.data)

#Removing the first two insignificant variables: policy and policy types
#Iteration.2.
LinearModel1=lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=original.data)
summary(LinearModel1)

#Removing the insignificant variables: Total Claim, Sales Channels, Policy Special, Months Since Policy Inception, Marital  Status, Location, Income
#Iteration.3.
LinearModel2=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Vehicle.Class, data=original.data)
summary(LinearModel2)

#Removing the insignificant classes in dummy variables
#Iteration.4.
LinearModel3=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV")+ I(Vehicle.Size == "Small"), data=original.data)
summary(LinearModel3)

#Iteration.5.
LinearModel4=lm(clv~	Coverage+	Education+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)
summary(LinearModel4)

#Iteration.6.
LinearModel5=lm(clv~	Coverage+	I(Education=="College")+I(Education =="High School or Below")+	EmploymentStatus+	 Monthly.Premium.Auto+	Months.Since.Last.Claim+		Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)
summary(LinearModel5)

##---------------------------------------------------Final Model

FinalModel=lm(clv~	Coverage+	I(EmploymentStatus=="Medical Leave")+	 Monthly.Premium.Auto + Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	I(Vehicle.Class=="SUV"), data=original.data)

summary(FinalModel)
#Months.Since.Last.Claim
#+I(EmploymentStatus =="Retired")

#Checking Multicollinearity in the model

## Get the predicted or fitted values
vif(FinalModel)


## Get the predicted or fitted values
fitted(FinalModel)

par(mfrow=c(2,2))
plot(FinalModel)

## MAPE
original.data$pred <- fitted(FinalModel)
write.csv(original.data,"mape.csv")

#Calculating MAPE
attach(original.data)
MAPE<-print((sum((abs(clv-pred))/clv))/nrow(original.data))

############ Residual Analysis ############################################################################

res <- original.data

res$stu_res <- studres(FinalModel) ##Studentized residuals
res$stud.del.resids <- rstudent(FinalModel) ##studentized deleted residuals
res$leverage <- hatvalues(FinalModel) ## leverage values (hi)
res$cooks_dis <- cooks.distance(FinalModel) ## Cook's distance
res$dffits <- dffits(FinalModel) ## Dffit
res$dfbetas <- dfbetas(FinalModel) ## Dfbetas
res$cov_ratio <- covratio(FinalModel) ## Covariance Ratio

write.csv(res,"res.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated 
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

durbinWatsonTest(FinalModel)
dwt(FinalModel)
#Since, the p-value is >0.05, we fail to reject H0: (No Autocorrelation)

# Checking multicollinearity
vif(FinalModel) # should be within 2.

################ Constant error variance ##########Heteroscedasticity

# Breusch-Pagan test
bptest(FinalModel)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)

#Cook-Weisberg test
# hypothesis of constant error variance against the alternative that the error variance changes with the level of the  response 
# p value should be more than 0.05
ncvTest(lm(clv~ 	Coverage +	I(Education=="College") 
           + I(EmploymentStatus=="Medical Leave")+ I(EmploymentStatus=="Retired")+ I(EmploymentStatus=="Unemployed")
           +Monthly.Premium.Auto  +	Number.of.Open.Complaints +	Number.of.Policies +	Renew.Offer.Type 
           +	I(Vehicle.Class=="SUV")  , data=original.data))


## Normality testing Null hypothesis is data is normal.

resids <- FinalModel$residuals

ad.test(resids) #get Anderson-Darling test for normality 
lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 

qqnorm(resids)
