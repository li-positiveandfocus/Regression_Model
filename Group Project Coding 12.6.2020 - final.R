## Finance 2409/2509: Econometrics Group - Is living Space Size the Main Factor of a House's Price?

### Load data to R 

data <- read.csv("C:/Users/linji/Documents/2020fall_Finance 2409 Econometrics - MW 0315-0515PM/Group Project/House Sales Project/Final Report Submitted 12.06.2020/House_Price3.csv")

setwd("C:/Users/linji/Documents/2020fall_Finance 2409 Econometrics - MW 0315-0515PM/Group Project/House Sales Project/Final Report Submitted 12.06.2020/")

## Write the csv file
write.csv(data, "data2.csv")

## Load packages 

library(tidyverse) 
library(dplyr)
library(ggplot2)
library(car)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)
library(psych)
library(caTools)
library(stargazer)
library(lmtest)
library(sandwich)
library(ggeffects)
library(ggcorrplot)
library(corrr)
library(lattice)
library(erer)
library(doBy)


## View the dataset
str(data)
head(data)
stargazer(data, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")
dim(data)



#Lets plot the data to see if there are any outliers

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(3,4))

boxplot(x=data$sqft_living,xlab="Sqft_living",col=c('powderblue'))
boxplot(x=data$grade,xlab='Grade',col=c('powderblue'))
boxplot(x=data$view,xlab="View",col=c('powderblue'))
boxplot(x=data$condition,xlab='Condition',col=c('powderblue'))
boxplot(x=data$yr_built,xlab='yr_built',col=c('powderblue'))
boxplot(x=data$waterfront,xlab='Waterfront(Dummy)',col=c('powderblue'))



#From the above plot, we don't find any outlier values in the variables we are interested in. 
#However there is an outlier value in the bedrooms dataset: A house with 33 bedrooms, 1.75 bathrooms and 1,620 sqft-living. This is an error apparently. We decided to remove this ourlier.


# Remove Outlier

dim(data)
dim(data0)
data0 <- data[-15871,]

#remove <- data[row.names(data) == "15871",]
#data0 <- data[ !(row.names(data) %in% remove), ] (!Not Working)
#data0 <- subset(data, select =-15871) (!Not Working)

ggplot(data, aes(x=bedrooms, y=price)) + geom_point(col="blue") + geom_text(aes(label=row.names(data)), hjust=1, vjust=1)+
  labs(title = "Bedrooms and House Price(w/ Outlier)", x = "bedrooms", y = "price") +
  stat_smooth(method = "lm", col = "red", se=FALSE)+abline(lm(y~x, data = data), col="red")


ggplot(data0, aes(x=bedrooms, y=price)) + geom_point(col="blue") + geom_text(aes(label=row.names(data0)), hjust=1, vjust=1)+
  labs(title = "Bedrooms and House Price(w/t Outlier)", x = "Bedrooms", y = "price") +
  stat_smooth(method = "lm", col = "red", se=FALSE)+abline(lm(y~x, data = data0), col="red")

# As we can see from the below plots comparison, after removing the outlier, the plot line fits the data better.

#check for NA
 sapply(data0, function(x) sum(is.na(x)))
 #there is no null value in our dataset

#check for missing values 
 colSums(is.na(data0)) #check count of missing values before imputing
 #There is no missing value in our dataset

# Let's compute the correlation to see if any features are highly correlated.

   # Correlation between independent variables

    ggcorrplot(cor(data0, use="pairwise.complete.obs"), p.mat = cor_pmat(data0), 
           hc.order=FALSE, type='lower',lab=TRUE, lab_size=2.5)


### Conducting t-tests enables us to identify variables that are statistically significant.

    ## We perform two t-test :
  
    ##  **Test 1:** 
  
    ##  <i>T-test of all X variables against Variable of interest-living space(sqft_living)</i>

    lapply(data[,c("bedrooms", "bathrooms", "sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated")], function(x) anova(lm(x ~ data$sqft_living)))

    ##  **Test 2: **
  
    ##  <i>T-test of all X variables against Dependent Variables - price</i>
  

    lapply(data[,c("bedrooms", "bathrooms", "sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated")], function(x) anova(lm(x ~ data$price)))


    ##From the above t-tests, we saw that all the variables are signifcant variables against sqft_living.
    ##In addition, all variables are significant at 5% interval when tested against price. 

### Analysis


  #### How does our Variable of Interest affect our dependent variable?
  
  Plot_1  = ggplot(data0, aes(x=sqft_living, y=price)) + geom_point(col="blue")+ geom_text(aes(label=row.names(data0)), hjust=1, vjust=1)+
  labs(title = "Price vs Living_Space", x = "sqft_living", y = "price")+
  stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")+abline(lm(y~x, data = data0), col="red")

  print(Plot_1)

##From our expectations, an increase in the square feet of living space should increase the house price.


cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

l1 = lm(price~sqft_living, data=data0)
stargazer(l1, se=list(NULL), 
          column.labels=c("1"),
          title="Price and Living_space", type="text", 
          star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)


### In our base model, lets add some control variable

### Adding Control Variable: Grade

  Plot_2= ggplot(data0, aes(x=grade, y=price)) + geom_point(col="blue") + 
  labs(title = "Price vs Grade", x = "grade", y = "Price") +
  stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")

print(Plot_2)

   ##From the plot between grade and dependent variable price above, it appears houses with higher grade tend to have higher house prices.

   ##Lets see how grade relates to sqft_living (variable of interest).

   #cat("Correlation (X, Z):" 
cor(data0$Living_space, data0$grade)
anova(lm(sqft_living ~ grade, data=data0))

   ##With the anova test, we are trying to show how "Grade" relates to our variable of interest (sqft_living). From the results we can see that not only Grade is positively correlated with sqft_living, but also significant with 95% confidence. Thus establishing the relevance that corr (X, Z) > 0.


   ##Run Regression: 
  
cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

l1 = lm(price~sqft_living, data=data0)
l2 = lm(price~sqft_living+grade, data=data0)
stargazer(l1, l2, se=list(NULL, NULL), 
          column.labels=c("1", "2"),
          title="Price and Grade", type="text", 
          star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)
  
### Adding Control Variable:Age of House (yr_built)

  Plot_3= ggplot(data0, aes(x=yr_built, y=price)) + geom_point(col="blue") + 
  labs(title = "Price vs Age of House", x = "yr_built", y = "Price") +
  stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")
  +abline(lm(y~x, data = data0), col="red")

   print(Plot_3)

   ##From the plot between yr_built and dependent variable price, it appears houses built in more recent years tend to have higher house prices.

   ##Lets see how yr_built relates to sqft_living (variable of interest).

   #cat("Correlation (X, Z): 
cor(data0$sqft_living, data0$yr_built)
anova(lm(sqft_living ~ yr_built, data=data0))


   ##From the results we can see that not only yr_built is positively correlated with sqft_living, but also significant with 95% confidence. Thus establishing the relevance that corr (X, Z) > 0.

   ##Run Regression:

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

l1 = lm(price~sqft_living, data=data0)
l2 = lm(price~sqft_living+grade, data=data0)
l3 = lm(price~sqft_living+grade+yr_built, data=data0)
stargazer(l1, l2, l3,se=list(NULL, NULL, NULL), 
          column.labels=c("1", "2", "3"),
          title="Price and Living_spce", type="text", 
          star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)


### Adding Control Variable:View
    
    Plot_4= ggplot(data0, aes(x=view, y=price)) + geom_point(col="blue") + 
    labs(title = "Price vs view", x = "view", y = "Price") +
    stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")+abline(lm(y~x, data = data0), col="red")
  
    print(Plot_4)
  
    ##From the plot between View and dependent variable price, it appears the house with a greater number of views will have a higher price.

    ##Lets see how view relates to sqft_living (variable of interest).
  
    #cat("Correlation (X, Z)
    cor(data0$sqft_living, data0$view)
    anova(lm(sqft_living ~ view, data=data0))
  
    ##With the anova test, we are trying to show how 'view' relates to our variable of interest (sqft_living). From the results we can see that not only view is positively correlated with sqft_living,but also significant with 95% confidence. Thus establishing the relevance that corr (X, Z) > 0.
  
    ##Run Regression:
  
  cse=function(reg) {
    rob=sqrt(diag(vcovHC(reg, type="HC1")))
    return(rob)
  }
  
  l1 = lm(price~sqft_living, data=data0)
  l2 = lm(price~sqft_living+grade, data=data0)
  l3 = lm(price~sqft_living+grade+yr_built, data=data0)
  l4 = lm(price~sqft_living+grade+yr_built+view, data=data0)
  
  stargazer(l1, l2, l3,l4,se=list(NULL, NULL, NULL, NULL), 
            column.labels=c("1", "2", "3", "4"),
            title="Price and View", type="text", 
            star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)
  
### Adding Control Variable: Waterfront Dummy
      
    #change Waterfront to dummy variable

      data0$waterfront <- ifelse(data0$waterfront!=0, 1, 0)
      
      Plot_5= ggplot(data0, aes(x=waterfront, y=price)) + geom_point(col="blue") + 
      labs(title = "Price vs Waterfront Dummy", x = "waterfront", y = "Price") +
      stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")+abline(lm(y~x, data = data0), col="red")
    
    print(Plot_5)
    
##From the plot between Waterfront Dummy and dependent variable price, it appears the house with waterfront will have a higher price.

    ##Lets see how waterfront dummy relates to sqft_living (variable of interest).
    
    ##cat("Correlation (X, Z): 
    
    cor(data0$sqft_living, data0$waterfront)
    anova(lm(sqft_living ~ waterfront, data=data0))
    
    ##With the anova test, we are trying to show how 'waterfront dummy' relates to our variable of interest (sqft_living). From the results we can see that not only waterfront is positively correlated with sqft_living, but also significant with 95% confidence. Thus establishing the relevance that corr (X, Z) < 0.
    
    ## Run regression:
    
    cse=function(reg) {
      rob=sqrt(diag(vcovHC(reg, type="HC1")))
      return(rob)
    }
    l1 = lm(price~sqft_living, data=data0)
    l2 = lm(price~sqft_living+grade, data=data0)
    l3 = lm(price~sqft_living+grade+yr_built, data=data0)
    l4 = lm(price~sqft_living+grade+yr_built+view, data=data0)
    l5 = lm(price~sqft_living+grade+yr_built+view+waterfront, data=data0)
    
    stargazer(l1, l2, l3,l4,l5, se=list(NULL, NULL, NULL, NULL, NULL),
              column.labels=c("1", "2", "3", "4", "5"),
              title="Price and Condition", type="text", 
              star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)


### Adding Control Variable: Condition
    
      Plot_6= ggplot(data0, aes(x=condition, y=price)) + geom_point(col="blue") + 
      labs(title = "Price vs Condition", x = "condition", y = "Price") +
      stat_smooth(method = "lm", col = "red", se=FALSE, col = "green")+abline(lm(y~x, data = data0), col="red")
    
    print(Plot_6)
    
    ## From the plot between Condition and dependent variable price above, it appears the house with a higher grade of condition will have a better price.
    
    ##Lets see how Condition relates to sqft_living (variable of interest).
    
    ##cat("Correlation (X, Z):"
    cor(data0$sqft_living, data0$condition)
    anova(lm(sqft_living ~ condition, data=data0))
    
    ##With the anova test, we are trying to show how 'condition' relates to our variable of interest (sqft_living). From the results we can see that not only Condition is negatively correlated with sqft_living, but also significant with 95% confidence. Thus establishing the relevance that corr (X, Z) < 0.  	 
    
    ##Run regression:
      
    cse=function(reg) {
      rob=sqrt(diag(vcovHC(reg, type="HC1")))
      return(rob)
    }
    
    l1 = lm(price~sqft_living, data=data0)
    l2 = lm(price~sqft_living+grade, data=data0)
    l3 = lm(price~sqft_living+grade+yr_built, data=data0)
    l4 = lm(price~sqft_living+grade+yr_built+view, data=data0)
    l5 = lm(price~sqft_living+grade+yr_built+view+waterfront, data=data0)
    l6  = lm(price~sqft_living+grade+yr_built+viewwaterfront+condition, data=data0)
    stargazer(l1, l2, l3,l4,l5, l6,se=list(NULL, NULL, NULL, NULL, NULL, NULL),
              column.labels=c("1", "2", "3", "4", "5", "6"),
              title="Price and Condition", type="text", 
              star.cutoffs = c(0.05,0.01,0.001), df=FALSE, digits=3)
    
    
### Joint Hypothesis Testing Using the Chi-squared Test 
      
      
    ##Let's compare Model 5 and Model 6 using Chi- Square Test to find the better model . 

    anova(l5, l6, test = 'Chisq')
    

      
      
    
    