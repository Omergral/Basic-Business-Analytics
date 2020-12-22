####1####

#cleaning the data enviroment
rm(list=ls())

#loading the data
setwd("C:/Users/Omer Gralnik/Desktop/Business Analysis/Datasets")
getwd() #verifying that the work directory is correct
list.files()
kc <- read.csv("kc_house.csv", header = T)

#checking the data variables and a summary of the data
names(kc)
summary(kc)
str(kc)


#performing summary statistics
quantile(kc$bathrooms)
prop.table(table(kc$floors))
by(kc$price,kc$bedrooms, mean)
prop.table(table(kc$condition))
quantile(kc$grade,probs = seq(0,1,0.1))

#converting the relevant numeric variables to categorial ones
kc$condition <- as.factor(kc$condition)
class(kc$condition) #verification
kc$floors <- as.factor(kc$floors)
class(kc$floors)
kc$grade <- as.factor(kc$grade)
class(kc$grade)

#install.packages("stargazer")
library(stargazer)

#generating a summary stat table
#stargazer(kc, out = 'housesSummary.html') #for the first time using the code take of the '#' mark

#setting the dates correctly
kc$date <- as.Date(kc$date, format = "%Y%m%d")
#let's look at the final outcome of the new variable
str(kc$date)
summary(kc$date)


####2####

#let's subset the data
names(kc)#checking again the variable names

#the relevant subset
kc_new <- subset(kc, select = c(price, bedrooms,bathrooms,sqft_living,sqft_lot ))

#correlation matrix
cor(kc_new, use = 'complete.obs')
#stargazer(correlation, out = 'Correlation_matrix.html')

#more subsetting for practice
#Although we could have combine all those subsets into one
kc_new <- subset(kc_new, bedrooms >= 4)
summary(kc_new$bedrooms)
length(kc_new$bedrooms)

kc_new <- subset(kc_new, bathrooms >= 2)
summary(kc_new$bathrooms)
length(kc_new$bathrooms)




####3####


#install.packages("data.table")
library(data.table) #loading the data.table package

kc_data <- data.table(kc)#generating a data.table data frame

##Condition##

#generating a new data table for each variable's mean and sd by every condition level
mean_sd_condition <- kc_data[,                       
        c(lapply(.SD, mean),
          lapply(.SD, sd)),
        by = .(condition),           
        .SDcols = c("price", "sqft_living","sqft_lot","bedrooms")]

#fixing the duplicate names problem with the following command
setnames(mean_sd_condition,make.unique(names(mean_sd_condition)[1:9]))
#setting new names so that it would be clear wich one is the mean and which is the sd
setnames(mean_sd_condition,names(mean_sd_condition[1:9]),
         c('Condition',"price_mean", "sqft_living_mean","sqft_lot_mean","bedrooms_mean",
           "price_sd", "sqft_living_sd","sqft_lot_sd","bedrooms_sd"))


##Waterfront##

#now I will perform the same steps as for the Condition variable but for the Waterfront variable
mean_sd_waterfront <- kc_data[,                       
                             c(lapply(.SD, mean),
                               lapply(.SD, sd)),
                             by = .(waterfront),           
                             .SDcols = c("price", "sqft_living","sqft_lot","bedrooms")]
setnames(mean_sd_waterfront,make.unique(names(mean_sd_waterfront)[1:9]))
setnames(mean_sd_waterfront,names(mean_sd_waterfront[1:9]),
         c('waterfront',"price_mean", "sqft_living_mean","sqft_lot_mean","bedrooms_mean",
           "price_sd", "sqft_living_sd","sqft_lot_sd","bedrooms_sd"))


##Combined##

#now the variable's mean and sd for waterfront and non-waterfront houses in each level of condition
mean_sd_combined <- kc_data[,                       
                              c(lapply(.SD, mean),
                                lapply(.SD, sd)),
                              by = .(condition,waterfront),           
                              .SDcols = c("price", "sqft_living","sqft_lot","bedrooms")]
setnames(mean_sd_combined,make.unique(names(mean_sd_combined)[1:10]))
setnames(mean_sd_combined,names(mean_sd_combined[1:10]),
         c('condition','waterfront',"price_mean", "sqft_living_mean","sqft_lot_mean","bedrooms_mean",
           "price_sd", "sqft_living_sd","sqft_lot_sd","bedrooms_sd"))



####4####


#install.packages('ggplot2')
library(ggplot2)


#scatter plot of Price vs. Sqft_living 
ggplot(kc)+aes(x = sqft_living,y = price)+
  geom_point()+
  labs(title = "Price vs. Sqft_living")


#adding a linear regression line
ggplot(kc)+aes(x = sqft_living,y = price)+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title = "Price vs. Sqft_living")
  

#adding color by each condition level
ggplot(kc)+aes(x = sqft_living,y = price , color = as.factor(condition))+
  geom_point()+
  scale_colour_manual(values =c("1" = "red", "2" = "blue",
                                "3" = "darkgreen", "4" = "orange",
                                "5" = 'pink', 'NA'= 'gray'))+
  geom_smooth(method=lm, color= 'black')+
  labs(title = "Price vs. Sqft_living", color = "Condition")


#adding a linear regression line for each level of condition
ggplot(kc)+aes(x = sqft_living,y = price , color = as.factor(condition))+
  geom_point()+
  scale_colour_manual(values =c("1" = "red", "2" = "blue",
                                "3" = "darkgreen", "4" = "orange",
                                "5" = 'pink', 'NA'= 'gray'))+
  geom_smooth(method=lm, fill = NA)+
  labs(title = "Price vs. Sqft_living", color = "Condition")


#scatter plot of price vs. sqft_living for waterfront and non-waterfront house
ggplot(kc)+aes(x = sqft_living,y = price)+
  geom_point()+
  facet_wrap(~waterfront, labeller = as_labeller(c('1'='Waterfront','0'='Non-Waterfront')))+
  geom_smooth(method = lm, fill = NA)


#Histogram of the price variable
ggplot(kc, aes(x=price)) +
  geom_histogram(fill="blue",col="black",bins=20)+
  labs(title = 'Price Histogram')


#Boxplot of the price variable
ggplot(kc , aes(x=price)) +
  geom_boxplot(fill="pink",col="black", outlier.color = 'red')+
  labs(title = 'Price boxplot')


#Boxplot of the price variable for each level of condition
ggplot(kc , aes(x=price, color = as.factor(condition))) +
  geom_boxplot()+
  labs(title = 'Price boxplot by each level of condition' ,color = 'Condition')


#Barplot for the mean price in each level of condition
ggplot(mean_sd_condition , aes(x=Condition,y=price_mean, fill = as.factor(Condition))) +
  geom_bar(stat="identity", width=0.5)+
  labs(title = "Price's mean in each level of condition",
       fill = 'Condition')+
  ylab('Price mean')+
  xlab('Level of condition')
  

#creating a new data frame with the mean price for each number of bathrooms
mean_bathrooms <- kc_data[,lapply(.SD, mean), by = .(bathrooms),.SDcols = 'price']

#Barplot of the mean price for each number of bathrooms
ggplot(mean_bathrooms , aes(x=bathrooms ,y=price, fill = as.factor(bathrooms))) +
  geom_bar(stat="identity")+
  labs(title = "Price's mean by number of bathrooms",
       fill = 'bathrooms')+
  ylab('Price mean')+
  xlab('number of bathrooms')+
  scale_x_continuous(breaks = seq(0,8,0.5))






     