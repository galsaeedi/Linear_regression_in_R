

# To read the number with commas as thousand separator (Amount column in our data set)
setClass('num.with.commas')
setAs('character', 'num.with.commas', 
      function(from) as.numeric(gsub(',',"", from) ) )

# Read the data from csv file 
consumer <- read.csv("C:/Users/lenovo/Downloads/consumer.csv",  
                     colClasses=c('numeric','numeric','num.with.commas'))

# View the first few lines of the data 
head(consumer)

# Rename the column - to be easy to deal with 
colnames(consumer) # the old names
colnames(consumer)= c('income','household','charge')

# Descriptive statistics to summarize the data using summary() function, var and sd  
summary(consumer)

var_income = var(consumer$income)
var_household = var(consumer$household)
var_amount = var(consumer$charge)

var_income
var_household
var_amount

sd_income = sd(consumer$income)
sd_household = sd(consumer$household)
sd_amount = sd(consumer$charge)

sd_income
sd_household
sd_amount

# explore the relationship between annual income and annual credit card charges
plot(consumer$income, consumer$charge, 
     main="The relationship between annual income and annual credit card charges",
     xlab="Annual income", 
     ylab="Annual credit card charges", pch=19,
     col="#004766")

# check the correlation between income and charge
cor(consumer$income,consumer$charge)

# fit our simple linear regression model 1 (charge ~ income)
model1 <- lm (consumer$charge ~ consumer$income, data = consumer) 

# diagnose the model 
summary(model1)

# draw the least squares regression line
abline(model1, lwd = 2.5, 
       col = "#56B471") 

#explore the relationship between annual household size and annual credit card charges
plot(consumer$household, consumer$charge, 
     main="The relationship between household size and annual credit card charges",
     xlab="Household size", 
     ylab="Annual credit card charges", pch=19,
     col="#004766")

# check the correlation between the household size and charge 
cor(consumer$household,consumer$charge)

# fit the second simple linear regression model 2 (charge ~ household)
model2 <- lm (consumer$charge ~ consumer$household, data = consumer) 

# diagnose the model 
summary(model2)

# draw the least squares regression line
abline(model2, 
       lwd = 2.5, 
       col = "#56B471")

# check the correlation between the independent variables 
cor(consumer$household,consumer$income)

# check vif value in order to check the collinearity 
vif(multi_model)


# fit the multiple linear regression model with 2 variables 
multi_model <- lm (consumer$charge ~ consumer$income + consumer$household, data=consumer)

# diagnose the model 
summary(multi_model)

# predict the annual credit card charge for a three-person household with an annual income of $40,000
predicted_amount =multi_model$coefficients[1]+multi_model$coefficients[2]*40+multi_model$coefficients[3]*3
predicted_amount

