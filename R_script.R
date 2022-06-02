rm(list = ls())

getwd()

install.packages("car")
library(car)

setwd()
house_price <- readRDS("house_price.Rdata")
load("house_price.Rdata")
view(house_price)

attach(house_price)

# Housing prices vs square footage
plot(sqft, price, main="Home Price Based on Square Footage", xlab="Square feet", ylab="Price", pch=19)

# Regress housing prices on square footage. 
linearMod1 <- lm(price ~ sqft, data=house_price)
c1 <- linearMod1$coefficients
# Print model
lm1eqn <- paste("Home_Price =", 
            paste(round(c1[1],2), 
            paste(round(c1[-1],2), 
              names(c1[-1]), 
                sep="*", collapse=" + "), sep=" + "), "+ u")
lm1eqn

# Store the statistics summary and save as .txt
lm1sum <- summary(linearMod1)
sink(file = "lm1_output.txt")
lm1sum
sink(file = NULL)

##############################################

# Multivariate model
linearMod2 <- lm(price ~ sqft + beds + baths + intst + age, data=house_price)
c2 <- linearMod2$coefficients
# Print the model
lm2eqn <- paste("Home_Price =", 
                paste(round(c2[1],2), 
                paste(round(c2[-1],2), 
                  names(c2[-1]), 
                    sep="*", collapse=" + "), sep=" + "), "+ u")
lm2eqn

# Store the statistics summary and save as .txt
lm2sum <- summary(linearMod2)
sink(file = "lm2_output.txt")
lm2sum
sink(file = NULL)

# View multiple variables
avPlots(linearMod2)

##############################################

# Log model
linearMod3 <- lm(logprice ~ sqft + beds + baths + intst + age, data=house_price)
c3 <- linearMod3$coefficients
# Print the model
lm3eqn <- paste("logHome_Price =", 
                paste(round(c3[1],4), 
                      paste(round(c3[-1],4), 
                            names(c3[-1]), 
                              sep="*", collapse=" + "), sep=" + "), "+ u")
lm3eqn

# Store the statistics summary and save as .txt
lm3sum <- summary(linearMod3)
sink(file = "lm3_output.txt")
lm3sum
sink(file = NULL)

plot(sqft, logprice, main="Percent Change in Home Price Based on Square Footage", xlab="Square feet", ylab="Percent change in price", pch=19)


# You will need to create additional variables. For example, here I will create age squared
house_price$sqft2 <- house_price$sqft^2

# Log model different variables
linearMod4 <- lm(price ~ sqft + sqft2, data=house_price)
c4 <- linearMod4$coefficients
# Print the model
lm4eqn <- paste("Home_Price =", 
                paste(round(c4,2), 
                      paste(round(c4[-1],2), 
                            names(c4[-1]), 
                            sep="*", collapse=" + "), sep=" + "), "+ u")
lm4eqn


lm4sum <- summary(linearMod4)
sink(file = "lm4_output.txt")
lm4sum
sink(file = NULL)

linearMod4$fitted
# Predict prices
house_price$predictedprice <- predict(linearMod4, newdata=house_price)
# Add prices to dataframe
house_price$predicted1 <- 104035-440*house_price$age

attach(house_price)
plot(sqft, predictedprice, main="Home Price Based on Square Footage", xlab="Square feet", ylab="Price", pch=19)
abline(linearMod4)

library(ggplot2)
ggplot(house_price, aes(predictedprice, sqft)) + 
                geom_point(aes(size = 0.2)) + 
                geom_smooth()

avPlots(linearMod4)

