# Load required libraries
library(ggplot2)
library(stats)
library(zoo)
library(lmf)
library(imputeTS)
library(lmtest)
library(scales)

# Read in share price data and change date format
Share_INGR <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/Share_Ingr.csv', sep = ';', header = TRUE)
Share_INGR$Date <- as.Date(strptime(as.character(Share_INGR$Date), format = "%d.%m.%Y"), format = "%Y-%m-%d", origin = "2006-03-31")

# Read in dividend data and change date format
Div_INGR <- read.csv("C:/Users/pawel/OneDrive/Pulpit/Projekt/Div_INGR.csv", sep = ';', header = TRUE)
Div_INGR <- apply(Div_INGR, 2, rev)
Div_INGR <- as.data.frame(Div_INGR)
Div_INGR$Date <- as.Date(strptime(as.character(Div_INGR$Date), format = '%m/%d/%y'), format = "%Y-%m-%d")
Div_INGR$Dividend <- as.numeric(Div_INGR$Dividend)

# Multiply matrices and extract values from diagonal matrix
rDIV <- as.matrix(Div_INGR$Dividend) %*% t(as.matrix(Share_INGR$Price))
diagonal_matrix <- diag(1, 62, 62)
result_DIV <- 0
j <- 1
i <- 1
for(x in 1:(62 * 62)) {
  if(diagonal_matrix[j, i] == 1) {
    result_DIV[i] <- diagonal_matrix[j, i] * rDIV[j, i]
  }
  j <- j + 1
  if(j > 62) {
    j <- 1
    i <- i + 1
  }
}
To_reg <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/Project_to_regression.csv', sep = ';', header = TRUE)

x <- as.factor(To_reg[, 1])
y <- strptime(x,format="%d.%m.%Y")
z <- as.Date(y, format="%Y-%m-%d", origin = "2006-03-31")
Date <- z


# I used many other data manipulation techniques. One of these techniques is linear interpolation. 
#It is a method to estimate the value of NA obs, by using a regression lines within.

To_reg <- data.frame(Date, To_reg[, 2:6])
x <- as.factor(To_reg[, 3])
y <- strptime(x,format="%d.%m.%Y")
z <- as.Date(y, format="%Y-%m-%d", origin = "2006-03-31")

Date2 <- z

To_reg_temporary <- apply(To_reg[,4:6], 2, rev)
To_reg <- data.frame(To_reg[,1:3], To_reg_temporary)

x <- zoo(as.numeric(To_reg[,5]))
x <- na_interpolation(x, option = "spline")

y <- zoo(as.numeric(To_reg[,6]))
y <- na_interpolation(y, option = "spline")

To_reg[, 5] <- x
To_reg[, 6] <- y

To_reg <- cbind(To_reg[1:62, 1:3], To_reg[1:62, 5:6])



#Now I'm downloading risk free rate - 10 Year US Bond

Rf <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/10USBond.csv', sep = ';', header = TRUE)

 

result_DIV <- c(unlist(result_DIV))

 

To_APT <- data.frame(To_reg[,1], Rf[,2], To_reg[, 2:5])

 

#Name of variables used in the models are:
Risk_free_rate <- To_APT[,2]
PPI <- To_APT[,3]
Interest_rates <- To_APT[,4]
Longterm_debt  <- To_APT[,5]
Shortterm_debt <- To_APT[,6]
 
APT <- lm(formula = result_DIV ~ Risk_free_rate+PPI+Interest_rates+Longterm_debt+Shortterm_debt)
summary(APT)



#In the previous regression, I noticed that the second independent variable (PPI) was statistically insignificant, so I omitted it.
APT_2 <- lm(formula = result_DIV ~ Risk_free_rate+Interest_rates+Longterm_debt+Shortterm_debt)
summary(APT_2)



bptest(APT_2)
#The performance of the model is quite good in terms of R^2. It also does not imply the problem of heteroscedasticity (as shown in the Breusch-Pagan test with a p-value of 0.44)
#Additionally, I do not have to worry about the issue of autocorrelation because it is not a predictive model, but rather only shows certain dependencies over a period of time.

#It looks like the best independent variables for the model are the risk-free rate, interest rates, long-term debt, and short-term debt.
result_DIV <- data.frame(result_DIV)


#I have assigned the fitted values of the model to a new variable and created a plot to show the model's fitted values versus the actual values.
DIV_Reg_FIT <- APT_2$fitted.values
DIV_Reg_FIT <- data.frame(DIV_Reg_FIT)

colnames(result_DIV) <- c('Dividend_Amount')
Dividend_Amount <- result_DIV
colnames(as.Date(Date)) <- c('Date')


ggplot() + geom_line(data = Dividend_Amount, mapping= aes(y=Dividend_Amount, x=Date)) + geom_smooth(data=DIV_Reg_FIT, mapping = aes(y=DIV_Reg_FIT, x=Date))

#I have also created a loop to check how many observations are "under" the model values and how many are "over" them in order to show the frequency of this up-down cycle.
To_var_over = 0
To_var_under = 0
Observations = Dividend_Amount

j=1
for (i in 1:62) {
  if (Observations[j,] > DIV_Reg_FIT[j,]) {
    To_var_over[j] = Observations[j,]
  } else {
    To_var_under[j] = Observations[j,]
  }
  j=j+1
  
}

num_over = length(na.omit(To_var_over))
num_under = length(na.omit(To_var_under))

cat("Number of observations over model values:", num_over, "\n")
cat("Number of observations under model values:", num_under, "\n")


#It appears that the time periods in which the dividends were above the model prices were almost the same. I calculated the variability rate and found that it was relatively high, at 76%. 

var(Dividend_Amount[,1])^(1/2)/mean(Dividend_Amount[,1])

#Based on this, I would not personally recommend buying these stocks due to their high level of variability.





