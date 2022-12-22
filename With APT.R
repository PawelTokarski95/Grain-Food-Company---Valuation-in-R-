# Projekt 1 #

#In this particular project I will be testing the performance of the stock company
#with the 'INGR' ticker which is responsible for harvest and production of bread.
#Keeping that in mind, I will need the data of the prices of stocks, however I only
#have the data related to price and not with dividend included in it.
#Therefore I will be calculating the proper numbers and I will also provide a model
#that will test if the values of the stocks are overvaluated or undervaluated.
#The final performance measure will rely on dividend amount which I'll be calculating.

#I think it's a very vital problem due to current war circumstances
#and potential food supply shortages

#First of all, I will be downloading some common libraries. One is uncommon
#(in this case for example zoo which is responsible for date manipulation)
library(ggplot2)
library(stats)
library(zoo)
library(lmf)
library(imputeTS)
library(lmtest)
library(scales)



#I am downloading data for share prices and dividends for INGR and changing it's date format
Share_INGR <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/Share_Ingr.csv', sep = ';', header = TRUE)
x <- as.factor(Share_INGR[, 1])
y <- strptime(x,format="%d.%m.%Y")
z <- as.Date(y, format="%Y-%m-%d", origin = "2006-03-31")
Date <- z
Share_INGR <- data.frame(Date, Share_INGR)

Div_INGR <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/Divid_INGR.csv', sep = ';', header = TRUE)

#('rev' function is responsible for date transition (because previously it ended on 2007)).
Div_INGR <- apply(Div_INGR, 2, rev)
x <- as.factor(Div_INGR[, 1])
y <- strptime(x, format = '%m/%d/%y')
z <- as.Date(y, format="%Y-%m-%d")

Date <- z
Div_INGR <- data.frame(Date, Div_INGR[,-1])

Div_INGR[,2] <- as.numeric(Div_INGR[,2])

#Now I am multiplying matrices with using transposition also. After
#those transformations however, I will need to do some Linear Algebra
#mathematics in order to make a proper output (using diagonal matrix to 'store'
#data of dividend rate and extracting that in the next loop).
rDIV <- as.matrix(Div_INGR[,2]) %*% t(as.matrix(Share_INGR[,3]))

diagonal_matrix <- diag(1, 62,62)

result_DIV <- 0

j <- 1
i <- 1


for(x in 1:(62*62)) 
{
  
  if(diagonal_matrix[j, i]==1)
  {
  result_DIV[i] <- diagonal_matrix[j, i] * rDIV[j,i]
  }
  
  j = j + 1 
  
  if(j > 62)
  {
    j = 1
    i = i + 1
    
  }
  
  
  
}

#APT = alfa + b1*inflacja PPI + b2*zad³u¿enie w firmie + b3*zad³uzenie krotkoterminowe
# + b4*indeks gie³dowy + b5*stopa dywidendy + b6*stopy procentowe + b7*ceny indeksu zbo¿oweg
#I'm loading a variables to APT model. APT model is the more practical
#version of CAPM. It also check if the stocks are properly valuated, but only
#in the context of the stock markets conditions.

#In this case the used variables are:
#1. PPI (Producer Price Index)
#2. Interest_rates
#3. Longterm_debt (of this company)
#4. Shortterm_debt (of this company)

To_reg <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/Project_to_regression.csv', sep = ';', header = TRUE)

head(To_reg)

x <- as.factor(To_reg[, 1])
y <- strptime(x,format="%d.%m.%Y")
z <- as.Date(y, format="%Y-%m-%d", origin = "2006-03-31")

Date <- z

#---------------------------

#I used many other data manipulation techniques. One of these techniques
#is linear interpolation. It is a method to estimate the value of NA obs.
#by using a regression lines within.
To_reg <- data.frame(Date, To_reg[, 2:6])

x <- as.factor(To_reg[, 3])
y <- strptime(x,format="%d.%m.%Y")
z <- as.Date(y, format="%Y-%m-%d", origin = "2006-03-31")

Date2 <- z

To_reg_temporary <- apply(To_reg[,4:6], 2, rev)

To_reg <- data.frame(To_reg[,1:3], To_reg_temporary)

#To_reg[nrow(To_reg)+2,] <- NA

x <- zoo(as.numeric(To_reg[,5]))
x <- na_interpolation(x, option = "spline")

y <- zoo(as.numeric(To_reg[,6]))
y <- na_interpolation(y, option = "spline")


To_reg[, 5] <- x
To_reg[, 6] <- y

To_reg <- cbind(To_reg[1:62, 1:3], To_reg[1:62, 5:6])

#To_reg[,4] <- colnames(To_reg[,1])

#Now I'm downloading risk free rate - 10 Year US Bonds.
Rf <- read.csv('C:/Users/pawel/OneDrive/Pulpit/Projekt/10USBond.csv', sep = ';', header = TRUE)
#To_reg[,5] <- c(as.data.frame(To_reg[,5]))

result_DIV <- c(unlist(result_DIV))


To_APT <- data.frame(To_reg[,1], Rf[,2], To_reg[, 2:5])
APT <- lm(formula = result_DIV ~ To_APT[,2]+To_APT[,3]+To_APT[,4]+To_APT[,5]+To_APT[,6])
summary(APT)

#In the previus regression I've noticed that the second independent variable statisticaly
#insignificant, so I've omitted it.
APT_final <- lm(formula = result_DIV ~ To_APT[,2]+To_APT[,4]+To_APT[,5]+To_APT[,6])
summary(APT_final)

#Performance of the model is rather ok in case of R^2. It also doesn't imply
#the problem of heteroscedacitity (as shown in Breush-Pagan Test).

#Although it seems to be an autocorrelation of the residuals, it does not
#really matters that much. Afterall, the prices of stocks move up and down, so that's
#nothing special. It may also happen in the case of dividend if it's rate is fixed.
bptest(APT_final)


#It looks like the best independent variables are risk free rate, interest rates and longterm_debt
result_DIV <- data.frame(result_DIV)

#ggplot() + geom_point(data = result_DIV, mapping= aes(y=result_DIV, x=1:62)) + geom_line(DIV_Reg, mapping = aes(y=DIV_Reg, x=1:62))

#I assign fitted values of the model to the new variable
DIV_Reg_FIT <- APT_final$fitted.values


DIV_Reg_FIT <- data.frame(DIV_Reg_FIT)

colnames(result_DIV)  <- c('Dividend_Amount')
Dividend_Amount <- result_DIV
colnames(as.Date(Date)) <- c('Date')

#I create the plot in which there will be shown model fitted values vs actual values
ggplot() + geom_line(data = Dividend_Amount, mapping= aes(y=Dividend_Amount, x=Date)) + geom_smooth(data=DIV_Reg_FIT, mapping = aes(y=DIV_Reg_FIT, x=Date))
                                                                                                                                                                       


##### APT valuation ########
#-------------------------------------

#over_val <- sum(c(Observations > DIV_Reg_FIT))
#neutral_val <- sum(c(Observations == DIV_Reg_FIT))
#under_val <- sum(c(Observations< DIV_Reg_FIT))

To_var_over = 0
To_var_under = 0

#Finally I created the loop in which I'll be checking how many obs.
#are 'under' model values and how many are 'over' them in order to
#show the frequency of this up-down cycle.

i = 1
j= 1

Observations <- Dividend_Amount

for (i in 1:62) {
if(Observations[j,] > DIV_Reg_FIT[j,])
{
  
  To_var_over[j] <- Observations[j,]
  j = j + 1
  
} else {
  To_var_under[j] <- Observations[j,]
  j = j + 1
  
}
}


length(na.omit(To_var_over))
length(na.omit(To_var_under))
#It can be seen that the time periods in which the dividends were
#above the model prices were almost the same. I calculated
#the variability rate then.

var(Dividend_Amount[,1])^(1/2)/mean(Dividend_Amount[,1])
#I would say that in the terms of large relative variability (76%),
#I would personally don't buy those stocks.

