#Load required librararies
library("readxl")
library("faraway")
library("margins")
library("ggplot2")


#Read excel file into R and remove unnecessary columns
ZillowRaw <- read_excel("C:\\Users\\tyler\\OneDrive - Johns Hopkins\\Theory of Statistics - Course Project\\Zillow Data.xlsx")
Zillow <- ZillowRaw[,-c(1,3,9,10,11,13,14)]

#Create logistic regression model for the data to obtain logistic regression coefficients
Lreg <- glm(Y ~ ., family = binomial, data = Zillow)

#Use logistic regression coefficients to create equation for probability of outcome occurring
pi <- expression(1/(1 + exp(-(-11.6723 + (SQFT * 0.00156509) + (BED * 0.0257601) + (BATH * 1.28668)
                              + (TRANS * -0.0277313) + (WALK * 0.0852487) + (SCHL * -0.224243)))))

#Take the derivative of the equation for pi with respect to each variable
derivWALK <- D(pi, 'WALK')




#Choose the values that the derivative will be evaluated at
WALK <- mean(Zillow$WALK)
BATH <- mean(Zillow$BATH)
TRANS <- mean(Zillow$TRANS)
SCHL <- mean(Zillow$SCHL)
SQFT <- mean(Zillow$SQFT)

#Calculate marginal effect
margeffectWALK <- c()

for (i in c(3,3.5,4,4.5,5)){
    BED <- i
    val <- eval(derivWALK)
    margeffectWALK <- append(margeffectWALK, val)
}
plot(c(3,3.5,4,4.5,5),margeffectWALK, ylim = c(.02,.021), ylab = 'Marginal Effect of WALK Variable', xlab = 'BED')