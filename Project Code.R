#Load required librararies
library("readxl")
library("faraway")
library("margins")
library("ggplot2")


#Read excel file into R and remove unnecessary columns
ZillowRaw <- read_excel("C:\\Users\\tyler\\OneDrive - Johns Hopkins\\Theory of Statistics - Course Project\\Zillow Data.xlsx")
Zillow <- ZillowRaw[,-c(1,3,9,10,11,13,14)]

#Create logistic regression model for the data
Lreg <- glm(Y ~ ., family = binomial, data = Zillow)

#Test data to ensure calculations are correct
m1 <- margins(Lreg)
m2 <- margins(Lreg, at = list(SQFT = mean(Zillow$SQFT),
                             BED = mean(Zillow$BED),
                             BATH = mean(Zillow$BATH),
                             TRANS = mean(Zillow$TRANS),
                             WALK = mean(Zillow$WALK),
                             SCHL = mean(Zillow$SCHL)))

#Equation for probability of outcome occuring
pi <- expression(1/(1 + exp(-(-11.6723 + (SQFT * 0.00156509) + (BED * 0.0257601) + (BATH * 1.28668)
    + (TRANS * -0.0277313) + (WALK * 0.0852487) + (SCHL * -0.224243)))))

#Take the derivative of the equation for pi with respect to each variable
derivSQFT <- D(pi, 'SQFT')
derivBED <- D(pi, 'BED')
derivBATH <- D(pi, 'BATH')
derivTRANS <- D(pi, 'TRANS')
derivWALK <- D(pi, 'WALK')
derivSCHL <- D(pi, 'SCHL')



#Choose the values that the derivative will be evaluated at
SQFT <- mean(Zillow$SQFT)
BED <- mean(Zillow$BED)
BATH <- mean(Zillow$BATH)
TRANS <- mean(Zillow$TRANS)
WALK <- mean(Zillow$WALK)
SCHL <- mean(Zillow$SCHL)

#Calculate marginal effects at the means for each variable by evaluating each derivative
margeffectSQFT <- eval(derivSQFT)
margeffectBED <- eval(derivBED)
margeffectBATH <- eval(derivBATH)
margeffectTRANS <- eval(derivTRANS)
margeffectWALK <- eval(derivWALK)
margeffectSCHL <- eval(derivSCHL)

#Plot price increase vs BED variable
plot(ZillowRaw$BED, ZillowRaw$PRICE, pch = 19, xlab = 'BED', ylab = 'PRICE')