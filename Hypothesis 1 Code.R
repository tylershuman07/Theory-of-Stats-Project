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

#Equation for probability of outcome occuring
pi <- expression(1/(1 + exp(-(-11.6723 + (SQFT * 0.00156509) + (BED * 0.0257601) + (BATH * 1.28668)
                              + (TRANS * -0.0277313) + (WALK * 0.0852487) + (SCHL * -0.224243)))))

#Take the derivative of the equation for pi with respect to each variable
derivTRANS <- D(pi, 'TRANS')




#Choose the values that the derivative will be evaluated at
BED <- mean(Zillow$BED)
BATH <- mean(Zillow$BATH)
TRANS <- mean(Zillow$TRANS)
WALK <- mean(Zillow$WALK)
SCHL <- mean(Zillow$SCHL)

#Calculate marginal effects at the means for each variable by evaluating each derivative
margeffectTRANS <- c()
quants <- quantile(Zillow$SQFT,c(0.25,0.375,0.5,0.625,0.75))

for (i in 1:5){
    SQFT <- quants[[i]]
    val <- eval(derivTRANS)
    margeffectTRANS <- append(margeffectTRANS, val)
}
plot(quants,margeffectTRANS, xlab = 'SQFT', ylab = 'TRANS Marginal Effect Size', ylim = c(-0.0080,0))