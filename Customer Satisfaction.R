
# Variable description

#age -> numric
#job-> Type of job (categorical)
#marital-> Marital status
#education-> Level of education of the customer
#default-> has credit in default? (binary: "yes","no")
#balance-> average yearly balance, in euros (numeric)
#housing-> has housing loan? (binary: "yes","no")
#loan-> has personal loan? (binary: "yes","no")
#contact-> contact communication type (categorical: "unknown","telephone","cellular")
#day-> last contact day of the month (numeric)
#month-> last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#duration-> last contact duration, in seconds (numeric), important
#campaign-> number of contacts performed during this campaign and for this client (numeric, includes last contact)
#pdays-> number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#previous-> number of contacts performed before this campaign and for this client (numeric)
#poutcome-> outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")
#y-> has the client subscribed a term deposit? (binary : 0, 1)

# Library

library(ggplot2)
library(scales)
library(C50)
library(gmodels)
library(cwhmisc)

# Load the data 

df <- read.csv2("YOUR_PATH"/bank_customer_survey.csv",sep=",")


######################### Exploratory analysis ################################################

summary(df)

# First observations:

# For blance the minimum is -8.019, is this an error? Could it be this is a debt??
# For previous the max is 275 so this means a client was contacted 275 times, is this posible?

# Let's visualise the categorical variables to understand how they are distributed

ggplot(df) + geom_bar(aes(x=marital),fill="blue")

ggplot(df) + geom_bar(aes(x=education),fill="blue")
table(df$education) # there are 1.857 unknows, 4% of total population

ggplot(df) + geom_bar(aes(x=default),fill="blue") # a bit inbalanced
table(df$default) # there are 855 only who defaulted

ggplot(df) + geom_bar(aes(x=housing),fill="blue")

ggplot(df) + geom_bar(aes(x=loan),fill="blue")

ggplot(df) + geom_bar(aes(x=contact),fill="blue") # What is the difference between telephone and cellular?

ggplot(df) + geom_bar(aes(x=month),fill="blue") # pretty imbalanced little in dec and mar and nearly all in may

ggplot(df) + geom_bar(aes(x=poutcome),fill="blue") # not very helpful variable 
table(df$poutcome)

# Let's visualise continous vars

ggplot(df) + geom_density(aes(x=balance)) + scale_x_log10(breaks=c(110,1100,11000,110000))+annotation_logticks(sides="bt")

ggplot(df) + geom_density(aes(x=age)) + scale_x_continuous()

# Is there any correlation between age and balace?

cor(df$age,df$balance) # 0.09 not a significant correlation.

ggplot(df,aes(x=age,y=balance)) + geom_point() + geom_smooth() + ylim(0,11000) # Good prediction except for tails.

# Is there a relationship between the age and subscription to deposit

ggplot(df,aes(x=age,y=y)) + geom_point(position=position_jitter(w=0.5,h=0.5)) + geom_smooth()

# Let's interpret based on whether they got the deal or not

ggplot(df) + geom_bar(aes(x=education,fill=factor(y,levels = c(0,1),labels=c("False","True"))),position="fill") # The higher the ed the higher the prob

ggplot(df) + geom_bar(aes(x=marital,fill=factor(y,levels = c(0,1),labels=c("False","True"))),position="fill") # Higherprob for single's 

ggplot(df) + geom_bar(aes(x=month,fill=factor(y,levels = c(0,1),labels=c("False","True"))),position="fill") # Dec mar oct and sep when most contracted



############################ Variable evaluation ###############################################################

# Which variables will we be able to use for contacting our clients 

df_contactable <- df[(df$contact != "unknown"),]

# All our clients are now only contactable by telephone so we romove this var

df_contactable$contact <- NULL

# Since I am not interest in the specific value of their deposit let's factorise

df_contactable$deposit <- 0
df_contactable$deposit[df_contactable$balance < 0] <- "debtor"
df_contactable$deposit[df_contactable$balance >= 0 & df_contactable$balance <= 478] <- "low"
df_contactable$deposit[df_contactable$balance > 478 & df_contactable$balance <= 1515] <- "medium"
df_contactable$deposit[df_contactable$balance > 1515] <- "high"

table(df_contactable$deposit)

# QC CHECK OK!!

# We don't need the balance variable any more 

df_contactable$balance <- NULL

# Facotrise

df_contactable$deposit <- factor(df_contactable$deposit,levels = c("debtor","low","medium","high"))

## All variables seem to be useful 

############### Model creation ########################################

set.seed(2018)

intrain <- createDataPartition(df_contactable$y,p=0.7,list=FALSE)
training<- df_contactable[intrain,]
testing<- df_contactable[-intrain,]

# Let's confirm the slitt is correct:

dim(training); dim(testing)

# QC check OK!

prop.table(table(training$y))
prop.table(table(testing$y))

# It is evenly split

#### Baseline Model###

baseline <- train(y ~ ., data=training, method = "lm")
summary(baseline)

# We get 31% R2 

sum(df_contactable$y == 1)/nrow(df_contactable) # Vs 15% if we just did random guessing. 

### Tree Model ###

training$y <- factor(training$y,levels = c(0,1))
testing$y <- factor(testing$y,levels = c(0,1))


tree_model <- C5.0(training[-15],training$y)

tree_model # Check attributes 

summary(tree_model) # Error rate of 8.7% 

# Evaluate model performance 

tree_pred <- predict(tree_model,testing)

CrossTable(testing$y,tree_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c('actual y','predicted y'))

# Good model precision of 81%, however we are correctly classifing those who will not contract our product very well 91% 
# But we are failing to correctly predict those that will 55%

## Let's try boosting

tree_modelboost <- C5.0(training[-15],training$y,trials = 10)

tree_modelboost # Check attributes 

summary(tree_modelboost) # Error rate of 3.9% 

# Evaluate model performance 

tree_predboost <- predict(tree_modelboost,testing)

CrossTable(testing$y,tree_predboost,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c('actual y','predicted y'))

# We have managed to increase accuracy for those that wouldn't contract to 94% 
# for those who contracted we have gone down to 49%

## Creating a cost matrix ## 

matrix_dimensions <- list(c("0","1"),c("0","1"))
names(matrix_dimensions) <- c("predicted","actual")

# Let's check its properly created
matrix_dimensions

# QC CHECK OK!!!!

# Let's define the error cost, in our case the worst is classifing potential customers that would contract 
# as if they wouldn't (False Negatives)

error_cost <- matrix(c(0,1,3,0),nrow = 2,dimnames = matrix_dimensions)

error_cost

# Tree with cost function 

tree_cost <- C5.0(training[-15],training$y,costs = error_cost)

tree_cost_pred <- predict(tree_cost,testing)

CrossTable(testing$y,tree_cost_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE, dnn=c('actual y','pred y'))

# Accuracy 85%
# Precision 76%

# Good model

