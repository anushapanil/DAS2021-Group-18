
library(tidyverse)
library(GGally)
library(pscl)

# Reading in the data set
animals <- read_csv("dataset18.csv")

head(animals)
glimpse(animals)

# Some initial plots (just for me to check)
ggplot(animals, aes(x = outcome_type, y = time_at_shelter)) +
  geom_boxplot()
#how about modity the y axis to make the plot more readable

ggplot(animals, aes(x = time_at_shelter)) +
  geom_histogram()


ggpairs(animals)

# First Poisson model - full model
pmodel <- glm(data = animals, time_at_shelter~., family = poisson)

summary(pmodel)

# Poisson model with only significant variables
pmodel2 <- glm(data = animals, time_at_shelter~ intake_type + outcome_type + chip_status, family = poisson)

summary(pmodel2)




nrow(animals)

animals %>%
  filter(time_at_shelter == 0) %>%
  count()


# Many more zeros than any other value
plot(table(animals$time_at_shelter))


# 
mu <- predict(pmodel2, type="response")
exp <- sum(dpois(x=0, lambda = mu))

# Model predicts 34 zeros but we have 276!!!!
round(exp)


###

# Trying a hurdle model to account for lots of zeros
hurdlemodel <- hurdle(data = animals, time_at_shelter ~ intake_type + outcome_type + chip_status)

summary(hurdlemodel)

# Hurdle predicts 276 zeros by design
sum(predict(hurdlemodel, type="prob")[,1])



# First 5 predictions from the hurdle model
predict(hurdlemodel,type="response")[1:5]

# Install countreg package
install.packages("countreg", repos="http://R-Forge.R-project.org")


library(countreg)

# Poisson model WAY underfits zeros and overfits about 2:12
rootogram(pmodel2,max=80)

# This looks better but is still not quite right
# The hurdle model is underfitting 1:4 and overfitting a chunk after
rootogram(hurdlemodel, max= 80)

# Trying a hurdle model with negative binomial
nb.hurdlemodel <- hurdle(data = animals, time_at_shelter ~ intake_type + outcome_type + chip_status, dist = "negbin")

summary(nb.hurdlemodel)

# Looks better
rootogram(nb.hurdlemodel, max=80)

# negative binomial model has the best AIC by far!
AIC(pmodel2)
AIC(hurdlemodel)
AIC(nb.hurdlemodel)

# Zero inflated model with negative binomial is similar to hurdle
zero.model <- zeroinfl(data=animals, time_at_shelter ~ intake_type + outcome_type + chip_status, dist="negbin")

rootogram(zero.model,max=80)
AIC(zero.model)


head(animals)

newdat <- data.frame(intake_type = "STRAY", outcome_type = "RETURNED TO OWNER", chip_status = "SCAN NO CHIP")
newdat

predict(nb.hurdlemodel, newdat,type="response")


### Using test/train sets and the neg bin hurdle model 70/30 split

set.seed(7)
# Setting up the split
n <- nrow(animals)
split <- 0.7

n

#
intrain <- sample(c(1:n),round(n*split))
intest <- sample(c(1:n)[-intrain], round(n*(1-split)))

# Test and train dataframes
train.animals <- animals[intrain,]
test.animals <- animals[intest,]

head(train.animals)


# Training the hurdle model
nb.hurdle.train <- hurdle(data = train.animals, time_at_shelter~intake_type + outcome_type + chip_status, dist = "negbin")

# looks like the fit is still pretty good
rootogram(nb.hurdle.train)

# Using this model to predict the test data
pred.animals <- predict(nb.hurdle.train, test.animals[,4:6], type="response")

MSE <- mean((test.animals$time_at_shelter - pred.animals)^2)
RMSE <- sqrt(MSE)

# Mean squared and Root Mean squared error
MSE
RMSE


cbind(test.animals$time_at_shelter, pred.animals)

summary(nb.hurdle.train)

# Training a regular poisson model for comparison
pmodel.train <- glm(data = train.animals,  time_at_shelter~intake_type + outcome_type + chip_status, family = poisson)

pred.pmodel <- predict(pmodel.train, test.animals[,4:6], type="response")

rootogram(pmodel.train)

pMSE <- mean((test.animals$time_at_shelter-pred.pmodel)^2)
pRMSE <- sqrt(pMSE)

# The errors for the poisson model are actually way better.
cbind(MSE,RMSE,pMSE,pRMSE)

cbind(test.animals$time_at_shelter, pred.animals, pred.pmodel)


### Trying other hurdle models

nb.hurd.full <- hurdle(data = train.animals, time_at_shelter~intake_type+outcome_type, dist = "negbin")



AIC(nb.hurd.full)

# The AIC for the neg bin model is way better but the 
AIC(nb.hurdle.train)
AIC(pmodel.train)








