### Linear Regression -------------------------------------------------
#> toy example --------------------------------------------------------
# generate data
n <- 100
set.seed(2018)  # set the random number generator seed
toy <- data.frame(x1 = rnorm(n=n, mean=0, sd=1),
                  x2 = rnorm(n=n, mean=0, sd=1),
                  x3 = rnorm(n=n, mean=0, sd=1)
)
toy$y <- 5 + toy$x1 + 2*toy$x2 + toy$x2*toy$x3 + rnorm(n=n, mean=0, sd=1)
# data exploration:
summary(toy)
cor(toy)
hist(toy$y) # Frequency of y variable
pairs(toy)

# fit a model
toy.lm1 <- lm(formula=y ~ x1 + x2 + x3, data=toy)
# model outputs
toy.lm1
summary(toy.lm1)

#> practice 1 --------------------------------------------------------
# to use Cars93 data set
library(MASS) 
# Data exploration
summary(Cars93)
pairs(Cars93)
cor(Cars93$Horsepower, Cars93$Weight)
hist(Cars93$Price)
# fit the model
car.lm1 <- lm(formula=Price ~ Horsepower + Weight, data=Cars93)
# variable statistical significance
summary(car.lm1) 
#> write R formulas --------------------------------------------------------
# use all variables
lm(y ~ ., data=toy)
# add variable interactions (here, it's only 2-way interactions)
lm(y ~ . + x1:x2 + x1:x3 + x2:x3, data=toy)
# or equivalently (to above)
lm(y ~ (x1 + x2 + x3)^2, data=toy)
lm(y ~ . + .^2, data=toy)
# add all possible interactions (same as before but with x1:x2:x3)
lm(y ~ x1*x2*x3, data=toy)
# the above is equivalent with
lm(y ~ (x1 + x2 + x3)^3, data=toy)
# remove intercept
lm(y ~ x1*x2*x3 - 1, data=toy)
# remove a variable
lm(y ~ x1*x2*x3 - x3, data=toy)
# remove all variables
lm(y ~ x1*x2*x3 - ., data=toy)
# formulas are evaluated from left to right. 
# the below commands produce different models:
lm(y ~ . + x1*x2*x3 - (x1+x2+x3)^2, data=toy)
lm(y ~ x1*x2*x3 - (x1+x2+x3)^2 + ., data=toy)
# add arithmetic operations [see the difference without I()]:
lm(log(abs(y)) ~ x1 + x2 + I(x2^2) + I(x2*x3), data=toy)
lm(log(abs(y)) ~ x1 + x2 + x2^2 + x2*x3, data=toy)
#> practice 2 --------------------------------------------------------
# fit the model with interaction and log-transform
car.lm2 <- lm(formula=log(Price) ~ Horsepower*Weight, data=Cars93)
# variable statistical significance
summary(car.lm2) 
#> use categorical variables --------------------------------------------------
# add a categorical variable to the toy data frame
set.seed(7)
toy$x4 <- sample(x=c('a','b','c'), size=n, replace=TRUE)
#toy$x4 <- as.factor(toy$x4)
toy$z <- unlist(apply(X=toy, MARGIN=1, FUN=function(x) 
  if (x[5]=='a') return(as.numeric(x[4]))
  else if (x[5]=='b') as.numeric(x[4]) + 5
  else if (x[5]=='c') as.numeric(x[4]) + 10
  ))
# fit a model for z against x's
toy.lm2 <- lm(z ~ . - y, data=toy)
toy.lm2
#> stepwise regression --------------------------------------------------------
toy.lm3 <- step(object=toy.lm2, scope=~ .^2)
summary(toy.lm3)
#> diagnostics tools --------------------------------------------------------
# basic plot
plot(toy.lm3) # See different representations of the residual
# see more tools:
methods(class='lm') # see all functions for lm
class(toy.lm1) # lm class
toy.lm3.infl <- influence.measures(toy.lm3)
summary(toy.lm3.infl)
#> predictions ------------------------------------------------------------
# predict is generic function
predict(object=toy.lm3, newdata=toy)
predict(object=toy.lm3, newdata=toy, interval = "prediction") # interval argument
predict(object=toy.lm3, newdata=toy, interval = "confidence")
#> practice 3 -------------------------------------------------------------
# fit the initial model
car.lm3 <- lm(log(Price) ~ Horsepower + Weight + AirBags, data=Cars93)
summary(car.lm3)
# stepwise regression
car.lm4 <- step(car.lm3, ~ + .^2)
summary(car.lm4)
# diagnostc plots
plot(car.lm4)
# compute residuals
log(Cars93$Price) - predict(object=car.lm4, newdata=Cars93)
residuals(car.lm4)

### Binary Logistic Regression ----------------------------------------------
#> Iris dataset ----------------------------------------------
summary(iris)
pairs(iris)
cor(iris[,-5])
#> model fitting ----------------------------------------------
iris.glm1 <- glm(formula=Species != "virginica" ~ ., 
                 family=binomial, data=iris)
# Getting error for above glm: glm.fit: fitted probabilities numerically 0 or 1 occurred 
?family # see supported distribution and link functions
#> model outputs & diagnostic toolsplot(iris.glm1)
iris.glm1
summary(iris.glm1)
plot(iris.glm1)
methods(class='glm') # All functions that utilize class glm
#> predictions ----------------------------------------------
iris.glm1_logodd <- predict(iris.glm1, iris, type='link')
iris.glm1_prob <- predict(iris.glm1, iris, type='response')
#> practice 4 ----------------------------------------------
iris.glm2 <- glm(formula=Species != "versicolor" ~ ., 
                 family=binomial, data=iris)
summary(iris.glm2)
iris.glm2_prob <- predict(iris.glm2, iris, type='response')

### Multilevel Regression --------------------------------------
#> install the 'lme4' package and load it into the enviroment  ----
#install.packages('lme4')
library(lme4)
#> data exploration --------------------------------------
str(sleepstudy)
summary(sleepstudy)
library(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, pch= 20, layout=c(9,2),
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)
       })
#> fit the model  --------------------------------------
sleep.lmer1 <- lmer(formula=Reaction ~ Days + (Days | Subject), data=sleepstudy)
#> model outputs and predictions  --------------------------------------
summary(sleep.lmer1)
fixef(sleep.lmer1)
ranef(sleep.lmer1)
coef(sleep.lmer1)
predict(sleep.lmer1, sleepstudy)
#> basic diagnostic plots  --------------------------------------
plot(sleep.lmer1, type = c("p", "smooth"))
plot(sleep.lmer1, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
qqmath(sleep.lmer1, id = 0.05)
#> practice 5 --------------------------------------
# varying-intercept with fixed-slope model
sleep.lmer2 <- lmer(formula=Reaction ~ Days + (1 | Subject), data=sleepstudy)
summary(sleep.lmer2)
fixef(sleep.lmer2)
ranef(sleep.lmer2)
coef(sleep.lmer2)
sleepstudy$Reaction - predict(sleep.lmer2, sleepstudy)
plot(sleep.lmer2, type = c("p", "smooth"))
plot(sleep.lmer2, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
qqmath(sleep.lmer2, id = 0.05)
# varying-intercept with fixed-mean model 
sleep.lmer3 <- lmer(formula=Reaction ~ 1 + (1 | Subject), data=sleepstudy)
summary(sleep.lmer3)
fixef(sleep.lmer3)
ranef(sleep.lmer3)
coef(sleep.lmer3)
sleepstudy$Reaction - predict(sleep.lmer3, sleepstudy)
plot(sleep.lmer3, type = c("p", "smooth"))
plot(sleep.lmer3, sqrt(abs(resid(.))) ~ fitted(.), type = c("p", "smooth"))
qqmath(sleep.lmer3, id = 0.05)

### Regression Trees --------------------------------------
# install the 'rpart' package and load it into the enviroment
# install.packages('rpart')
library(rpart)
# see current parameter setting
rpart.control()
# set new parameters for rpart
control <- rpart.control(cp=0.01, max.depth=10)
# grow a regression tree
car.tree1 <- rpart(Price ~ . , data=Cars93[, -c(1, 2, 4, 6, 27)], 
                   method="anova", control=control)
# model outputs
car.tree1
summary(car.tree1)
plot(car.tree1, uniform=FALSE, branch=1, margin=rep(.1, 4))
text(car.tree1, use.n = TRUE)
#> practice 6--------------------------------------
control <- rpart.control(cp=0.001)
#set.seed(1)
car.tree2 <- rpart(Price ~ . , data=Cars93[, -c(1, 2, 4, 6, 27)], 
                   method="anova", control=control)
plot(car.tree2, uniform=FALSE, branch=1, margin=rep(.1, 4))
text(car.tree2, use.n = TRUE)
summary(car.tree2)
#> tree pruning and predictions --------------------------------------
# plot (relative) CV error against complexity parameter (or tree size)
plotcp(car.tree2)
# tree pruning
car.tree3 <- prune(car.tree2, cp=.017)
plot(car.tree3, uniform=FALSE, branch=1, margin=rep(.1, 4))
text(car.tree3, use.n = TRUE)
# predictions
predict(object=car.tree3, newdata=Cars93, type='vector')
newob <- Cars93[1:2,]
for (i in 1:ncol(Cars93)) newob[1, i] <- NA
newob[1,]$EngineSize <- 5
predict(object=car.tree3, newdata=newob[1,], type='vector')
#> practice 7 --------------------------------------------------------------
plotcp(car.tree3)
car.tree4 <- prune(car.tree3, cp=.023)
Cars93$Price - predict(object=car.tree4, newdata=Cars93, type='vector')
residuals(car.tree4)
#> Regression Tree Diagnostics --------------------------------------------------------------
control <- rpart.control(minscplit=20, cp=0.01)
car.tree5 <- rpart(Price ~ Horsepower + Weight , data=Cars93, control=control)
plotcp(car.tree5)
res <- residuals(car.tree5)
plot(Cars93$Price, res)
car.tree6 <- rpart(res ~ ., data=Cars93[,-c(1, 2, 4, 5, 6, 27)])
plot(car.tree6, uniform=FALSE, branch=1, margin=rep(.1, 4)); text(car.tree6, use.n = TRUE)
car.tree7 <- rpart(res ~ Wheelbase + Turn.circle, data=Cars93)
plot(car.tree7, uniform=FALSE, branch=1, margin=rep(.1, 4)); text(car.tree7, use.n = TRUE)
### Decision Trees --------------------------------------------------------------
#> tree fitting and outputs ---------------------------------------
control <- rpart.control(cp=0.001)
iris.tree1 <- rpart(Species ~ ., data=iris, method='class', control=control)
iris.tree1
summary(iris.tree1)
plot(iris.tree1, uniform=FALSE, branch=1, margin=rep(.1, 4))
text(iris.tree1, use.n = TRUEE)
plotcp(iris.tree1)
#> predictions ---------------------------------------
predict(iris.tree1, iris, type="prob")
predict(iris.tree1, iris, type="class")
residuals(iris.tree1)
residuals(iris.tree1, type="pearson")
residuals(iris.tree1, type="deviance")
#> practice ---------------------------------------
control <- rpart.control(minsplit=10, cp=0.01, xval=5)
iris.tree2 <- rpart(Species ~ ., data=iris, method='class', control=control)
plotcp(iris.tree2)
iris.tree3 <- prune(iris.tree2, cp=.1)
iris.tree3
summary(iris.tree3)
plot(iris.tree3, uniform=FALSE, branch=1, margin=rep(.1, 4))
text(iris.tree3, use.n = TRUE)
predict(iris.tree3, iris, type="class")
### Random Forests ------------------------------------------------------------------
#> install the 'randomForest' package and load it into the enviroment
install.packages("randomForest")
require(randomForest)
#> fit a random forest --------------------------------------------------------------
iris.rf1 <- randomForest(Species ~ ., data=iris, ntree=100, mtry=2, importance=TRUE)
iris.rf1
#> plots --------------------------------------------------------------------------
plot(iris.rf1)
varImpPlot(iris.rf1)
partialPlot(x=iris.rf1, pred.data=iris, x.var=Petal.Width, which.class="versicolor")
#> predictions  -----------------------------------------------------------------
predict(object=iris.rf3, newdata=iris, type='response')
predict(object=iris.rf3, newdata=iris, type='prob')
#> some other tools --------------------------------------------------
# add more trees
iris.rf2 <- grow(iris.rf1, how.many=100)
iris.rf2
# combine two random forests
iris.rf3 <- combine(iris.rf1, iris.rf2)
iris.rf3
# find optimal mtry
tuneRF(iris[,-5], iris[,5], mtryStart=1, stepFactor = 2)
#> practice 9 --------------------------------------------------------------
class(Cars93$Price)
cars <- rfImpute(Price ~ ., data=Cars93[, -c(1, 2, 4, 6, 27)], iter=5, ntree=300)
tuneRF(cars[,-1], cars[,1], mtryStart=1, stepFactor = 2)
car.rf1 <- randomForest(Price ~ ., data=cars, ntree=300, mtry=2, importance=TRUE)
car.rf1 # random forest outputs
plot(car.rf1) # plot OOB MSE against ntree
varImpPlot(car.rf1) # variable importance
partialPlot(x=car.rf1, pred.data=cars, x.var=Horsepower)
cars$Price - predict(object=car.rf1, newdata=cars, type='response')

