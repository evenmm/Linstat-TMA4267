library(leaps)
ds <-  + read.csv("https://web.stanford.edu/~hastie/CASI_files/DATA/diabetes.csv", sep = ",")
pairs(ds, pch = ".")
full <- lm(prog ~ ., data = ds)
allsubs <- regsubsets(prog ~ . , data = ds , nvmax = 10)
allsummary <- summary(allsubs)
allsummary$outmat
plot(allsummary$bic,
       + xlab = "Number of Variables",
       + ylab = "BIC",
       + type = "l")
allsummary$bic
which.min(allsummary$bic)
allsummary$adjr2
which.max(allsummary$adjr2)

# Coefficients of fullmodel
full$coefficients
# Coefficients of model with 5 parameters
myModel=lm(prog ~  sex+bmi+map+hdl+ltg, data = ds)
summary(myModel)
