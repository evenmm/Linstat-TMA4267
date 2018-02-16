library(MASS)
library("ggplot2", lib.loc="~/R/win-library/3.4")
?forbes
head(forbes)
names(forbes)
forbes

n = length(forbes$bp)
Y = matrix((forbes$bp - 32)*5/9, ncol = 1)
X = cbind(rep(1,n), forbes$pres * 0.0338633882)

beta = solve(t(X)%*%X)%*%t(X)%*%Y
plot(X[,2], Y, pch = 20)
plot
?plot
?aes
?data.frame

#attempt to plot both Y and fhat in smae plot
fhat = cbind(beta[1] + beta[2]*X[,2])
df = data.frame(x = X[,2], fhat = fhat, Y = Y)
multiplot = ggplot(data=df) + geom_line(x, fhat) 
  + geom_point(x, Y)

fhat = X%*%beta
df = data.frame(x = X[,2], fhat = fhat, Y = Y)
multiplot = ggplot(data=df, aes(x=x, y= Y)) + geom_abline(intercept=beta[1], slope= beta[2])
multiplot
?geom_abline

# plot pressure
plot(X[,2], Y-fhat)
sum(Y-fhat) #additivity ok

#qq-plot
ggplot(df, aes()) +
  stat_qq(pch = 19) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(modelA$call))

