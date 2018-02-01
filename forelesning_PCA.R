# PCA with a drinking habits data set, sadly, source of the data is unknown - if you have information on the source please inform Mette.Langaas@math.ntnu.no
# p=6 variables: the (average pr inhabitant?) consumption of coffee, tea, cocoa, liqueur, wine, beer
# n=24 observations from 24 different countries

drink <- read.csv("http://www.math.ntnu.no/~bakke/TMA4267/2015V/drikke.TXT",sep=",",header=TRUE)
drink
# missing data on three countries- why?
drink <- na.omit(drink)
drink

# now for PCA
?prcomp
pca <- prcomp(drink,scale=TRUE) # scale: variables are scaled

# coefficients of PCs, called rotations or loadings
pca$rotation

# done "manually":
s <- cor(drink) # cor, not cov, since covariates are scaled
s
cov(scale(drink)) # the same
eigen(s) # same as pcs$rotations - opposite sign of some vectors

# sample variances of the PCs are the eigenvalues of s
pca$sdev
pca$sdev^2 # compare with eigenvalues above

# scores - values of the PCs
pca$x
cov(pca$x) # offdiag=0, ondiag=pca$sdev^2
pca$sdev^2

# plot the scores of two first PCs against each other
plot(pca$x[,1],pca$x[,2],type="n")
text(pca$x[,1],pca$x[,2],rownames(drink),cex=0.6)
# The same with also loading of two first PCs plotted
biplot(pca,scale=0,cex=0.6) # scale=0: arrows scaled to represent the loadings
# Great Britain has large PC1, for which tea is influental.
# Italy has large negative PC1, for which wine is influental.
# The Netherlands has large PC2, for which coffee and tea is influental.
# East European countries has large negative PC2, for which liqueur (should be liquor = spirits?)

# plot scaled original data for tea and wine, which are influental for PC1,
# then add line parallell with vector consisting of loadings for tea and wine for PC1
plot(scale(drink)[,2],scale(drink)[,5])
abline(0,pca$rotation[5,1]/pca$rotation[2,1],col="red")
# agrees visually with line onto which projections have large variance

# the same for other pairs of PCs:
biplot(pca,choices=c(1,3),scale=0,cex=0.6) # Ireland: Much tea, little liquor, Italy: Much wine, little liquor
biplot(pca,choices=c(2,3),scale=0,cex=0.6) # Poland: Much liqour, little cocoa or coffee
biplot(pca,choices=c(1,4),scale=0,cex=0.6)
biplot(pca,choices=c(1,5),scale=0,cex=0.6)

# How many PCs do we need to capture a large part of the variability in the data?
summary(pca) # 4 to get 84%
plot(pca) # screeplot - says 4?

# the effect of scaling
pca.noscale <- prcomp(drink,scale=FALSE)
summary(pca.noscale) # 1 or 2 PC enough
biplot(pca.noscale,scale=0)
# beer and wine dominate

## artificial data - data in two non-overlapping balls

# function to generate point uniformly in unit ball
ballrand<-function(){
  repeat{
    x<-runif(3,-1,1)
    if(sum(x^2)<1) break
  }
  x
}

set.seed(1)
n<-200
data<-matrix(rep(NA,6*n),ncol=3) # 2*n = 400 data points
offset<-1.2
for(i in 1:(2*n)) data[i,]<-ballrand()
data[(n+1):(2*n),]<-data[(n+1):(2*n),]+offset # offset second half
x1<-data[,1]; x2<-data[,2]; x3<-data[,3]
plot(x1,x2) # no pattern - projections of balls overlap in x1-x2 plane
plot(x1,x3) # no pattern
plot(x2,x3) # no pattern
data<-data.frame(x1,x2,x3)
pc<-prcomp(data,scale=TRUE)
plot(pc$x[,1],pc$x[,2]) # balls separate from this perspective
# with colouring of data belonging to each ball:
plot(pc$x[,1],pc$x[,2],col=c(rep("red",n),rep("blue",n)))
biplot(pc,scale=0,pch=1)

## The following shows PCA in regression - not done (yet) in TMA4267 2018 spring
# Now, turn to PCR - principal component regression
# Acid rain data set: pH in Nowegian lakes (y) vs. sulfate, nitrate, calcium, aluminium and organic content (x1, ... x5), area of lake (x6) and location (x7 = 0, Telemark, or x7 = 1, Tr?ndelag)? Data from Statens forurensningstilsyn (1986). Here 26 random lakes from Telemark and Tr?ndelag out of 1005 lakes 

acidrain <- read.table("http://www.math.ntnu.no/~mettela/TMA4267/Data/acidrain.txt",header=TRUE)

pcaacid <- prcomp(acidrain[,-1],scale=TRUE) # remove y column
biplot(pcaacid,scale=0)
plot(pcaacid) # 4 PCs?
summary(pcaacid) # 3 PCs?
pcaacid$rotation

z <- pcaacid$x[,1:3]
z
fit <- lm(y~z,data=acidrain) # only y used from acidrain
summary(fit)

# for interpretation, back to original data
betas <- pcaacid$rotation[,1:3]%*%matrix(fit$coeff[2:4],ncol=1) # first matrix transpose of Phi
betas

# compared to the findings for the full model, remember that the xs were scaled
#x <- apply(acidrain[,-1],2,scale)
x <- scale(acidrain[,-1])
#y <- ds[,1]
fullfit <- lm(y~x,data=acidrain)
summary(fullfit)
plot(fullfit$coeff[-1],betas)
abline(0,1) # line, intercept 0, slope 1
cbind(fullfit$coeff[-1],betas)