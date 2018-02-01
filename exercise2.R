library(ggfortify)
head(USArrests)
pca = prcomp(USArrests, scale=TRUE)
autoplot(pca)

d.factanal <- factanal(state.x77, factors = 3, scores = 'regression')
autoplot(d.factanal, data = state.x77, colour = 'Income')
autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

pca$rotation[,1]
n = dim(USArrests)[1]
Xmean = c(0,0,0,0)
for(i in 1:4) {
  Xmean[i] = mean(USArrests[,i])
}
(matrix(USArrests[1,] - Xmean, nrow = 1))%*%(matrix(USArrests[1,] - Xmean, ncol = 1))
sigmaApprox = 0
for(i in 1:n){
  diff = (USArrests[i,] - Xmean)
  sigmaApprox = sigmaApprox + diff*t(diff)
}
sigmaApprox = sigmaApprox/(n-1)
