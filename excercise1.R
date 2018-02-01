# Spørsmål: 
# Finnes det andre måter å finne ut om en matrise er positive definite
# enn å beregne xTAx? JA! Finn eigenvalues!
# Kan vi hente infoen fra eigen(A) og bruke den direkte til å 
# lage P? Ja! Dollartegn
# h) Jaaaa fordi den er symmetrisk og positive definite. Mer enn bra nok

A = matrix(c(9,-2,-2,6), nrow = 2, byrow = TRUE, dimnames = list(c("row1", "row2"), c("c1", "c2")))
At = t(A)
eA = eigen(A)
eA$vectors[,1]

P = eA$vectors

D = diag(c(10,5))
t(P)%*%D%*%P

Ainverse = solve(A)
eigen(Ainverse)

corA = diag(1/sqrt(diag(A)))%*%A%*%diag(1/sqrt(diag(A)))
cov2cor(A)

# j)
muX = c(3,1)
CovX = A
# first
one = matrix(c(1,1,1,2), nrow = 2)
E1=one%*%muX
Cov1 = one%*%CovX%*%t(one)

# second
two = matrix(c(1,2), nrow = 1)
E2 = two%*%muX
Cov2 = two%*%CovX%*%t(two)

#third
three = diag(c(1,3))
E3 = three%*%muX
Cov3 = three%*%CovX%*%t(three)

#fourth
muNewX = c(muX, muX)
E4 = diag(c(1,1,3,3))%*%muNewX
B = cbind(A, 3*A)
D = rbind(B, 3*B)

# Problem 2

matrix2 = matrix(c(2/3,-1/3,-1/3,-1/3, 2/3, -1/3, -1/3, -1/3, 2/3), nrow = 3)
mean = matrix2%*%c(1,1,1)
mcovariance = diag(3)%*%matrix2%*%diag(3)

