kilo <- runif(20,min=500,max = 600)
yag <- runif(20,min=45,max = 60)

df <- data.frame(kilo , yag)
df

# tasarim matrisi
X <- model.matrix(~yag, data = df)
X
# y matrisi
y <- matrix(df$kilo)
y

## Uygulama (Y = AX + E)  A degerini bulma  indercept ve slope
A <- solve(t(X)%*%X) %*% t(X) %*% y 
A

#plot
plot(yag,kilo, lwd=10)
abline(A)

## Lm fonkisyonu ile 
lm<-lm(kilo ~.,df)
summary(lm)
as.vector(coef(lm))

## coef karsilastirma
AvsLM <- data.frame(LM=coef(lm),matris = A)
AvsLM

Sonuclar <- data.frame(original = kilo, predicted = fitted(lm))
Sonuclar

