clothing <- read.csv( "clothing.csv" )
attach( clothing )

##### a)

hist(tsales)
mean(tsales)
median(tsales)   # mean > median -> tsales is positively skewed
plot(ssize,tsales)

##### b)

hist(sales)
mean(sales)
median(sales)   # mean > median -> sales is positively skewed
plot(ssize,sales)

##### c)

m1 <- lm( sales~ssize )

##### d)

ssize2 <- ssize^2
m2 <- lm( sales~ssize+ssize2 )

##### e)

m3 <- lm( sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2 )

## vi)

m3A <- lm(sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2, subset = start<=40 )
m3B <- lm( sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2, subset = start>40 )


##### f)

plot(clothing$ssize, mod3$residuals^2 )   # pattern -> heteroskedasticity
m4 <- lm( mod3$residuals^2 ~ ssize, clothing)   # slope is significant -> heteroskedasticity
summary(m4)





