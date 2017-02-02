setwd("/Users/Personas/My Cloud/PhD _october_2016/teaching/NBS8186/computerlab2/data")



clothing = read.csv("clothing.csv", sep = ",", header = TRUE)

# Histogram 
library(ggplot2)

m <- ggplot(clothing, aes(x = tsales))
m + geom_histogram() + ggtitle("Sales in Dutch guilders")

# Scatterplot
p <- ggplot(clothing, aes(tsales, ssize))
p + geom_point() + ggtitle("Tsales vs ssize")

# base way 
hist(clothing$tsales)
with(clothing, plot(ssize,tsales))


# Median & mean 

mean(clothing$tsales)
median(clothing$tsales)   


# Regression 

mod1 = lm(sales ~ ssize, clothing)
summary(mod1)


# Simple regression transformed variable

clothing$ssize2 = clothing$ssize^2

mod2 = lm(sales ~ ssize + ssize2, clothing)
summary(mod2)


# Multiple regression 

mod3 =lm(sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2, clothing)
summary(mod3)


# Contrast values of coefficient 

library(broom)
mod3.tidy = tidy(mod3) # use tidy for creating a data frame out of your regression 

# t statistic and cumulative function
      
      # beta 1000
      t = (mod3.tidy[2, 2] - 1000)/mod3.tidy[2,3]
      t
      2*pt(t, df = 391)
      

      # beta nfull
      t.1 = (mod3.tidy[3,2] - 2*(mod3.tidy[4,2]))/mod3.tidy[3,2]
      pt(t.1, df = 391)
      
# Chow test 

      # Step 1: Create the regression 
      mod3.1 =lm(sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2, subset(clothing,
                                                                            start <= 40))
      summary(mod3.1)
      
      mod3.2 <- lm(sales~nown+nfull+npart+naux+inv1+inv2+ssize+ssize2, subset(clothing,
                                                            
                                                                                                start>40))
      summary(mod3.2)
      
      # Step 2: Create the residuals
      
      SSR = NULL
      SSR$r = mod3$residuals^2
      SSR$ur1 = mod3.1$residuals^2
      SSR$ur2 = mod3.2$residuals^2
      
      K = mod3$rank
      
      # Step 3: Compute the Chow statistic -  according to efintion
      numerator = (sum(SSR$r) - (sum(SSR$ur1) + sum(SSR$ur2)) ) / K
      denominator = (sum(SSR$ur1) + sum(SSR$ur2))/(nrow(clothing) - 2*K)
      
      chow = numerator / denominator
      chow
      
      
      # Step 4: Compute the p-value
      
      pchow = 1-pf(chow, K, (nrow(clothing) - 2*K))
      pchow

#  Heterokedasticity
      plot(clothing$ssize, mod3$residuals^2 ) # pattern
      
      m4 <- lm( mod3$residuals^2 ~ ssize, clothing)   
      summary(m4)
      