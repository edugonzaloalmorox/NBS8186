# functions R computer lab NBS8186

# comments excluded 
# ----------------


setwd("") # set the path 



nba = read.csv("nba.csv", sep = ",", header = TRUE)


nba = import("nba.csv")




head(nba)
tail(nba)
str(nba)


summary(nba) # also used for variables  
mean(nba$age)



table(nba$forward)



  

hist(nba$points, xlab= "points", main = "Histogram of points")


   

with(nba, plot(points, exper, 
               xlab= "points", 
               ylab = "experience", 
               main = "Scatterplot of points vs 
               experience"))


library(stats)
library(AER)

model1 = lm(points ~ exper + age + coll +
              forward + center, 
            data = nba)

summary(model1)



with(nba, cor(cbind(exper, age, coll, center, forward)))



# --------------------
library(Hmisc) # for doing the correlation matrix 
library(dplyr) # for handling data 


vars_mod = nba %>% select(exper, age, coll,
                          forward, center)

cor_mat <- rcorr(as.matrix(vars_mod), type = "pearson")



cor_mat <- rcorr(as.matrix(vars_mod), type = "pearson")
emphasize.strong.cells(which(cor_mat[[3]] < 0.001,
                             arr.ind = TRUE))



nba$expersq =nba$exper^2


model2 = lm(points ~ exper+expersq+age+coll+
              center+forward, 
            data = nba)

summary(model2)

nba$logwage = with(nba, log(wage))


model3 <- lm(logwage~points+exper+expersq+age+coll, data = nba)





mod_unrest <- lm(logwage~points+exper+expersq+
                   age+coll, data = nba)
mod_rest <- lm(logwage~points+exper+expersq, data = nba)

anova(mod_rest, mod_unrest)
