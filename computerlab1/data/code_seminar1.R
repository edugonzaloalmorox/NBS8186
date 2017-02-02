setwd("")


# Load data
# ---------
      
      # read.csv function 
      nba = read.csv("nba.csv", sep = ",", header = TRUE)
      
      # import function 
      nba = import("nba.csv")


    # Explore data 
      
      head(nba)
      tail(nba)
      str(nba)

# Summary 
# -------
      
      summary(nba) # also used for variables  
      mean(nba$age)
      
# Forward category
# ----------------
      
      table(nba$forward)
  
# Histogram
# ----------
      
      # xlab = rename the axis X
      # main = title of the plot      
      
      hist(nba$points, xlab= "points", main = "Histogram of points")
      
      
      
# Scatterplot
# -----------
      
      # xlab = rename the axis X
      # ylab = rename the axis Y
      # main = title of the plot      
      
      with(nba, plot(points, exper, 
                     xlab= "points", 
                     ylab = "experience", 
                     main = "Scatterplot of points vs 
                     experience"))
      
# Regression model 
# ----------------
      
      library(stats)
      library(AER)
      
      model1 = lm(points ~ exper + age + coll +
                    forward + center, 
                  data = nba)
      
      summary(model1)
      
# Correlation matrix
# ------------------
      
      
      with(nba, cor(cbind(exper, age, coll, center, forward)))
      
      
      # look at significance 
      # --------------------
      library(Hmisc) # for doing the correlation matrix 
      library(dplyr) # for handling data 
      
      # select variables from the model 
      vars_mod = nba %>% select(exper, age, coll,
                                forward, center)
      
      cor_mat <- rcorr(as.matrix(vars_mod), type = "pearson")
      # note: subsetting using pipes 
      
      # correlation matrix 
      cor_mat <- rcorr(as.matrix(vars_mod), type = "pearson")
      emphasize.strong.cells(which(cor_mat[[3]] < 0.001,
                                   arr.ind = TRUE))
      
# Creating new variables
# ----------------------
      nba$expersq =nba$exper^2
      
      # include 'expersq'
      model2 = lm(points ~ exper+expersq+age+coll+
                    center+forward, 
                  data = nba)
      
      summary(model2)
      
      nba$logwage = with(nba, log(wage))

      #include 'logwage'
      model3 <- lm(logwage~points+exper+expersq+age+coll, data = nba)
      
# Model comparison - ANOVA 
# ------------------------
      
      
      mod_unrest <- lm(logwage~points+exper+expersq+
                         age+coll, data = nba)
      mod_rest <- lm(logwage~points+exper+expersq, data = nba)
      
      anova(mod_rest, mod_unrest)
