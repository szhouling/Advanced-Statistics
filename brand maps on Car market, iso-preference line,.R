# Team K
library(glmnet)

#Load data
data <-  read.csv("Cars_Data.csv", header=T)    # read csv file and label the data as "data"
brand_name <- data[,1]
attributes <- colnames(data)
attributes <- attributes[2:16]

y <-  data[,17]
x <-  as.matrix(data[,2:16])
cor_mat = cor(x)									# create correlation matrix

##### Principal Components Analysis #####

out1 <-  eigen(cor_mat)		# eigen decomposition of correlation matrix
va <-  out1$values			# eigenvalues
ve <-  out1$vectors			# eigenvector



#the scree plot
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")

## Q2 ##
ego <- va[va > 1]							# eigenvalues > 1
nn <- nrow(as.matrix(ego))					# number of factors to retain

#### Q2:Only 4 Factors to retain #####


out2 <- ve[,1:nn]							# eigenvectors associated with the reatined factors
out3 <- ifelse(abs(out2) < 0.3, 0, out2)		# ignore small values < 0.3

## Assign names to retained factors
rownames(out3) <- attributes

out4 <- out3

#flip the values to make its correlation positive in regression
out4[,1] <- out4[,1]*(-1)
out4[,2] <- out4[,2]*(-1)

out4
# Principal Component "Scores" are the linear combination of original variables, where the weights come from the eigenvectors.
# Denoted by Z in the slides

z <- x %*% out4			# Component Scores; coordinates of the brands in the map

## Q3 ##

## Based on the out 4, name the benefits
colnames(z) <- c('Aesthetic_Appeal_and_Prestige', 'Space','Cost Efficiency and Practicality','Innovation and Uniqueness')
z
out5 <- lm(y ~ z)		# Preference Regression to estimate how benefits drive overall preferences = f(benefits)


summary(out5)



##### Iso Preference Line and Ideal Vector ####			

## Consider factors z1 and z3 with positive slopes

## Z2 and Z4 are not very significant ,and Z2 is not a very meaningful benefits(positive unreliable, negative easy-service and sporty)

## Let z3 be the y-axis and z1 as the x-axis	
## Plot (z3, z1) to get brands in factor space of benefit 1 and benefit 3 

# coordinates of brands in (z1, z3) space
Aesthetic_Appeal_and_Prestige <- z[,1]
Space <- z[,3]
z.out <- cbind(Aesthetic_Appeal_and_Prestige, Space)
rownames(z.out) = brand_name
z.out
# Plot, add labels, and save the brand map 

plot(Aesthetic_Appeal_and_Prestige, Space, main = "Brands in Appeal_and_Prestige and Space", xlab = "Benefit: Appeal_and_Prestige", ylab = "Benefit: Space", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z3 space
text(z.out, labels = row.names(z.out), font = 2, cex = 0.5, pos = 1)						# labeling brands

## Q4 ##
## The coefficients from the regression model tell how much each benefit contributes to the overall preference. The iso-preference line's slope is calculated by taking the negative ratio of the 
## coefficient for "Aesthetic Appeal and Prestige" to the coefficient for "Space" (-0.68944 to 0.46138), which results in the iso-preference slope of 1.494305. This means that to maintain 
## the same level of preference and also improving, it would need to increase its "Space" benefit by about 1.494 units for every unit decrease in the negative aspect of "Aesthetic Appeal and Prestige."

## The difference between this iso-preference line and a regression line is that the iso-preference line represents equal preference, whereas a regression line 
##  represent the predictive relationship between "Aesthetic Appeal and Prestige" and "Space" on consumer preference. 

## Q5 what is an ideal vector and why it indicates the direction of increasing preferences ##
## The concept of an ideal vector is central in multi-attribute utility theory, where each dimension corresponds to a criterion of interest. The ideal vector signifies the most favorable outcome for 
## each criterion and is represented by a point in multi-dimensional space.
## As Aesthetic_Appeal_and_Prestige increases, Space will increase by a certain amount determined by the coefficient of Aesthetic_Appeal_and_Prestige.
##  If the coefficient of Aesthetic_Appeal_and_Prestige is positive, it indicates that as aesthetic appeal and prestige increase, space also tends to increase. 
##  The ideal slope represents the direction that is considered ideal or preferred for increasing preferences. If the estimated slope is closer to this ideal slope, it suggests that the observed 
# relationship between aesthetic appeal/prestige and space matches more closely with the ideal direction of increasing preferences.

## Q6 ##

# Slopes of iso-preference and ideal vector	
b1 <- as.vector(coef(out5)[2])
b3 <- as.vector(coef(out5)[4])
slope.iso.preference = - b1/b3						# Why? See slides
slope.iso.preference
slope.ideal.vector = b3/b1 							# Why? See slides
slope.ideal.vector


# Angles of iso-preference and ideal vector	
angle.iso.preference <- atan(slope.iso.preference)*180/pi
angle.iso.preference
angle.ideal.vector <- atan(slope.ideal.vector)*180/pi
angle.ideal.vector



## Q7 ##
nn <- nrow(z.out)
bb <- 1000
angle.ideal.vector_star <- numeric(bb)  # Initialize the vector to store angles from each iteration
class(z.out)
z.out2 <- z.out
z.out2 <- cbind(y,z.out2)
for(ii in 1:bb) {
  # Create (y*, z*) by resampling rows in the original data matrix
  data.star <- z.out2[sample(nn, nn, replace = TRUE),]
  ystar <- data.star[,1]  # Make sure to use the bootstrapped data
  zstar <- data.star[,-1]  # Make sure to use the bootstrapped data
  
  out.star <- lm(ystar ~ zstar)  # lm with new y* and new z* to get new bhat*
  
  b1_star <- coef(out.star)[2]  # Assuming the model has a single independent variable zstar
  b3_star <- coef(out.star)[3]  # This line seems to be an error since the model defined doesn't appear to include a second independent variable
  slope.ideal.vector_ci = b3_star/b1_star
  # Compute angle - Adjust the computation if your model indeed includes more than one independent variable
  angle.ideal.vector_star[ii] <- atan(slope.ideal.vector_ci)*180/pi
}

# Compute the 95% CI for Ideal Vector
ideal.vector.CI.data.boot <- quantile(angle.ideal.vector_star, probs = c(0.025, 0.975), na.rm = TRUE)
ideal.vector.CI.data.boot


## Q8 ##
# Leverage Prestige: Infinity scores highest on "Prestige," which is likely correlated with Benefit Z1 based on its 
# position on the map, which means customers see Infinity as a prestigious brand. So we recommend the manager continuing to build on the prestige factor since it's a clear strength for Infinity. 

# Improve Comfort: The Infinity’s "Uncomfortable" score is relatively lower, which could be an area for improvement if this is negatively impacting customer preferences.
# Address the lower scores in "Uncomfortable" by enhancing the comfort features of the cars, such as seating, ride smoothness, and interior ergonomics.

# Highlight and Boost Key Attributes: Based on the high scores and the brand map, Infinity should continue to emphasize its strengths in sportiness, service, and success features. For manager’s product 
# design, this means focusing on performance characteristics, easy maintenance, and cutting-edge technology.

# Differentiate from Close Competitors: Infinity is positioned well against most brands, but BMW is a close competitor in the Z1 dimension. Infinity should find ways to distinguish itself further from BMW.