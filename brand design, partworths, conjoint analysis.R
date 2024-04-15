library(readxl)
library(ggplot2)


product_data <-read.csv("Design Matrix.csv")

#Get the data given
designs <- data.frame(
  Profiles <- c('My design', 'Competing Brand 1(Sony)', 'Competing Brand2(Sharp)'),
  Intercept <- c(1,1,1),
  Screen_75_inch <- c(0,1,0),
  Screen_85_inch <- c(1,0,1),
  Resolution_4k <- c(0,1,1), 
  Sony_1 <- c(0,1,0),
  Price <- c(1500,2500,2000)
)
names(designs) <- c('Profiles','Intercept',"Screen75_ich", "Screen85_inch", "Resolution4K", "Brand_Sony", "Price")
costs <- c(1000,500,1000,250,250)
#Calculate the net cost
Net_Cost = designs[1,2]*costs[1]+designs[1,3]*costs[2]+designs[1,4]*costs[3]+designs[1,5]*costs[4]+designs[1,6]*costs[5]
#market size
market_size = 100

#Create function here:
perform_conjoint_analysis <- function(preference_vector) {
  design_matrix <- product_data[,c(1:5)]
  model <- lm(preference_vector ~ ., data = design_matrix)
  summary(model)
  #Get coefficients
  coeff <- model$coefficients

  #Calculate the importance and the range
  screensize_range <- coeff[3]-coeff[2]
  resolution_range <- coeff[4]
  brand_range <- coeff[5]
  price_range <- 0-coeff[6]

  screensize_importance <- screensize_range/sum(screensize_range,resolution_range,brand_range,price_range)
  resolution_importance <- resolution_range/sum(screensize_range,resolution_range,brand_range,price_range)
  brand_importance <- brand_range/sum(screensize_range,resolution_range,brand_range,price_range)
  price_importance <- price_range/sum(screensize_range,resolution_range,brand_range,price_range)

  attribution_importance <- data.frame(screensize_importance,resolution_importance,brand_importance,price_importance,row.names = NULL)
  
  
  #Calculate the WTP
  price_saving <- designs$Price[2]-designs$Price[3]
  WTP_unit <- price_saving/price_range
  WTP_75screen <- WTP_unit*coeff[2]
  WTP_85screen <- WTP_unit*coeff[3]
  WTP_4Kresolution <- WTP_unit*coeff[4]
  WTP_SonyBrand <- WTP_unit*coeff[5]
  
  Willingness_To_Pay <- data.frame(WTP_unit,WTP_75screen,WTP_85screen,WTP_4Kresolution,WTP_SonyBrand,row.names = NULL)
  
  #Calculate utility and attractiveness and market share
  utility_mydesign <- coeff[1]*designs[1,2]+coeff[2]*designs[1,3]+coeff[3]*designs[1,4]+coeff[4]*designs[1,5]+coeff[5]*designs[1,6]+coeff[6]*(designs[1,7]-designs[3,7])/(designs[2,7]-designs[3,7])
  attractiveness_mydesign <- exp(utility_mydesign)
  utility_sony <- coeff[1]*designs[2,2]+coeff[2]*designs[2,3]+coeff[3]*designs[2,4]+coeff[4]*designs[2,5]+coeff[5]*designs[2,6]+coeff[6]*(designs[2,7]-designs[3,7])/(designs[2,7]-designs[3,7])
  attractiveness_sony <- exp(utility_sony)
  utility_sharp <- coeff[1]*designs[3,2]+coeff[2]*designs[3,3]+coeff[3]*designs[3,4]+coeff[4]*designs[3,5]+coeff[5]*designs[3,6]+coeff[6]*(designs[3,7]-designs[3,7])/(designs[2,7]-designs[3,7])
  attractiveness_sharp <- exp(utility_sharp)
  
  total_attractiveness <- sum(attractiveness_mydesign,attractiveness_sony,attractiveness_sharp)
  
  market_mydesign <- (attractiveness_mydesign/total_attractiveness)
  
  #Conduct what if analysis as excel
  what_if_price = seq(from = 1500, to = 3200, by = 100)
  what_if_analysis <- data.frame(
    Price = what_if_price,
    Market_Share = numeric(length(what_if_price)),
    Sales = numeric(length(what_if_price)),
    Margin = numeric(length(what_if_price)),
    Profit = numeric(length(what_if_price))
  )
  
  # Calculate the market share, sales, margin, and profit for each price
  for (i in seq_along(what_if_price)) {
    current_price <- what_if_price[i]
    
    # Recalculate utility based on the new price
    utility_mydesign_current <- coeff[1]*designs[1,2]+coeff[2]*designs[1,3]+coeff[3]*designs[1,4]+coeff[4]*designs[1,5]+coeff[5]*designs[1,6]+coeff[6]*(current_price-designs[3,7])/(designs[2,7]-designs[3,7])
    
    attractiveness_mydesign_current <- exp(utility_mydesign_current)
    total_attractiveness <- attractiveness_mydesign_current + attractiveness_sony + attractiveness_sharp
    
    # Calculate market share for the new price
    market_share_current <-(attractiveness_mydesign_current / total_attractiveness)
    
    # Calculate sales, margin, and profit
    sales <- market_share_current * market_size
    margin <- current_price - Net_Cost
    profit <- margin * sales
    
    # Store the calculations in the dataframe
    what_if_analysis$Market_Share[i] <- market_share_current
    what_if_analysis$Sales[i] <- sales
    what_if_analysis$Margin[i] <- margin
    what_if_analysis$Profit[i] <- profit
  }
  max_profit <- max(what_if_analysis$Profit)
  optimal_price <- what_if_analysis$Price[which.max(what_if_analysis$Profit)]
  
  share_with_optimal_profit <- what_if_analysis$Market_Share[which.max(what_if_analysis$Profit)]
  
  profit_plot <- ggplot(what_if_analysis, aes(x = Price, y = Profit)) +
    geom_line() +
    geom_point() +
    labs(title = "Profit vs Price",x = "Price", y = "Profit")+
    geom_vline(xintercept = optimal_price, color = "red", linetype = "dashed") +
    theme_minimal()
  
  market_share_plot <- ggplot(what_if_analysis, aes(x = Price, y = Market_Share)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(title = "Market Share vs Price",x = "Price", y = "Market Share")
    theme_minimal()
    
    return(list(
      Partworths = coeff,
      Attribute_Importance = attribution_importance,
      Willingness_To_Pay = Willingness_To_Pay,
      Optimal_Price = optimal_price,
      Maximum_Profit = max_profit,
      Market_Share_Optimal_Price = share_with_optimal_profit,
      Market_Share_Plot = market_share_plot,
      Profit_Plot = profit_plot
    ))
}
test <- perform_conjoint_analysis(preference_vector = c(15, 16, 7, 21, 24, 17, 11, 12, 5, 22, 23, 14, 6, 10, 4, 9, 18, 3, 13, 8, 2, 19, 20, 1))
test$Partworths
test$Market_Share_Plot
test$Profit_Plot
