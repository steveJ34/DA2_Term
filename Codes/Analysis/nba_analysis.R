#######################
## Analysis of       ##
##  Covid-19 cases   ##
##    and            ##
##  Death due to     ## 
##  Covid-19 on a    ## 
##  given day        ##
##                   ##
##      NO. 3        ##
##                   ##
## Regression        ##
## Analysis          ## 
##                   ##
#######################

# Clear memory
rm(list=ls())
# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(dplyr)

# Call the data from github
my_data <- "https://raw.githubusercontent.com/steveJ34/DA2_Term/main/Data/Clean/nba_clean.csv"
df <- read_csv( my_data )

# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Summary of the data 
summary( df )

######
# Plotting basic scatterplots to make a decision about the variable transformation 

# Basic regressions: 


######
# Checking distributions of salary and PER per career by applying different variable transformations


# 1) Average salary per career (in $MM)  - player efficiency rating: level-level model without scaling
ggplot( df , aes(x = PER, y = avg_salary)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Average PER per career", y = "Average salary per career (in $MM)")


# 1a) level - level with a quadratic term 
ggplot( data = df, aes( x = PER, y = avg_salary ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

 
# 2) Average salary per career (in $MM) - ln(PER per career): level-log transformation  
ggplot( df , aes(x = PER, y = avg_salary)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Average PER per career (ln scale)",y = "Average salary per career (in $MM)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 3) ln(Average salary per career (in $MM)) - Average PER per career: log - level transformation 
ggplot( df , aes(x = PER, y = avg_salary))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Average PER per career",y = "Average salary per career (in $MM, ln scale )") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 4) ln(Average salary per career (in $MM)) - ln(average points scored per career): log log transformation 
ggplot( df , aes(x = PER, y = avg_salary))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Average PER per career (ln scale)", y = "Average salary per career (in $MM, ln scale )") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)  )

# Checking distributions of salary and total games played per career by applying different variable transformations


# 1) Average salary per career (in $MM)  - total games: level-level model without scaling
ggplot( df , aes(x = tot_games, y = avg_salary)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "total games", y = "Average salary per career (in $MM)")

# 1a) level - level with a quadratic term 
ggplot( data = df, aes( x = tot_games, y = avg_salary ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# 2) Average salary per career (in $MM) - ln(total games): level-log transformation  
ggplot( df , aes(x = tot_games, y = avg_salary)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "total games (ln scale)",y = "Average salary per career (in $MM)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 3) ln(Average salary per career (in $MM)) - total games: log - level transformation 
ggplot( df , aes(x = tot_games, y = avg_salary))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "total games",y = "Average salary per career (in $MM, ln scale )") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 4) ln(Average salary per career (in $MM)) - ln(total games): log log transformation 
ggplot( df , aes(x = tot_games, y = avg_salary))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "total games (ln scale)", y = "Average salary per career (in $MM, ln scale )") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)  )

# Checking distributions of salary and draft round  by applying different variable transformations


# 1) Average salary per career (in $MM)  - draft round: level-level model without scaling
ggplot( df , aes(x = draft_round, y = avg_salary)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Draft Round", y = "Average salary per career (in $MM)")


# Conclusions: 

# 1) The level-level model seems to work best for the association between the average salary and PER. A quadratic term might be useful to capture the non-linearities, however it will complicate the interpretation. The benefit of it should be assessed relative to how complex the interpretation gets.   
# 2) The simple model also works best for total games played, but there's an infection point, at about 500. A spline should be introduced to account for that. 
# 3) The draft round variable indicates a negative association between the draft round and average salary. The variable can be used as a dummy. 

# Reasons to use transformations
# 1) Statistical: the quadratic term and spline should help to offset the observed non-linear patterns. In case of PER (quadratic term addition) and total games played (linear spline)
# 2) Substantive: while introducing the quadratic term and adding a linear spline, the interpretation should still be suitable for players as observations. 

# Transforming the variables based on the conclusions

# Add powers of the PER variable:
df <- df %>% mutate( PER_sq = PER^2)

# Introduce cutoff for Linear Spline: 
cutoff <- 500

# Use simple regression with the lspline function to check the new functional form 

ggplot( data = df, aes( x = tot_games, y = avg_salary ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = 'red' )

# Converting draft round to a dummy variable
df$draft_round <- ifelse(df$draft_round == '2', 1, 0)

##### 

# Testing how the explanatory variables are related to each other 

# Creating a new data frame that contains of only the explanatory variables 
df_X <- df[,4:7]

# Creating a correlation matrix
library(GGally)
ggpairs(df_X)


#install.packages('corpcor')
library(corpcor)
cor2pcor(cov(df_X))

# Conclusions: 

# Overall, it looks like there in no multicollinearity in the data. However we can check the following. 
# 1) The matrix suggests that there might be a correlation between the PER and the number of games played: level-level model without scaling
ggplot( df , aes(x = PER, y = tot_games)) +
  geom_point() +
  geom_smooth(method="loess")
# There might be a a positive association between the number of games played and PER, however after PER = 14, it tends to be more and more flat. Collinearity is not expected between these varaiables. 

# Regression test: estimating a regression and checking the 
reg_coll <- lm_robust(tot_games ~ PER, data = df)
summary(reg_coll)


# 2) We can safely assume that there is no collinearity between the draft round and the number of games played. Draft round measures the potential of a player coming to the league, while the number of games played should measure the longevity of a player. It can be affected by many other factors such as injuries 

ggplot( df , aes(x = draft_round, y = PER)) +
  geom_point() +
  geom_smooth(method="loess")

# 3) No collinearity should be assumed for draft round and PER. Draft round is reflecting the accomplishments of a player before coming to the NBA, while PER is accumulated throughout a player's career. 


######
# Exploring potential models:
# 
#     reg1: avg_salary = alpha + beta * PER
#     reg2: avg_salary = beta_0 + beta_1 * PER + beta_2 * PER^2
#     reg3: avg_salary = beta_0 + beta_1 * PER + beta_2 * tot_games
#     reg4: avg_salary = beta_0 + beta_1 * PER + beta_2 * tot_games * 1(ln_CPC < 500) + beta_3 * tot_games * 1(ln_CPC >= 50)
#     reg5: avg_salary = beta_0 + beta_1 * PER + beta_2 * tot_games * 1(ln_CPC < 500) + beta_3 * tot_games * 1(ln_CPC >= 50) + beta_4 * draft_round 
#     reg6: avg_salary = beta_0 + beta_1 * PER + beta_2 + beta_3 * draft_round, weights: total games (weighted-ols)
#     reg7: avg_salary = beta_0 + beta_1 * PER + beta_2 * PER^2 + beta_3 * tot_games * 1(tot_games < 500) + beta_4 * tot_games * 1(tot_games >= 50) + beta_5 * draft_round 

 
# Regression 1: Simple regression with squared PER 
reg1 <- lm_robust(avg_salary ~ PER, data = df)
summary(reg1)


# Regression 2: Simple regression with squared PER 
reg2 <- lm_robust(avg_salary ~ PER + PER_sq, data = df)
summary(reg2)


# Regression 3: Multiple regression, controlling for the number of games played
reg3 <- lm_robust(avg_salary ~ PER + tot_games, data = df)
summary(reg3)


# Regression 4: Multiple regression controlling for the number of games played, with linear spline at 450
reg4 <- lm_robust(avg_salary ~ PER + lspline(tot_games,cutoff), data = df)
summary(reg4)


# Regression 5: Multiple regression with number of games played per career (spline) and draft round 
reg5 <- lm_robust(avg_salary ~ PER + lspline(tot_games, cutoff) + draft_round, data = df)
summary(reg5)


# Regression 6: Weighted-OLS, using reg4 setup and weight with total games 
reg6 <- lm_robust(avg_salary ~ PER + draft_round, data = df, weights = tot_games )
summary( reg6 )

# Don't add the quadratic term, it will complicate the interpretation and the benefit is not that significant
# Regression 7: Weighted-OLS, using reg4 setup and weight with total games 
reg7 <- lm_robust(avg_salary ~ PER + PER_sq + lspline(tot_games, cutoff) + draft_round, data = df)
summary(reg7)

#####
# Creating model summary with texreg
data_out <- "https://github.com/steveJ34/DA2_Term/tree/main/Out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4, reg5, reg6, reg7),
         type = 'html',
         custom.model.names = c("Simple Regression","Quadratic Model","Extended Model w. Tot games ",
                                "Extended Model w. Total games (PLS)", "Extended Model w. Tot games (PLS) & Draft round", 
                                "Weighted OLS", "Quadratic Model w. Tot games (PLS) & Draft round" ),
         caption = "Modelling average salaries of NBA players and different performance statistics",
         file = paste0( data_out ,'model_comparison.html'), digits = 5, include.ci = FALSE)


######
# Based on model comparison our chosen model is reg5 - avg_salary ~ PER + lspline(tot_games, cutoff) + draft_round
#   Substantive: - although somewhat complex, the model still enables easier interpretation that works properly for players
#                - magnitudes of coefficients seem to be meaningful
#   Statistical: - adjusted R2 is the second highest among the models
#                - most of the the variables are highly significant 
#                - due to the sample being rather small, a highly "tailored" model can lead to overfitting 

######
# Residual analysis.


# Get the predicted y values from the model
df$reg5_y_pred <- reg5$fitted.values
# Calculate the errors of the model
df$reg5_res <- df$avg_salary - df$reg5_y_pred

# Find players with largest negative errors
neg_err <- df %>% top_n( -5 , reg5_res ) %>% 
  select( name , avg_salary , reg5_y_pred , reg5_res )
neg_err

# Find players with largest positive errors
pos_err <- df %>% top_n( 5 , reg5_res ) %>% 
  select( name , avg_salary , reg5_y_pred , reg5_res )
pos_err

# Visualizing fit, using y_hat-y for reg5 


# Predict is more general and can handle missing values...
df <- mutate( df , y_hat = predict( reg5 , df ) )

# Create: y_hat-y plot
ggplot( data = df ) +
  geom_point (aes( x = y_hat , y = avg_salary ) ,  color="red")+
  geom_line( aes( x = avg_salary , y = avg_salary ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted salaries", y = "Actual salaries")


# BIC and AIC measures of the model:

# lm_robust cannot be used for this purpose, thus simple lm is to be used. In this case SEs are not of main importance. 

# Question: Does adding the Total games and Draft Round increase the prediction? 
#   - Using the same number of observations to compare models
reg3_lm <- lm(avg_salary ~ PER + tot_games, data = df)

reg4_lm <- lm(avg_salary ~ PER + lspline(tot_games,cutoff), data = df)

reg5_lm <- lm(avg_salary ~ PER + lspline(tot_games, cutoff) + draft_round, data = df)

BIC(reg3_lm, reg4_lm, reg5_lm)
AIC(reg3_lm, reg4_lm, reg5_lm)

#################################
## Prediction uncertainty
#

# Calculating the SEs for each point:
 pred5_CI <- predict( reg5, newdata = df , se.fit=T,
                 interval ="confidence" , alpha = 0.05 )

# Hand made CI for regression line
# 1) Add to datatset:
df <- df %>% mutate( CI_reg5_lower = pred5_CI$fit[,2],
                     CI_reg5_upper = pred5_CI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = PER, y = avg_salary ) , color='gray') +
  geom_line( data = df, aes( x = PER, y = reg5_y_pred ) , color = 'red' , size = 0.5 ) +
  geom_line( data = df, aes( x = PER, y = CI_reg5_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = df, aes( x = PER, y = CI_reg5_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "Average Player Efficiency Rating",y = "Average Salary ($MM)") +
  scale_x_continuous(breaks = c(1,2,5,10,20,50,100,200,500,1000,10000))


##
# Calculating prediction intervals 
#
pred5_PI <- predict( reg5, newdata = df , interval ="prediction" , alpha = 0.05 )

# Hand made Prediction Interval for regression line
# 1) Add to datatset (You can use the SE's as well if you wish...
#                        then alpha does not have any meaning)
df <- df %>% mutate( PI_reg5_lower = pred5_PI$fit[,2],
                     PI_reg5_upper = pred5_PI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = PER, y = avg_salary ) , color='blue') +
  geom_line( data = df, aes( x = PER, y = reg5_y_pred ) , color = 'red', size = 0.25 ) +
  geom_line( data = df, aes( x = PER, y = PI_reg5_lower ) , color = 'green' ,
             size = 1 , linetype = "dotted" ) +
  geom_line( data = df, aes( x = PER, y = PI_reg5_upper ) , color = 'black' ,
             size = 1 , linetype = "dotted" ) +
  labs(x = "Average Player Efficiency Rating",y = "Average Salary ($MM)") +
  scale_x_continuous(breaks = c(1,2,5,10,20,50,100,200,500,1000,10000))



#################################
## Testing hypothesis
# The test hypothesis is the following $H_0: \betas = 0, \, H_A: \betas \neq 0$ or not in our model. 
# The estimated t-statistics is `r round(reg5$statistic[2],2)`, with p-value: `r reg5$p.value[2]`. 
# Choosing a significance level of p = 0.05. 
summary( reg5 )
# Thus we reject the $H_0$, which means the average salary per career of an NBA player is not uncorrelated with PER, total games played and draft round in which the player was selcted.


######
# Robustness checks 


## Testing for external validity, by running the model on the sample of players who played in the NBA from 1988 to 2003 

# Taking a random sample from original sample to enable comparison between the two samples 
df_sample <- df[sample(nrow(df), 389), ]

# Re-running the model on the random data sample

reg5_t1 <- lm_robust(avg_salary ~ PER + lspline(tot_games, cutoff) + draft_round, data = df_sample)
summary(reg5_t1)

# Importing the test sample 
my_data_1 <- "https://raw.githubusercontent.com/steveJ34/DA2_Term/main/Data/Clean/nba_clean_rob.csv"
df_test <- read_csv( my_data_1 )

# Running the model on the test data 
reg5_t2 <- lm_robust(avg_salary ~ PER + lspline(tot_games, cutoff) + draft_round, data = df_test)
summary(reg5_t2)

# Creating model summary with texreg
data_out <- "https://github.com/steveJ34/DA2_Term/tree/main/Out"
htmlreg( list(reg5_t1 , reg5_t2),
         type = 'html',
         custom.model.names = c("Random Sample (original data)","Sample 1988-2003"),
         caption = "Testing robustness of model on different samples",
         file = paste0( data_out ,'robust_check.html'), digits = 5, include.ci = FALSE)
