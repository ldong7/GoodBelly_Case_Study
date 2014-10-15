library("psych")
library('xtable')
library('car')
library("lmtest")
library("MASS")
library('stats')
library('ggplot2')

# Write console to file
sink('R_Console_Output.txt')           

# Function that output test result into LaTex code
table <- function(test){
  a <- c(test$statistic, test$p.value)
  b <- matrix(a,nrow=1)
  if (names(test$statistic)=='BP'){
    colnames(b) <- c('BP','p-value')
  }
  else{
    colnames(b) <- c('W','p-value')
  }
  print(xtable(b),include.rownames=F)  
}

# Load data
data <- read.csv("goodBellyData.csv")
# Subset data
data <- data[3:12]                        
# Rearrange columns
data <- cbind(data[2], data[1],data[3:10])


# Use all columns except store
data_overall <- cbind(data[1], data[3:10])

# Univariate Analysis/Data Exploration
xtable(describe(data_overall))

# Boxplots
pdf("plots/Boxplot_of_Units_Sold.pdf", width=5, height=7)
boxplot(data_overall[1], main="Units Sold", ylab="Units Sold", col="grey", pars=list(outcol="red"))
dev.off()

pdf("plots/Boxplot_of_Average_Retail_Price.pdf", width=5, height=7)
boxplot(data_overall[2], main="Average Retail Price", ylab="Average Retail Price",  col="grey", pars=list(outcol="red"))
dev.off()

pdf("plots/Boxplot_of_Natural_Retailers.pdf", width=5, height=7)
boxplot(data_overall[8], main="Natural Retailers", ylab="Natural Retailers",  col="grey", pars=list(outcol="red"))
dev.off()

pdf("plots/Boxplot_of_Fitness_Centers.pdf", width=5, height=7)
boxplot(data_overall[9], main="Fitness_Centers", ylab="Fitness_Centers",  col="grey", pars=list(outcol="red"))
dev.off()


# Plot col1 vs col2
pdf("plots/Units_Sold_vs_Average_Retail_Price_A.pdf", width=7, height=5)
plot(x=data_overall$Average.Retail.Price, y=data_overall$Units.Sold, main='Units Sold vs. Average Retail Price ', xlab='Average Retail Price', ylab='Units Sold')
dev.off()


# Linear Regression

# Model 1
MLR <- lm(as.formula(paste(colnames(data_overall)[1],'~ .')),data=data_overall)
xtable(summary(MLR))

# Residual plots and QQ plots
pdf("plots/Residuals_Plot_A.pdf", width=7, height=5)
residuals_vs_Y <- cbind(MLR$fitted.values, MLR$residuals)
plot(residuals_vs_Y,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_A.pdf", width=7, height=5)
hist(MLR$residuals, breaks=30, freq=F, ylim=c(0, .010), main='Histogram of Residuals', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_A.pdf", width=7, height=5)
plot(MLR$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot", type="o", col="black")
abline(h=0, col="red")
dev.off()

pdf("plots/Studentized_Residuals_A.pdf", width=7, height=5)
plot(studres(MLR), main='Studentized Residuals',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off()

pdf("plots/QQ_Plot_A.pdf", width=7, height=5)
qqPlot(MLR$residuals, distribution='norm', main='Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()
 
# BP Test
table(bptest(MLR))
# Shapiro Wilk Test
table(shapiro.test(MLR$residuals))
# End Model 1


# Model 2
# Box-cox Transformation
bc <- boxcox(MLR)
lambda <- bc$x[which.max(bc$y)]

# Transform data
data_overall_transform <- cbind(data_overall, data_overall[1]^lambda)
names(data_overall_transform)[10] <- 'transform_data'

# Plot col1 vs col2 after Transformation
pdf("plots/Units_Sold_Transformed_vs_Average_Retail_Price_B.pdf", width=7, height=5)
plot(x=data_overall_transform$Average.Retail.Price, y=data_overall_transform$transform_data, main='Units Sold (Transformed) vs. \nAverage Retail Price', xlab='Average Retail Price', ylab='Units Sold (Transformed)')
dev.off()

# Linear Regression
MLR_transform <- lm(as.formula(paste(colnames(data_overall_transform)[10],'~.-Units.Sold')),data=data_overall_transform)
xtable(summary(MLR_transform))

# Residual plots and QQ plots
pdf("plots/Residuals_Plot_Transformed_B.pdf", width=7, height=5)
residuals_vs_Y_transform <- cbind(MLR_transform$fitted.values, MLR_transform$residuals)
plot(residuals_vs_Y_transform,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot (Transformed Model)")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_B.pdf", width=7, height=5)
hist(MLR_transform$residuals, breaks=30, main='Histogram of Residuals\n(Transformed Model)', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_Transformed_B.pdf", width=7, height=5)
plot(MLR_transform$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot (Transformed Model)", type="o", col="black")
abline(h=0, col="red")
dev.off()
 
pdf("plots/Studentized_Residuals_transformed_B.pdf", width=7, height=5)
plot(studres(MLR_transform), main='Studentized Residuals (Transformed Model)',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off()
 
pdf("plots/QQ_Plot_transformed_B.pdf", width=7, height=5)
qqPlot(MLR_transform$residuals, distribution='norm', main='Normal Q-Q (Transformed Model)', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()
 
# BP Test
table(bptest(MLR_transform))
# Shapiro Wilk Test
table(shapiro.test(MLR_transform$residuals))
# End Model 2


# Model 3 endcap_model
data_endcap <- cbind(data[1],data[3],data[5])

# Linear Regression
MLR_endcap <- lm(as.formula(paste(colnames(data_endcap)[1],'~ .^2')),data=data_endcap)
xtable(summary(MLR_endcap))
 
# Residual plots and QQ plots
pdf("plots/Residuals_Plot_C.pdf", width=7, height=5)
residuals_vs_Y_endcap <- cbind(MLR_endcap$fitted.values, MLR_endcap$residuals)
plot(residuals_vs_Y_endcap,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_C.pdf", width=7, height=5)
hist(MLR_endcap$residuals, breaks=30, main='Histogram of Residuals', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_C.pdf", width=7, height=5)
plot(MLR_endcap$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot", type="o", col="black")
abline(h=0, col="red")
dev.off()

pdf("plots/Studentized_Residuals_C.pdf", width=7, height=5)
plot(studres(MLR_endcap), main='Studentized Residuals',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off() 

pdf("plots/QQ_Plot_C.pdf", width=7, height=5)
qqPlot(MLR_endcap$residuals, distribution='norm', main='Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# BP Test
table(bptest(MLR_endcap))
# Shapiro Wilk Test
table(shapiro.test(MLR_endcap$residuals))
# End Model 3


# Model 4
# Box-cox Transformation
bc_endcap <- boxcox(MLR_endcap)
lambda_endcap <- bc_endcap$x[which.max(bc_endcap$y)]

# Transform Data 
data_endcap_transform <- cbind(data_endcap, data_endcap[1]^lambda)
names(data_endcap_transform)[4] <- 'transform_data'
data_endcap_transform <- data_endcap_transform[2:4]

# Plot col1 vs col2 after Transformation
pdf("plots/Units_Sold_Transformed_vs_Average_Retail_Price_D.pdf", width=7, height=5)
plot(x=data_endcap_transform$Average.Retail.Price, y=data_endcap_transform$transform_data, main='Units Sold (Transformed) vs. \nAverage Retail Price', xlab='Average Retail Price', ylab='Units Sold (Transformed)')
dev.off()

# Linear Regresion
MLR_endcap_transform <- lm(as.formula(paste(colnames(data_endcap_transform)[3],'~.^2')), data=data_endcap_transform)
xtable(summary(MLR_endcap_transform))


# Residual plots and QQ plots
pdf("plots/Residuals_Plot_Transformed_D.pdf", width=7, height=5)
residuals_vs_Y_endcap_transform <- cbind(MLR_endcap_transform$fitted.values, MLR_endcap_transform$residuals)
plot(residuals_vs_Y_endcap_transform,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot (Transformed Model)")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_D.pdf", width=7, height=5)
hist(MLR_endcap_transform$residuals, breaks=30, main='Histogram of Residuals\n(Transformed Model)', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_Transformed_D.pdf", width=7, height=5)
plot(MLR_endcap_transform$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot (Transformed Model)", type="o", col="black")
abline(h=0, col="red")
dev.off()

pdf("plots/Studentized_Residuals_transformed_D.pdf", width=7, height=5)
plot(studres(MLR_endcap_transform), main='Studentized Residuals (Transformed Model)',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off()

pdf("plots/QQ_Plot_transformed_D.pdf", width=7, height=5)
qqPlot(MLR_endcap_transform$residuals, distribution='norm', main='Normal Q-Q (Transformed Model)', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# BP test
table(bptest(MLR_endcap_transform))
# Shapiro Wilk Test
table(shapiro.test(MLR_endcap_transform$residuals))
# End Model 4


# Model 5 demo model
data_demo <- cbind(data[1],data[3],data[6:8])


# Linear Regression
MLR_demo <- lm(Units.Sold~.^2,data=data_demo)
xtable(summary(MLR_demo))

# Residual plots and QQ plots
pdf("plots/Residuals_Plot_E.pdf", width=7, height=5)
residuals_vs_Y_demo <- cbind(MLR_demo$fitted.values, MLR_demo$residuals)
plot(residuals_vs_Y_demo,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_E.pdf", width=7, height=5)
hist(MLR_demo$residuals, breaks=30, freq=F, ylim=c(0, .012), main='Histogram of Residuals', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_E.pdf", width=7, height=5)
plot(MLR_demo$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot", type="o", col="black")
abline(h=0, col="red")
dev.off()


pdf("plots/Studentized_Residuals_E.pdf", width=7, height=5)
plot(studres(MLR_demo), main='Studentized Residuals',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off() 

pdf("plots/QQ_Plot_E.pdf", width=7, height=5)
qqPlot(MLR_demo$residuals, distribution='norm', main='Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# BP test
table(bptest(MLR_demo))
# Shapiro Wilk Test
table(shapiro.test(MLR_demo$residuals))
# End Model 5



# Model 6
# Linear Regression on all variables and respective interaction terms
MLR_inter <- lm(as.formula(paste(colnames(data_overall)[1],'~ .^2')),data=data_overall)
# Search Algorithm using AIC
stepAIC(MLR_inter,direction='both')
# Linear Model with lowest AIC
MLR_best_AIC <- lm(formula = Units.Sold ~ Average.Retail.Price + Sales.Rep + Endcap + Demo + Demo1.3 + Demo4.5 + Natural + Fitness + Sales.Rep:Endcap + Sales.Rep:Natural + Demo:Demo1.3 + Demo4.5:Fitness, data = data_overall)
xtable(summary(MLR_best_AIC))

# Residual plots and QQ plots
pdf("plots/Residuals_Plot_F.pdf", width=7, height=5)
residuals_vs_Y_bestaic <- cbind(MLR_best_AIC$fitted.values, MLR_best_AIC$residuals)
plot(residuals_vs_Y_bestaic,xlab="Fitted Values", ylab="Residuals", main="Residuals Plot")
abline(h=0, col="red")
dev.off()

pdf("plots/residual_hist_F.pdf", width=7, height=5)
hist(MLR_best_AIC$residuals, breaks=30, freq=F, ylim=c(0, .010), main='Histogram of Residuals', xlab='Residuals')
dev.off()

pdf("plots/Sequence_Plot_F.pdf", width=7, height=5)
plot(MLR_best_AIC$residuals, xlab="Entries", ylab="Residuals", main="Sequence Plot", type="o", col="black")
abline(h=0, col="red")
dev.off()
 
pdf("plots/Studentized_Residuals_F.pdf", width=7, height=5)
plot(studres(MLR_best_AIC), main='Studentized Residuals',xlab='Observations',ylab='Studentized Residuals')
abline(h=0, col="red")
dev.off()
 
pdf("plots/QQ_Plot_F.pdf", width=7, height=5)
qqPlot(MLR_best_AIC$residuals, distribution='norm', main='Normal Q-Q Plot', xlab='Expected Distribution', ylab='Theoretical Distribution')
dev.off()

# BP Test
table(bptest(MLR_best_AIC))
# Shapiro Wilk Test
table(shapiro.test(MLR_best_AIC$residuals))
# End Model 6


sink()