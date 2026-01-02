#Reference about the dataset used in this analysis:
#https://www.kaggle.com/datasets/lovishbansal123/sales-of-a-supermarket?resource=download

supermarket_sales <- read.csv("C:/Users/tomma/OneDrive/Desktop/HOMEWORK DEC/supermarket_sales.csv")

#ONE SAMPLE T-TEST: 

#Is the average unit price 40$?
#H0: mu = 40$
#H1: mu != 40$

t.test(supermarket_sales$Unit.price, mu = 40)
#p-value < 2.2e-16 --> the average unit price is significantly different from $40

t.test(supermarket_sales$Unit.price, mu = 55)
#We know that the mean_unit.price is 55.6721 from this function. So, if we set mu = 55 (very close 
#to the original mean), we have p-value = 0.4226 which means that we fail to reject our hypothesis

#Is the average customer satisfaction 7?
#H0: mu = 7
#H1: mu != 7

t.test(supermarket_sales$Rating, mu = 7)
#This means that mu = 7 is a quite close value to the real mean of the dataset

#TWO SAMPLE T-TEST:

#Is there a difference in gross income between male and female customers?
#H0: There is no difference in the mean gross income between male and female customers 
#(mu_male = mu_female)
#H1: There is difference in the mean gross income between male and female customers 
#(mu_male != mu_female)

t.test(gross.income ~ Gender, data = supermarket_sales)
#p-value = 0.1181 --> there isn't a very big difference in gross income between male and female customers

#Is there a difference in customer satisfaction between male and female customers?
#H0: There is no difference in the mean customer satisfaction between male and female customers 
#(mu_male = mu_female) 
#H1: There is difference in the mean customer satisfaction between male and female customers 
#(mu_male != mu_female)

t.test(Rating ~ Gender, data = supermarket_sales)
#p-value = 0.8795 --> there isn't a very big difference in customer satisfaction between male and 
#female customers

#PAIRED T-TEST:

#Is there a significant difference in the customer satisfaction for purchases made before and after a 
#promotional campaign?
#But, in this dataset i don't see 2 variables that can be used for this typo of hypothesis testing,
#so it's not useful to perform it.

#An example of how to perform it could be the following one:

#p_t_test_result <- t.test(supermarket_sales$Unit.Price_before_promotion, 
#supermarket_sales$Unit.price_after_promotion, paired = TRUE)
#p_t_test_result

#Interpretation:
#p-value >= 0.05 --> we fail to reject the hypothesis
#p-value < 0.05 --> we reject the hypothesis

#ONE PROPORTION Z-TEST:

#Is the proportion of customers using 'Ewallet' as a payment method 60%?
#H0: The proportion of customers using 'Ewallet' as a payment method is 60%
#H1: The proportion of customers using 'Ewallet' as a payment method isn't 60%

x_ewallet <- sum(supermarket_sales$Payment == "Ewallet") #to count how many customers use E-wallet

result_ewallet1 <- prop.test(x_ewallet, n = 1000, p = 0.60, alternative = "two.sided")
result_ewallet1
#p-value < 2.2e-16 --> we reject our hypothesis because it's very different from the reality

result_ewallet2 <- prop.test(x_ewallet, n = 1000, p = 0.33, alternative = "two.sided")
result_ewallet2
#p-value = 0.3295 --> we fail to reject H0: p = 33% because is very close to the real proportion of the 
#(p_reality = 0.345)

#Is the proportion of customers buying 'Electronic accessories' 40%?
#H0: The proportion of customers buying 'Electronic accessories' is 40%
#H1: The proportion of customers buying 'Electronic accessories' is not 40%

x_elec_acc <- sum(supermarket_sales$Product.line == "Electronic accessories") 

result_ele_acc <- prop.test(x_elec_acc, n = 1000, p = 0.60, alternative = "two.sided")
result_ele_acc
#p-value < 2.2e-16 --> we reject our hypothesis because it's very different from the reality
#(p_reality = 0.17)

#TWO PROPORTION Z-TEST

#Is there a difference in the proportion of male and female customers using Credit card as a payment 
#method?
#H0: The proportions are equal (p_male = p_female)
#H1: The proportion are not equal (p_male != p_female)

#to know the number of male and female that use the credit card as payment method
creditcard_male <- sum(supermarket_sales$Payment == "Credit card" & supermarket_sales$Gender == "Male")
creditcard_female <- sum(supermarket_sales$Payment == "Credit card" & supermarket_sales$Gender == "Female")

#to know the number of male and female customers
total_male <- sum(supermarket_sales$Gender == "Male")
total_female <- sum(supermarket_sales$Gender == "Female")

prop_test_result <- prop.test(c(creditcard_male, creditcard_female), 
                              c(total_male, total_female))
prop_test_result
#p-value = 0.3608 --> we fail to reject our hypothesis because there isn't enough statistical 
#evidence to disprove.

#Is there a difference in the proportion of male and female customers being Member of the supermarket?
#H0: The proportions are equal (p_male = p_female)
#H1: The proportion are not equal (p_male != p_female)

member_male <- sum(supermarket_sales$Customer.type == "Member" & supermarket_sales$Gender == "Male")
member_female <- sum(supermarket_sales$Customer.type == "Member" & supermarket_sales$Gender == "Female")

total_male <- sum(supermarket_sales$Gender == "Male")
total_female <- sum(supermarket_sales$Gender == "Female")

prop_test_result <- prop.test(c(member_male, member_female), 
                              c(total_male, total_female))
prop_test_result
#p-value = 0.2295 --> we fail to reject our hypothesis because there isn't enough statistical 
#evidence to disprove.

#CHI-SQUARE TEST:

#Is the type of customer (Member or Normal) independent of payment method?
#H0: The two variables (Customer.type and Payment) are independent of each other 
#H1: The two variables (Customer.type and Payment) are not independent of each other

contingency_table1 <- table(supermarket_sales$Customer.type, supermarket_sales$Payment)

chi_square_result1 <- chisq.test(contingency_table1)
chi_square_result1
#p-value = 0.07364 --> we fail to reject H0
#X-squared = 5.217 --> the considered variables are quite dependent because when chi-square = 0, 
#it means that the 2 variables are the same!

#Is the gender of the customers independent from the quantity of products that are bought?
#H0: The two variable (Gender and Quantity) are independent of each other
#H1: The two variables (Gender and Quantity) are not independent of each other

contingency_table2 <- table(supermarket_sales$Gender, supermarket_sales$Quantity)

chi_square_result2 <- chisq.test(contingency_table2)
chi_square_result2
#p-value = 0.2699 --> we fail to reject H0
#X-squared = 11.086 --> the considered variables are quite dependent because when chi-square = 0, 
#it means that the 2 variables are the same!

#Analysis of Variance:

#Testing if Gross Income differs across Product Lines
#H0: The means of gross income across all product lines are equal
#H1: At least one product line's mean gross income is different

anova_result1 <- aov(gross.income ~ Product.line, data = supermarket_sales)
summary(anova_result1)
#F-value = 0.338 --> meaning the variance explained by Product.line is negligible compared to the 
#variance within groups.
#p-value = 0.89 --> we fail to reject our hypothesis

boxplot(gross.income ~ Product.line, data = supermarket_sales, 
        main = "Gross Income by Product Line", 
        xlab = "Product Line", ylab = "Gross Income", col = rainbow(6))
#the medians are quite similar across groups, it suggests that that there are no significant differences

#Testing if customer types differ across customer satisfaction
#H0: There is no significant difference in customer satisfaction ratings across different customer 
#types (Member or Normal) 
#H1: There is a significant difference in customer satisfaction ratings across different customer types

anova_result2 <- aov(Rating ~ Customer.type, data = supermarket_sales)
summary(anova_result2)
#F-value = 0.356 --> meaning the variance explained by Customer.type is negligible compared to the 
#variance within groups.
#p-value = 0.551 --> we fail to reject our hypothesis

boxplot(Rating ~ Customer.type, data = supermarket_sales, 
        main = "Customer Satisfaction by Customer Type", 
        xlab = "Customer Type", ylab = "Ratings", col = rainbow(2))
#the medians are quite similar across groups, it suggests that that there are no significant differences

#LINEAR REGRESSION:

#I want to predict Gross Income based on one predictor which is Quantity
#H0: There is no relationship between the independent variable (Quantity) and the dependent 
#variable (Gross.income)
#H1: There is a significant relationship between Quantity and Gross.income

linear_model1 <- lm(gross.income ~ Quantity, data = supermarket_sales)
summary(linear_model1)
#Since the p-value is less than 0.05, we reject the null hypothesis and conclude that the model is 
#statistically significant. This indicates that Quantity is a good predictor of gross.income
#p-value = 2.2e-16

library(ggplot2)

ggplot(supermarket_sales, aes(x = Quantity, y = gross.income)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Linear Regression: Gross Income vs Quantity",
       x = "Quantity",
       y = "Gross Income")
#This seems to be a good fit because the graph shows that the data points are quite close to the 
#regression line

#I want to predict Customer satisfaction on one predictor which is Quantity
#H0: There is no relationship between the independent variable (Quantity) and the dependent 
#variable (Rating)
#H1: There is a significant relationship between Quantity and Rating

linear_model2 <- lm(Rating ~ Quantity, data = supermarket_sales)
summary(linear_model2)
#Since the p-value is less than 0.05, we reject the null hypothesis and conclude that the model is 
#statistically significant. This indicates that Quantity is a good predictor of Customer satisfaction
#p-value = 2.2e-16

ggplot(supermarket_sales, aes(x = Quantity, y = Rating)) +
  geom_point(color = "green") + 
  geom_smooth(method = "lm", color = "orange") + 
  labs(title = "Linear Regression: Rating vs Quantity",
       x = "Quantity",
       y = "Rating")
#This seems to be a good fit because the graph shows that the data points are quite close to the 
#regression line

#MULTIPLE REGRESSION:

#I want to predict Gross Income based on some predictors: Quantity, Unit.price and Tax
#H0: None of the independent variables (predictors) have a significant effect on the dependent variable
#H1: At least one of the independent variables (predictors) has a significant effect on the dependent variable

multiple_model1 <- lm(gross.income ~ Quantity + Unit.price + Tax.5., data = supermarket_sales)
summary(multiple_model1)
#Since all the p-values are less than 0.05, we reject the null hypothesis and conclude that the model is 
#statistically significant. This indicates that Quantity, Unit Price and Tax are good predictors of gross.income
#p-value = 2.2e-16

#Multiple regression models can't easily be plotted directly because you have more than one predictor. 
#However, you can create partial regression plots (for each predictor) to visualize how each predictor 
#affects the outcome while holding other predictors constant.

#In the part related to Linear Regression, I already created a graph that shows the relationship between
#Gross.income and Quantity. So, i didn't create it again.

ggplot(supermarket_sales, aes(x = Unit.price, y = gross.income)) + 
  geom_point(color = "green") + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Multiple Regression: Gross Income vs Unit Price",
       x = "Unit Price", y = "Gross Income")
#This seems to be a good fit because the graph shows that the data points are quite close to the 
#regression line

ggplot(supermarket_sales, aes(x = Tax.5., y = gross.income)) + 
  geom_point(color = "yellow") + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Multiple Regression: Gross Income vs Tax",
       x = "Tax", y = "Gross Income")
#This seems to be a good fit because the graph shows that the data points are quite close to the 
#regression line

#I want to predict Customer satisfaction based on some predictors: Quantity and Unit.price
#H0: None of the independent variables (predictors) have a significant effect on the dependent variable
#H1: At least one of the independent variables (predictors) has a significant effect on the dependent variable

multiple_model1 <- lm(Rating ~ Quantity + Unit.price, data = supermarket_sales)
summary(multiple_model1)
#Since all the p-values are more than 0.05, we fail to reject the null hypothesis and conclude that the model is 
#not statistically significant. This indicates that Quantity and Unit Price are not good predictors of gross.income
#p-value_quantity = 0.620 and p-value_Unit.price = 0.786 

ggplot(supermarket_sales, aes(x = Unit.price, y = Rating)) + 
  geom_point(color = "pink") + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Multiple Regression: Rating vs Unit Price",
       x = "Unit Price", y = "Rating")
#This doesn't seems to be a good fit because the graph shows that the data points are not quite close to the 
#regression line

ggplot(supermarket_sales, aes(x = Quantity, y = Rating)) + 
  geom_point(color = "lightblue") + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "red")) +
  labs(title = "Multiple Regression: Rating vs Quantity",
       x = "Quantity", y = "Rating")
#This doesn't seems to be a good fit because the graph shows that the data points are not quite close to the 
#regression line

#LOGISTIC REGRESSION:

#I want to predict if a customer is a Member (1) or Normal (0) based on the Gender.
#H0: The coefficient for Gender (Male) is equal to zero. This means that there is no relationship 
#between Gender and the likelihood of a customer being a Member or a Normal customer.
#H1: The coefficient for Gender (Male) is not equal to zero. This suggests that Gender has a significant 
#relationship with the likelihood of a customer being a Member or a Normal customer.

supermarket_sales$binary_customer.type <- ifelse(supermarket_sales$Customer.type == "Member", 1, 0)

logit_model1 <- glm(binary_customer.type ~ Gender, data = supermarket_sales, family = "binomial")
summary(logit_model1)
#Both the Intercept and the GenderMale coefficients have p-values greater than 0.05, meaning that there 
#is no significant evidence to suggest that gender influences the likelihood of a customer being a 
#"Member" in this dataset. So, we fail to reject H0.

supermarket_sales$predicted_prob1 <- predict(logit_model1, type = "response")

ggplot(supermarket_sales, aes(x = Gender, y = predicted_prob1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_point(aes(color = as.factor(Customer.type)), size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Logistic Regression: Probability of Being a Member",
       x = "Gender",
       y = "Predicted Probability",
       color = "Customer Type") +
  theme_minimal()
#The bars represent the predicted probabilities of being a Member based on Gender
#The points show the actual observations, colored based on whether the customer is a Member (blue) 
#or Normal (red).

#I want to predict if a customer is a Male (1) or Female (0) based on the Quantity
#H0: The coefficient for Quantity is equal to zero. This means that there is no relationship 
#between Quantity and the likelihood of a customer being a Male or a Female customer.
#H1: The coefficient for Quantity is not equal to zero. This suggests that Quantity has a significant 
#relationship with the likelihood of a customer being a Male or a Female customer.

supermarket_sales$binary_gender <- ifelse(supermarket_sales$Gender == "Male", 1, 0)

logit_model2 <- glm(binary_gender ~ Quantity, data = supermarket_sales, family = "binomial")
summary(logit_model2)
#Both the Intercept and the Unit.price coefficients have p-values greater than 0.05, meaning that there 
#is no significant evidence to suggest that Quantity influences the likelihood of a customer being a 
#"Male or Female" in this dataset. So, we fail to reject H0.

supermarket_sales$predicted_prob2 <- predict(logit_model2, type = "response")

ggplot(supermarket_sales, aes(x = Quantity, y = predicted_prob2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_point(aes(color = as.factor(Gender)), size = 3) +
  scale_color_manual(values = c("orange", "lightblue")) +
  labs(title = "Logistic Regression: Probability of Being a Male",
       x = "Quantity",
       y = "Predicted Probability",
       color = "Gender") +
  theme_minimal()
#The bars represent the predicted probabilities of being a Male or a Female based on Quantity
#The points show the actual observations, colored based on whether the customer is a Female (orange) 
#or Male (light blue).

#POISSON REGRESSION:

#I want to model the number of items purchased (Quantity) based on the Gender of the customer
#H0: There is no significant relationship between the independent variable (Gender) and the dependent 
#variable (Quantity), gender does not affect the number of items purchased.
#H1: There is a significant relationship between the independent variable (Gender) and the dependent 
#variable (Quantity), gender affects the number of items purchased.

poisson_model1 <- glm(Quantity ~ Gender, family = "poisson", data = supermarket_sales)
summary(poisson_model1)
#The p-value for GenderMale is 0.00347, which is less than 0.05, indicating that Gender is a significant 
#predictor of the purchase count. Specifically, male customers are expected to purchase fewer items 
#than female customers, and this result is statistically significant.

ggplot(supermarket_sales, aes(x = Gender, y = Quantity)) +
  geom_boxplot(aes(fill = Gender)) +
  theme_minimal() +
  labs(title = "Poisson Regression: Quantity Purchased by Gender", x = "Gender", y = "Quantity Purchased")
#We can see that female's median is quite greater compared to male's median. This shows that females tend
#to buy a bigger number of items compared to males.

#I want to model the value of the Customer satisfaction (Rating) based on the Gender of the customer
#H0: There is no significant relationship between the independent variable (Gender) and the dependent 
#variable (Rating), gender does not affect the value of customer satisfaction.
#H1: There is a significant relationship between the independent variable (Gender) and the dependent 
#variable (Rating), gender affects the value of customer satisfaction.

poisson_model2 <- glm(Rating ~ Gender, family = "poisson", data = supermarket_sales)
summary(poisson_model2)
#The p-value for GenderMale is 0.921, which is more than 0.05, indicating that Gender is not a significant 
#predictor of the customers satisfaction. 

ggplot(supermarket_sales, aes(x = Gender, y = Rating)) +
  geom_boxplot(aes(fill = Gender)) +
  theme_minimal() +
  labs(title = "Poisson Regression: Customer satisfaction by Gender", x = "Gender", y = "Ratings")
#We can see that female's median is more or less the same compared to male's median. This shows that there is
#no relationship between customer satisfaction and the gender of the customers.

#THE END
