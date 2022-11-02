Laptop_dataset <- read.csv("C:/Users/Lenovo/Desktop/Research Article/Dataset( Laptops).csv")
head(Laptop_dataset)

Top_brand1 <- Laptop_dataset[which(Laptop_dataset$Which.brand.of.laptop.do.you.have. == "12"),] #37 #HP
Top_brand2 <- Laptop_dataset[which(Laptop_dataset$Which.brand.of.laptop.do.you.have. == "11"),] #28 #Lenovo
Top_brand3 <- Laptop_dataset[which(Laptop_dataset$Which.brand.of.laptop.do.you.have. == "10"),] #19 #Dell
New_dataset <- rbind(Top_brand1,Top_brand2,Top_brand3)


New_dataset['Which.brand.of.laptop.do.you.have.'] <- lapply(New_dataset['Which.brand.of.laptop.do.you.have.'], as.character)

New_dataset
Reviews.on.the.Internet
Friends.Recommendations
Advertisements
# summary statistics for dependent variable Review on the internet
New_dataset %>% group_by(Which.brand.of.laptop.do.you.have.) %>%  summarise(n = n(), mean = mean(Reviews.on.the.Internet), sd = sd(Reviews.on.the.Internet))

# summary statistics for dependent variable Friends recommendations
New_dataset %>% group_by(Which.brand.of.laptop.do.you.have.) %>%  summarise(n = n(), mean = mean(Friends.Recommendations), sd = sd(Friends.Recommendations))

# summary statistics for dependent variable Advertisements
New_dataset %>% group_by(Which.brand.of.laptop.do.you.have.) %>%  summarise(n = n(), mean = mean(Advertisements), sd = sd(Advertisements))

dep_vars <- cbind(New_dataset$Reviews.on.the.Internet, New_dataset$Friends.Recommendations,New_dataset$Advertisements)
fit <- manova(dep_vars ~ Which.brand.of.laptop.do.you.have., data = New_dataset)
summary(fit)
# Inference
By default , MANOVA in R uses Pillai's Test Statistic default .The P value is greater than > 0.05
which indicates that the Branches (A,B,C) is not having a statistically significant difference(i.e, the
group mean vectors are same for the dependent variables)
The association with the dependent variables Reviews.on.the.Internet,Friends.Recommendations,Advertisements are
similar.We accept the null hypothesis H0.
# Summary
# There is no statistically significant effect on the response variable which means that the dependent
variables have association in each of the brand (12,11,10) that is why the brands are in the top position

summary(fit, test='Wilks')

summary(fit, test='Hotelling-Lawley')

summary(fit, test='Roy')

library(MASS)
post_hoc <- lda(New_dataset$Which.brand.of.laptop.do.you.have. ~ dep_vars, CV=F)
post_hoc

plot_lda <- data.frame(New_dataset[,"Which.brand.of.laptop.do.you.have."], lda = predict(post_hoc)$x)
ggplot(plot_lda) + geom_point(aes(x = lda.LD1, y = lda.LD2, colour = New_dataset$Which.brand.of.laptop.do.you.have.), size = 3)

library(tidyverse)

model <- lm(Which.brand.of.laptop.do.you.have. ~ Price+Brand+Guarentee+Reviews.on.the.Internet+Friends.Recommendations+Advertisements+Discounts..sales..on.laptops..notebooks..computers, data = Laptop_dataset)
summary(model)

summary(model)$coefficient

###### install.packages("lavaan")
library(lavaan)
install.packages("haven")


Laptop_dataset <- read.csv("C:/Users/Lenovo/Desktop/Research Article/Dataset( Laptops).csv")
head(Laptop_dataset)

###CFA MODEL

BrandSelection.Model <- 'brandvalue=~Price+Guarentee+Reviews.on.the.Internet+Friends.Recommendations+Advertisements+Discounts..sales..on.laptops..notebooks..computers'

#Fitting the model

fit <- cfa(BrandSelection.Model,data=Laptop_dataset)
summary(fit, standardized = TRUE, fit.measures = TRUE)

inspect(fit,'r2')

install.packages("semPlot")
library(semPlot)
semPaths(fit,"std")
