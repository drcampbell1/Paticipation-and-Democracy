#Introduction to the labs#

# I mentioned that the study of participation involves looking at evidence.
# Most of the questions are about: levels; styles; and factors that influence participation
# In these labs, we"ll look at the evidence.


#To do that, we need data and software to analyse it. These labs use RStudio to analyse the data.
# This is a programming-based environment, but I've written all of the code for you.
#All you need to do is run it.


#Introduction to the data file#

#In the labs we'll be using the ESS. This is the European Social Survey -
#a big cross-sectional survey conducted in 30 countries in Europe.


#Coverage
# To make the file more manageable, I've cut it down quite a bit.

#Basic Terminology#

#Dependent variable: something we are looking to explain, e.g. one (or more) forms of participation:

#Independent variable : something that influences our dependent variable (age, for example, may influence voting).


#Observation: someone we ask about their participation and age.
#Overall, then, this file contains :
# 8 different forms of participation;
# 8 different variables that may influence participation (or 9 if we include country)
# In total we have 12472 observations - or 12472 people gave us answers to our questions.




attach(ESS)
library(dplyr)
ESS %>% count(cntry)

#Let's look at the data file and see what forms of participation it contains#

View(ESS)
#We have:

#1. Voting; 2. Contacting politicians; 3 working for a political party; 4 signing a petition;
#5 wearing political paraphenalia; 6. signing a petition; 7. demonstrating; 8. boycotting; 9. voting Labour/Con (UK only);
#voting radical right#

#And we also have#

#Variables that might influence these forms of participation, including:

#1. Age; 2. Education; 3. gender; 4. unemployed. These are *structural* characteristics that might influence participation.

#Values and attitudes: trust in gov; attitudes towards EU Par;

# Exercise 1: Let's look at voting#

ESS %>% filter(!is.na(vote1)) %>% count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100)

#How does this vary by country?#

ESS %>% group_by(cntry) %>% filter(!is.na(vote1)) %>% count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100)



