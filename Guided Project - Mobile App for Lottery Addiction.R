
# INTRODUCTION

# In this guided project, we will be working as an employee aiding in the design of a mobile app intended to help 
# lottery addicts to realistically understand their chances of winning. Considering that the fun of winning can 
# escalate into a habit and then to an addiction that can spiral towards accumulating debt that results in situations 
# that are not favorable, it would make sense to develop such an app. Specifically, you will be working as a data analyst
# tasked with building the logic behind this app and to calculate probabilities of winning. 

# For the first version of this app, we will be focusing on the Ontario lottery [6/49](https://en.wikipedia.org/wiki/Lotto_6/49). 
# This lottery essentially involves choosing a set of 6 numbers from 1 to 49 with a graded prizes depending on the degree of 
# matching. For our analysis, we will be looking at a [data set](https://www.kaggle.com/datascienceai/lottery-dataset) of
# 3665 drawings from 1982 to 2018. Ultimately we would like to answer the following questions:
  
#   - What is the probability of winning the big prize with a single ticket?
#   - What is the probability of winning the big prize if we play 40 different tickets (or any other numbers)?
#   - What is the probability of having at least five(or four or three or two) winning number on a single ticket?


# STEP 1: Reading the file + loading the necessary libraries

library(tidyr)
library(purrr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tibble)
library(readr)
library(aod)
library(epiR)
library(Rcpp)
library(nnet)
library(car) 
library(survival) #
library(survminer)
library(lmtest)
library(rms) 
library(ggfortify) 
library(sets)

lotto <- read.csv("C:/Users/micha/Downloads/649.csv")
View(lotto)



# STEP 2: Writing Core Functions 

# Since we will need to calculate factorials, combinations and such throughout this analysis, we will need to create 
# functions for these calculations. Remember that permutations is used to calculate the probabilities of an event where
# where the sampling involves no replacement and order matters. However in the instance where it does not matter, then 
# we will be using combination. 


factorial <- function(n) {
  final_number <- 1
  final_number <- as.numeric(final_number)
  for (i in 1:n){
    final_number <- final_number * i
  }
  return(final_number)
}

permutation <- function(n, k) {
  final <- factorial(n) / factorial(n-k)
  final <- as.numeric(final)
  return(final)
}

combination <- function(n, k){
  part_a <- factorial(n)/factorial(n-k)
  part_b <- 1/factorial(k)
  final <- part_a * part_b
  final <- as.numeric(final)
  return(final)
}



# STEP 3: One-Ticket Probability

# In the 6/49 lottery, you will select six numbers from a set of 49 numbers ranging from 1 to 49. 
# The player wins the big prize (essentially a large portion of the pooled pot worth millions) if all of the 6 numbers 
# match the chosen draw numbers. This does not include the bonus number that can also be selected for an additional cost.
# For the rest of the outcomes, it will be a prize ranging from a free play to a small share of the jackpot pool. 

# Knowing this, we want to be able to calculate the probability of winning the big prize (i.e. 6/6) with the 
# various numbers available on a single ticket. There are a few things that was made aware to us when formulating
# a means to determine this:

#   - user inputs six diferent numbers from 1 to 49
#   - the six numbers will come as a R vector which will serve as a single input
#   - print out the probability in a friendly way (i.e. make it so that really simple that a 10 year old can understand)


# PART A: Total possible combinations for a single ticket

# total_combo = combination(49, 6) # 13983816 total possible combinations to get all 6 numbers right


# PART B: Input and percentages 

# Understanding that this entailed function will have the participant input a single combination of 6 values, 
# which will be in the form of a vector (i.e. c(#,#,#,#,#,#)) the likelihood of an outcome would be 1 / 13983816. 

# options(scipen = 999) # Used to get rid of scientific notation 
# probability = round((1/total_combo) * 100, 9)
    

# PART C: Creating a message  

# From the probability, we want to then turn it into a string and then paste it on a statement that 
# says "You have a ______ % chance of getting the big prize."

# probability <- as.character(probability)
# statement <- cat("You have a " , , "% chance of winning the big prize.", sep = "")


# PART D: Putting it all together into a single function

one_ticket_probability <- function(numbers) {
  total_combo = combination(49, 6)
  probability = round((1/total_combo) * 100, 9) 
  probability = as.character(probability)
  statement = cat('You have a ', probability, '% chance of winning the big prize.', sep = '')
  return(statement)
} 

test <- one_ticket_probability(c(1,2,3,4,5,6))



# STEP 4: Historical Data Check 

# For the first version of the app, users should be able to compare their ticket against past winning
# combinations in the historyical lottery data in Canada. For now, lets check out the dataset. 

num_cols <- length(lotto)
num_rows <- count(lotto) 

dimensions <- dim(lotto)

head(lotto, 3)
tail(lotto, 3)



# STEP 5: New Data structure

data1 = c(1,3,5)
data2 = c(2,4,6)
data3 = c(8,9,7)

unnamed_list <- list(data1, data2, data3)
first_vector <- unnamed_list[[1]]

named_list <- list(first = data1, second = data2, third = data3)
first_item_sum <- named_list$first[1] + named_list$second[1] + named_list$third[1]



# STEP 6: Using PMAP

data_list = list(data1, data2, data3)

sums = pmap(data_list, function(x,y,z) {x+y+z})

averages <- pmap(data_list, function(x,y,z) {(x+y+z)/3})
first_average <- unlist(averages)[1]



# STEP 7: Function for Historical Data Check

# Now that we have an idea of the makeup of the data set, we want to now create a function that will enable
# users to compare their ticket against historical lottery data in Canada and determine whether they would 
# have ever won by now.

# Some details that we need to be aware of:
#   - inside the app, user will input six different numbers from 1 to 49
#   - under the hood, the six numbers will come as an R vector and serve as an input to our function
#   - the engineering team want us to write a function that prints:
#     > the number of times the combination selected occured in the Canada data set
#     > the probability of winning the big prize in the next drawing with that combination


# Part A: Extracting the six winning numbers in a column

# We want to create a way to be able to extract the 6 numbers from the columns [NUMBER.DRAWN.1 - NUMBER.DRAWN.6]. 
# Ideally, we want to use the pmap function which requires a list entry, then a function of some sort. So the first 
# step should be to create a list with these 6 numbers. 


list_test <- list(
  a <- lotto$NUMBER.DRAWN.1,
  b <- lotto$NUMBER.DRAWN.2,
  c <- lotto$NUMBER.DRAWN.3,
  d <- lotto$NUMBER.DRAWN.4,
  e <- lotto$NUMBER.DRAWN.5,
  f <- lotto$NUMBER.DRAWN.6
) 

class(list_test)

  
# Now that we have a list of values, we will now want to convert this into a vector which should have 3665 rows of them.

winning_numbers <- pmap(list_test, function(a,b,c,d,e,f) {c(a,b,c,d,e,f)})
View(winning_numbers)
head(winning_numbers)
lotto$winning_numbers <- winning_numbers


# PART B: Create the function

# Now that I created a list, I want to finally make the function that takes in two inputs: 
# (1) an R vector containing the user numbers 
# (2) the list containing the sets of winning numbers (i.e. that thing that I just made in PART A)

# So the input of the new function should be something along the lines of:

# some_name <- function(my_numbers, winning_numbers) {
#     SOMETHING THAT COMPARES MY NUMBERS TO PAST NUMBERS,
#     SOMETHING THAT PRINTS OUT NUMBER OF TIMES THAT MY NUMBERS MATCHES PAST NUMBERS,
#     SOME PHRASE INDICATING THAT MY SET OF NUMBERS HAS X% OF WINNING THE BIG PRIZE
  
#   return the phrase  
#   }

# To compare my numbers to past numbers, I will need to use the setequal function which will match TRUE or FALSE 
# depending if all of the entries of a given list will match the reference list (irregardless of order)

# Ex. 
set_a <- c('Foo', "Bar", "Baz")
set_b <- c("Not", 'At', 'All')
set_c <- c("Foo", "Baz", "Bar")
set_d <- c("Foo", "Bar", "Baz", "Not, At")
set_e <- c("Bar", "Baz", "Foo", "Foo", "Baz")

# setequal(test_list, reference_list)

setequal(set_a, set_b) # FALSE
setequal(set_a, set_c) # TRUE
setequal(set_a, set_d) # FALSE
setequal(set_a, set_e) # TRUE


# Thought process: I will need to use the map function since I am going down a series of rows to find matches
# Additionally, this should be saved into a variable like "match"

    # map(list, function(x) {setequal(list, ref_list)})

    # match <- map(my_numbers, function(x) {setequal(x, winning_numbers)})
    
    
    test_a <- function(nums, past_winners = winning_numbers) {
      historical_match <- map(past_winners, function(x) {setequal(x, nums)})
      return(historical_match)
    }
    
    sample_a <- test_a(c(3,11,12,14,41,43))
    
    class(sample_a) # it's a list
    
    # With this test, I ended up getting a giant print out of TRUE + FALSES. So, at the very least, there is an outcome
    
    
    test_b <- function(nums, past_winners = winning_numbers) {
      historical_match <- map(past_winners, function(x) {setequal(x, nums)})
      historical_match <- unlist(historical_match)
      return(historical_match)
    }
    
    sample_b <- test_b(c(3,11,12,14,41,43))
    
    class(sample_b)
    
    # With this test, I unlisted the return, so that now it return a different type of variable. In this case, it's a logical
    
    
    # Knowing that I now have a variable that stores the results of whether my set of numbers match the reference list, 
    # I want to know the number of times that these matches. Likely, I will need to get the sum of the counts of some kind. 

    test_c <- function(nums, past_winners = winning_numbers) {
      historical_match <- map(past_winners, function(x) {setequal(x, nums)})
      counts <- sum(unlist(historical_match))
      return(counts)
    }
    
    sample_c <- test_c(c(3,11,12,14,41,43)) 
    
    # With this test, it return the count of the number of matches that exists within the reference list  that matches my entries
  
    
    # Lastly, I want to create a phrase that will let the person know if this set of numbers was a winning set in the past.  
      

      test_d <- function(nums, past_winners = winning_numbers) {
        historical_match <- map(past_winners, function(x) {setequal(x, nums)})
        counts <- sum(unlist(historical_match))
        phrase <- cat("This set of numbers that you've entered matched ", counts, " times in the past lotto 6/49 draws.", sep = "")
        return(phrase)
      }
      
      sample_d <- test_d(c(3,11,12,14,41,43)) 
      
      # So this test seems to work. However, it's phrasing can be improved upon by differentiating b/t never been pulled, only once and more than once.  
      
      
      historical_check <- function(nums, past_winners = winning_numbers) {
        historical_match <- map(past_winners, function(x) {setequal(x, nums)})
        counts <- sum(unlist(historical_match))
        phrase <- ifelse(counts == 1, "This set of numbers that you've entered has been pulled once in past lotto 6/49 draws. The odds that you will win the big prize is 0.0000072%.", 
                  ifelse(counts == 0, "This set of numbers that you've entered has never been pulled once in previous lotto 6/49 draws. The odds that you will win the big prize is 0.0000072%.", 
                         cat("This set of numbers that you've entered matched ", counts, " times in the past lotto 6/49 draws. The odds that you will win the big prize is 0.0000072%.", sep = "")))
        return(phrase)
      }
      
      sample_e <- historical_check(c(3,11,12,14,41,43))
      sample_f <- historical_check(c(1,2,3,4,5,6))
      

      # Ultimately, we created a function that will check whether the selected 6 numbers (entered as a vector) had previously matched winning numbers in its entirety or not and, if so, the number
      # of times that it did match. Considering that there was never an instance of multiple occurance of the same numbers being selected, we couldn't fully examine this function's capabilities. 




# STEP 8: Multi Ticket Probability 

# One situation our functions do not cover is the issue of multiple tickets. Lottery addicts usually play more than one ticket on a single drawing, 
# thinking that this might increase their chances of winning significantly. Our purpose is to help them better estimate their chances of winning — on this screen, 
# we are going to write a function that will allow the users to calculate the chances of winning for any number of different tickets. 

# Talking with the engineering team, they gave us the following information:
  
#   - The user will input the number of different tickets they want to play (without inputting the specific combinations they intend to play).
#   - Our function will see an integer between 1 and 13,983,816 (the maximum number of different tickets).
#   - The function should print information about the probability of winning the big prize depending on the number of different tickets played.

# Ultimately our function will workout to something like this: 
  
#          multi_ticket_probability = function(num_of_tickets){
#            total_possibilities <- combination(49, 6) # should come out to 13983816 possible outcomes
#            chance_of_winning <- (num_of_tickets/total_possibilities) * 100 # should come down to some impossibly small number 
#            phrase <- cat("Based on the number of tickets that you plan to purchase, your chance of having a winnig ticket is " , chance_of_winning, "%.", sep = "")
#            return(phrase)
#          }

multi_ticket_probability = function(n) {
  total_possibilities <- combination(49,6)
  chance_of_having_one <- (n/total_possibilities) * 100
  phrase <- paste('Based on the number of tickets that you plan on purchasing, your chance of having a winning ticket is ', chance_of_having_one, "%. Fun fact, at $3 per ticket, the pot needs to be at least $52.77 million dollars if you are going to purchase every possible ticket combination just to break even and that is not considering someone else having the same set of 6 numbers.", sep = "")
  return(phrase)
} 

multi_ticket_probability(1)
multi_ticket_probability(10)
multi_ticket_probability(100)
multi_ticket_probability(1000)
multi_ticket_probability(10000)
multi_ticket_probability(100000)
multi_ticket_probability(13983816)

# Based on this function, we can show that out of the 13983816 possible tickets choices, the odds for having a ticket with all 6 numbers is going to be impossibly small. So we will
# create a text that outlines this as a percentage. 

# Fun fact: at $3 per ticket, the pot needs to be at least $52.77 million dollars for purchasing every possible ticket combination to just break even when considering that the 
# big prize winner only gets 79.5% of the total pot (and that assumes you are the only one with the winning 6 numbers, otherwise it is a split pot.)



# STEP 9: Less Winning Numbers 

# We're going to write one more function to allow the users to calculate probabilities for two, three, four, or five winning numbers.
# For extra context, in most 6/49 lotteries there are smaller prizes if a player's ticket matches two, three, four, or five of the six numbers drawn. 
# As a consequence, the users might be interested in knowing the probability of having two, three, four, or five winning numbers.

# Some details to be mindful of:
#  - Within the app, the user inputs (a) six different numbers from 1-49 and (b) an integer b/t 2 to 5 that represents the winning numbers expected
#  - Our function prints information about the probability of having the inputted number of winning numbers

# The specific combination on the ticket is irrelevant and we only need the integer between 3 and 5 representing the number of winning numbers expected. 
# Consequently, we will write a function which takes in an integer and prints information about the chances of winning depending on the value of that integer.

# The breakdown of the function 

# probability_less_6 <- function(integer) {
#   - Number of successful outcomes 
#   - number of total possible outcomes
#   - probability of number of successful outcomes and total possble outcomes
#   - some text display
# }


# PART A: Number of successful outcomes 

# combination_total = combination(6, n) # where n is the input integer

# PART B: Number of total possible outcomes 

# combination_remaining = combination(49-n, 6-n) 
# successful = combination_total * combination_remaining

# PART C: calculating probability of having matching numbers on a ticket

# total_combination = combincation(49, 6)
# probability = successful / total_combination * 100


# PART D: Finalizing the function 

probability_less_6 <- function(integer){
  n_successful_matches_on_ticket = combination(6, integer)  # this is to find the number of successful matches possible out of six numbers that are drawn
  n_remaining_combinations_on_ticket = combination(49 - integer, 6 - integer) # this is to find the number of matches that are left over after our projected number of correct matches out of 6 numbers drawn  
  successful_matches = n_successful_matches_on_ticket * n_remaining_combinations_on_ticket # this is the number of possible matches that I can have assuming I achieved the requested number of correctly selected numbers out of 6
  total_combination = combination(49,6) # this is the total possible combinations of all 6 numbers 
  possibility_percentage =  round((successful_matches / total_combination) * 100, 4) # percentage of the probability for me to win a prize based on the combination of correctly selected number drawn that are less than 6
  possibility = round(total_combination/successful_matches, 1) # just an inverse of the possibility percentage. 
  phrase = paste("Your chances of getting ", integer, " out of the 6 numbers drawn are ", possibility_percentage, "%. In other words, you have a 1 in ", possibility, " chance to win a prize.", sep = "")
  return(phrase)
}

probability_less_6(1)
probability_less_6(2)
probability_less_6(3)
probability_less_6(4)
probability_less_6(5)




# UPDATE: 
  
# Let us try to improve of the probability_less_6 function so that it will include a interesting statistic based on the integer entered.

update_less_than_6 <- function(integer){
  n_successful_matches_on_ticket = combination(6, integer)  
  n_remaining_combinations_on_ticket = combination(43, 6 - integer) # refer to explanation shown [here](https://community.dataquest.io/t/guided-project-mobile-app-for-lottery-addiction-definition-of-probability-less-6/287512/2)
  successful_matches = n_successful_matches_on_ticket * n_remaining_combinations_on_ticket 
  total_combination = combination(49,6) 
  possibility_percentage =  round((successful_matches / total_combination) * 100, 4) 
  possibility = round(total_combination/successful_matches, 1) 
  phrase_1 = "Your chances of getting 1 out of 6 winning numbers drawn are 41.3019%. In other words, you have a 1 in 2.4 chance to win a prize. This is the equivalent to the world's population of people under the age of 24 (as of 2019)."
  phrase_2 = "Your chances of getting 2 out of 6 winning numbers drawn are 13.2378%. In other words, you have a 1 in 7.6 chance to win a prize. This is also the equivalent world's population of people who have genital herpes."
  phrase_3 = "Your chances of getting 3 out of 6 winning numbers drawn are 1.765%. In other words, you have a 1 in 56.7 chance of win a prize. This is the equivalent of having birth a child with Down's syndrome."
  phrase_4 = "Your chances of getting 4 out of 6 winning numbers drawn are 0.0969%. In other words, you have a 1 in 1032.4 chance to win a prize. This is lower than the chance of having given birth to triplets."
  phrase_5 = "Your chances of getting 5 out of 5 winning numbers drawn are 0.0018%. In other words, you have a 1 in 54200.8 chance to win a prize. This is the mortality rate of children with COVID-19."
  phrase_6 = "Hey dummy, you've entered a value that doesn't work here (either a zero, something greater than 5, or a decimal number). Quit being a wise ass and enter a valid number."
  phrase = ifelse(integer == 1, phrase_1,
           ifelse(integer == 2, phrase_2, 
           ifelse(integer == 3, phrase_3, 
           ifelse(integer == 4, phrase_4, 
           ifelse(integer ==  5, phrase_5, phrase_6)))))
  return(phrase)
}

winning_nums <- c(0, 1, 2, 3, 4, 5)
for (n in winning_nums) {
  print(update_less_than_6(n))
}