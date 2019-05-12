#####################################################################
# As a first step, you should run the code provided in the course 
# material, to download and edit needed data sets.
# "edx"        as training set.
# "validation" as test     set.
#####################################################################

# Load required packages.
library(magrittr)
library(tidyverse)
library(lubridate)
library(gridExtra)

#########################
# Modifing edx data set #
#########################
edx %<>% mutate(year      = str_remove(str_extract(string = title,pattern = "[:digit:]{4}\\)$"),pattern = "\\)" ) %>% as.integer(), # seperate "year" from "title" 
                title     = str_remove(string = title, pattern = "\\([:digit:]{4}\\)$"),                                            # remove last four digits from title;"year"
                timestamp = as_date(as_datetime(timestamp)),                                                                        # extract reviwe year from timestamp
                genres    = str_split(genres, pattern = '\\|'))                                                                     # split genres into a list


######################
# Building our model #
######################

# Calculationg over all rating avrage.
mu <-mean(edx$rating)

# Calculating movie effect
movie_avg <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))       # Average error per "movieId"

gc() # garbage collection, to clear unused memory

# Calculating user effect
user_avg  <- edx %>% 
  left_join(movie_avg, by = "movieId") %>% # Join by "movieId" to collect calculated movie effect
  group_by(userId) %>% 
  summarise(b_u= mean(rating - mu - b_i))  # Average Error per "userId"   

gc() # garbage collection, to clear unused memory

# Calculating year effect
year_avg  <- edx %>% 
  left_join(movie_avg, by = "movieId") %>%  # Join by "movieId" to collect calculated movie effect 
  left_join(user_avg, by= "userId") %>%     # Join by "userId"  to collect calculated user  effect
  group_by(year) %>% 
  summarise(b_y= mean(rating - mu - b_i - b_u)) # Average Error per "year"

gc() # garbage collection, to clear unused memory


# create predict function
# takes df with the same structure as edx
# returns input df binded with b_i, b_u, b_y and predicted rating y_hat

PredictRating <- function(df){
  df %>%
    left_join(movie_avg, by = "movieId") %>% # collect movie effect
    left_join(user_avg, by="userId") %>%     # collect user effect
    left_join(year_avg, by = "year") %>%     # collect year effect
    mutate(y_hat = mu + b_i  + b_u + b_y,    # calculate y_hat
           y_hat = if_else(y_hat >  5 ,  5.0, y_hat), # make sure that our predictions are within range
           y_hat = if_else(y_hat < 0.5 , 0.5, y_hat))} # make sure that our predictions are within range


# Predict rating by user; calculate y_hat
edx %<>% PredictRating()

# calculate model performance
# find performance per effect
rmse_mu  <- caret::RMSE(edx$rating, mu)                                     # using overall average mu
rmse_b_i <- caret::RMSE(edx$rating, mu + edx$b_i)                           # using overall average mu with movie effect
rmse_b_u <- caret::RMSE(edx$rating, mu + edx$b_i + edx$b_u)                 # using overall average mu with movie effect, user effect
rmse_b_y <- caret::RMSE(edx$rating, mu + edx$b_i + edx$b_u + edx$b_y)       # using overall average mu with movie effect, user effect and year effect


# create a dataframe to show model performance, training set
training_RMSE <- data.frame(`Included Effects` = c("mu","mu + b_i","mu + b_i + b_u", "mu + b_i + b_u + b_y"),
                            RMSE = c(rmse_mu, rmse_b_i, rmse_b_u, rmse_b_y),
                            Improvment = c(0,round((rmse_mu - rmse_b_i)*100/rmse_mu,2),
                                           round((rmse_b_i - rmse_b_u)*100/rmse_b_i,2),
                                           round((rmse_b_u - rmse_b_y)*100/rmse_b_u,2)))

# calculate Error per user review, training set
edx %<>% mutate(Error = rating - y_hat)


####################
# Predict test set #
####################

# Editing validation set to match edx set
validation %<>% mutate(year      = str_remove(str_extract(string = title,pattern = "[:digit:]{4}\\)$"),pattern = "\\)" ) %>% as.integer(),
                       title     = str_remove(string = title, pattern = "\\([:digit:]{4}\\)$"),
                       timestamp = as_datetime(timestamp),
                       genres    = str_split(genres, pattern = '\\|'))

# Apply predict function
validation%<>% PredictRating()


# calculating RMSE
rmse_mu_test  <- caret::RMSE(validation$rating, mu)
rmse_b_i_test <- caret::RMSE(validation$rating, mu + validation$b_i)
rmse_b_u_test <- caret::RMSE(validation$rating, mu + validation$b_i + validation$b_u)
rmse_b_y_test <- caret::RMSE(validation$rating, mu + validation$b_i + validation$b_u + validation$b_y)

# create a dataframe to show model performance, test set

test_RMSE <- data.frame(`Included Effects` = c("mu","mu + b_i","mu + b_i + b_u", "mu + b_i + b_u + b_y"),
                        RMSE = c(rmse_mu_test, rmse_b_i_test, rmse_b_u_test, rmse_b_y_test),
                        Improvment = c(0,round((rmse_mu_test - rmse_b_i_test)*100/rmse_mu_test,2),
                                       round((rmse_b_i_test - rmse_b_u_test)*100/rmse_b_i_test,2),
                                       round((rmse_b_u_test - rmse_b_y_test)*100/rmse_b_u_test,2)))

# calculate Error per user review, test set
validation %<>% mutate(Error = rating - y_hat)

