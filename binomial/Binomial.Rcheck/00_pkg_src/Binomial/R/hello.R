#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials.
#' @param n trials
#' @return k success
#' @export an object of class \code{"bin_choose"}
#' @examples
#' # 5 choose 2
#' bin_choose(n = 5, k = 2)
#' # 5 choose 1,5 choose 2,5 choose 3
#' bin_choose(5, 1:3)

#Main function: add  roxygen comments(delete)
bin_choose <- function(n,k){
  condition <- sum(k <= n)
  if(condition == length(k)){
    factorial(n)/(factorial(k) * factorial(n - k))
  }
  else{
    stop("k cannot be greater than n")
  }
}


#Main function: add  roxygen comments(delete)
bin_probability <- function(success, trials, prob){
  if(check_trials(trials) & check_prob(prob) & check_success(success, trials)){
    bin_choose(trials, success) * prob^success * (1- prob)^(trials - success)
  }
}



#Private funtion to check if the input is a vaild probability.
check_prob <- function(prob){
  if((prob <= 1) & (prob >= 0)){
    TRUE
  }
  else{
    stop('p has to be a number betwen 0 and 1')
  }
}


#Private function to check if an input trials is a valid value.
check_trials <- function(trials){
  if(trials >= 0){
    TRUE
  }
  else{
    stop('invalid trials value')
  }
}


#Private function to check if an input success is a valid value for number of successes.
check_success <- function(success, trials){
  condition.1 <- sum(success >= 0)
  condition.2 <- sum(success <= trials)
  if(condition.1 == length(success) && condition.2 == length(success)){
    TRUE
  }
  else{
    stop('invalid success value')
  }
}


#Private function to get mean
aux_mean <- function(trials,prob){
  trials * prob
}



#Private function to get variance
aux_variance <- function(trials,prob){
  trials * prob * (1- prob)
}



#Private function to get mode
aux_mode <- function(trials,prob){
  if(is.integer(trials * prob + prob)){
    c(trials * prob + prob - 1, trials * prob + prob)
  }
  else{
    as.integer(trials * prob + prob)
  }
}



#Private function to get skewness
aux_skewness <- function(trials,prob){
  (1- 2 * prob) / sqrt(trials * prob * (1- prob))
}



#Private function to get kurtosis
aux_kurtosis <- function(trials,prob){
  (1- 6 * prob * (1- prob)) / (trials * prob * (1- prob))
}

