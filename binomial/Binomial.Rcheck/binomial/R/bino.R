#' @title bin_choose
#' @description Calculates the number of combinations in which k successes can occur in n trials.
#' @param n trials
#' @param k success
#' @return The number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' # 5 choose 2
#' bin_choose(n = 5, k = 2)
#' # 5 choose 1,5 choose 2,5 choose 3
#' bin_choose(5, 1:3)


bin_choose <- function(n,k){
  condition <- sum(k <= n)
  if(condition == length(k)){
    factorial(n)/(factorial(k) * factorial(n - k))
  }
  else{
    stop("k cannot be greater than n")
  }
}


#' @title bin_probability
#' @description Caculate the probability based on the number of success
#' @param success number of success
#' @param trials number of trails
#' @param prob probability per success
#' @return probability of success
#' @export
#' @examples
#'
#' # probability of getting 2 successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' # probabilities of getting 2 or less successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' #55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
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


#' @title bin_distribution
#' @description Generate a data frame with columns success and probability
#' @param trials number of trails
#' @param prob probability per success
#' @return probability of success
#' @export
#' @examples
#' # binomial probability distribution
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob){
  dat <- data.frame()
  for(i in (0:trials)){
    dat <- rbind(dat,c(i,bin_probability(i,trials,prob)))
  }
  colnames(dat) <- c("success","probability")
  class(dat) <- c("bindis", "data.frame")
  dat
}



# method:plot.bindis()
#' @export
plot.bindis <- function(x){
  dat <- x
  ggplot(dat,aes(success,probabilty)) +
   geom_bar(stat = "identity")
}



#' @title bin_cumulative
#' @description Generate a data frame with columns,probability,and cumulative probability
#' @param trials number of trails
#' @param prob probability per success
#' @return probability of success
#' @export
#' @examples
#' #binomial cumulative distribution
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials,prob){
  dat <- bin_distribution(trials,prob)
  cumulative <- c()
  for(i in (1:nrow(dat))){
      cumulative <- c(cumulative,sum(dat$probability[1:i]))
  }

  dat$cumulative <- cumulative
  class(dat) <- c("bincum", "data.frame")
  dat
}



#' @export
plot.bincum <- function(x){
  dat <- x
  plot(x = dat$success,y = dat$cumulative,type = "o")
}


bin_variable <- function(trials,prob){
  if(check_trials(trials) && check_prob(prob)){

  }
}


