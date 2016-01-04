library(plyr)



#' Time series of MoJo (similarity to final) distance
user_mojo_history <- (function(){
  data <- read.csv("data-raw/mojo_no_undo.csv")
  dist.time.noundo <- ddply(data, .(UID), transform, RelativeTime=Time-min(Time))
  dist.time.noundo$MoJo <- dist.time.noundo$toMoJo


  dist.time.noundo$window <- as.POSIXct(
    120 * (dist.time.noundo$RelativeTime %/% 120),
    origin="1/1/1970"
  )

  dist.time.noundo <- ddply(dist.time.noundo, .(window), transform, minD=min(MoJo), maxD=max(MoJo), avD=mean(MoJo))

  dist.time.noundo$RelativeTime <- as.POSIXct(dist.time.noundo$RelativeTime, origin="1/1/1970")

  dist.time.noundo$MoJo <- dist.time.noundo$toMoJo

  dist.time.noundo
})()
devtools::use_data(user_mojo_history)

#' Results from the user background questinonaire
user_survey_experience <- (function(){
  data <- read.csv("data-raw/survey_experience.csv")
  data$Years = factor(data$Years, levels=c("<1","1","2","3","4","5","6","7","8","9","10+"))

  data
})()
devtools::use_data(user_survey_experience)

#' Last snapshots from each time series
user_final_sizes <- (function(){
  last.rel.sizes <- read.csv("data-raw/last.rel.sizes.csv")
  last.rel.sizes$User <- 1:nrow(last.rel.sizes)
  last.rel.sizes
})()
devtools::use_data(user_final_sizes)


#' Summary of the time series data
#'
#' Includes time taken, MoJo distance, and UID.
user_results_summary <- (function(){
  data <- ddply(user_mojo_history, .(UID), function(df){
    totalTime <- max(df$RelativeTime)
    startMoJo <- df$MoJo[1]
    data.frame(UID=df$UID[1], totalTime=totalTime, startMoJo=startMoJo)
  })




  data <- merge(data, user_final_sizes, by="UID")
  data$TotalNo <- data$No + data$LockNo

  data
})()
devtools::use_data(user_results_summary)
