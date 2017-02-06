library(ggplot2)
library(plyr)
library(reshape2)
(function(){
  actual.sizes <- ddply(systematic_results, .(MDGmdg), summarise, Classes=Classes[1], Packages=Packages[1])
  rq2_sizes <-
    ggplot(actual.sizes, aes(Classes,Packages)) +
    geom_point() +
    theme_bw()
  ggsave(rq2_sizes, filename="inst/fig/rq2-sizes.pdf",width=5,height=4)
})()

(function(){
  rq2_mojo_maxmin <-
    ggplot(user_mojo_history, aes(window)) +
    geom_line(aes(y=minD),colour="blue") +
    geom_line(aes(y=maxD),colour="red") +
    geom_line(aes(y=avD)) +
    theme_bw() +
    labs(x="Time", y="MoJo Distance")

  ggsave("inst/fig/rq2-mojo-maxmin.pdf", rq2_mojo_maxmin, width=5,height=3)
})()

(function(){
  d_ply(user_mojo_history, .(UID), function(df){

    p <- ggplot(df, aes(RelativeTime,MoJo)) +
      geom_line(size=0.25) +
      ggmap::theme_nothing() +
      scale_y_continuous(expand=c(0.2,0))  +
      scale_x_datetime(expand=c(0.01,1)) +
      theme(panel.margin = unit(0,"null"))
    #TODO: it'd be nice to have a point at the end to indicate zero, and maybe start MOJO
    thisPlotName <- paste("sparkline_",df$UID[1],sep="")
    thisPlotName <- gsub("spc", "",thisPlotName, fixed=T)
    thisPlotName <- gsub(' \\d{2}\\.\\d{2} \\d{2}\\.\\d{2}\\.\\d{2}',"",thisPlotName,perl=T)

    ggsave(paste(paste0('inst/fig/sparklines/',thisPlotName),"pdf",sep="."), p,width=20, height=3.7694, units="mm")
  })
})()


(function(){
  rq2_mojo_iteration <- ggplot(user_mojo_history, aes(Step,MoJo,group=UID)) +
    geom_line() +
    theme_bw() +
    labs(x="SUMO Iteration", y="MoJo Distance") +
    scale_x_continuous(expand=c(0,0), limits=c(0,80)) + scale_y_continuous(expand = c(0,1), limits=c(0,75))
  ggsave('inst/fig/rq2-mojo-iteration.pdf', rq2_mojo_iteration, width=6, height=5)
})()

(function(){
  #Note: this uses bw instead of minimal theme, change to disable gridlines.
  rq2_mojo_time <- ggplot(user_mojo_history, aes(RelativeTime, MoJo, group=UID)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x="Time",y="MoJo Distance") +
    scale_x_datetime(expand=c(0,0), limits=as.POSIXct(c(0,5400),origin="1/1/1970")) + scale_y_continuous(expand = c(0,1), limits=c(0,75))

  ggsave('inst/fig/rq2-mojo-time.pdf', rq2_mojo_time, width=6, height=6)
})()

(function(){
  melted <- melt(user_final_sizes, measure.vars=c("Yes","No"))
  rq2_rels_yesno <- ggplot(melted, aes(User,fill=variable)) +
    geom_bar(aes(y=value), stat="identity") +
    theme_bw() +
    labs(x= "User", y="Number of Relations", fill="Relation Type")

  ggsave('inst/fig/rq2-rels-yesno.pdf', rq2_rels_yesno, width=5,height=5)
})()

(average_time <- function(){

  user_mojo_history %>% group_by(UID) %>%
      summarise(
        mean_secs = mean(Time[2:length(Time)] - Time[1:(length(Time)-1)]),
        median_secs = median(Time[2:length(Time)] - Time[1:(length(Time)-1)]),
        n = n()
        )

})()

(grand_average_time <- function(){
  average_time() %>% summarise(grand_mean=mean(mean_secs)/60, grand_median=median(median_secs)/60, total_mean=sum(mean_secs * n)/sum(n)/60)
})()

(all_median <- function(){
  user_mojo_history %>%
      group_by(UID) %>%
      do({data.frame(relTime = .$Time[2:length(.$Time)] - .$Time[1:(length(.$Time)-1)])}) %>%
      ungroup() %>%
      summarise(median(relTime)/60)
})()

(positiveRatio <- function(){
  user_results_summary %>% summarise(mean(Yes/(TotalRels)))
})()


user_results_summary %>% group_by(usedLock) %>% summarise(mean(TotalRels/num))

