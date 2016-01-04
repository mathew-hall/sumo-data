(function(){
  #Set boundaries for each facet [note these will need tweaking if new data comes in]
  results.75yr <- systematic_results[systematic_results$yesRatio == .75,]

  medians <- ddply(results.75yr, .(CaseStudy), summarise, steps=median(stepsToConverge))
  medians <- arrange(medians, steps)

  results.75yr$CaseStudy <- factor(results.75yr$CaseStudy, levels=medians$CaseStudy)
  results.75yr = merge(results.75yr, medians, by="CaseStudy", all.x=T)

  results.75yr$scale = as.numeric(cut_number(results.75yr$steps, 4))

  results.75yr$ymax = max(c(50,results.75yr$stepsToConverge[results.75yr$scale==1]))
  results.75yr$ymax[results.75yr$scale == 2] = max(c(200,results.75yr$stepsToConverge[results.75yr$scale==2]))
  results.75yr$ymax[results.75yr$scale == 3] = max(c(600,results.75yr$stepsToConverge[results.75yr$scale==3]))
  results.75yr$ymax[results.75yr$scale == 4] = max(c(4000,results.75yr$stepsToConverge[results.75yr$scale==4]))


  #SCALE MARKERS: Removed because they show very little

  #points<-data.frame(
  #  scale=c(0,1,1,2,2,3),
  #	y=c(800,800,100,100,30,30),
  #	x=c(24,0,25,0,25,0))
  #points$xs <- points$x+1
  #END OF SCALE MARKERS

  rq2_boxplot <- ggplot(results.75yr, aes(CaseStudy,stepsToConverge)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle =  90, hjust =1,vjust=0.4), strip.text.y=element_blank(), strip.text.x=element_blank(),strip.background=element_blank(), axis.title.x=element_blank()) +
    facet_wrap(~scale, scales="free", nrow=1) +
    scale_y_continuous(expand=c(0,0)) +
    geom_blank(aes(y=ymax)) + geom_blank(aes(ymin=0)) + labs(y="SUMO Iterations to Converge")
  # + geom_segment(aes(x=x,y=y,yend=y,xend=xs), points, linetype="dashed")
  ggsave('inst/fig/rq2-boxplot.pdf', rq2_boxplot, width=12,height=8)

  d_ply(results.75yr, .(scale), function(df){
    ggplot(df, aes(CaseStudy,stepsToConverge)) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_text(angle =  90, hjust =1,vjust=0.4), strip.text.y=element_blank(), strip.text.x=element_blank(),strip.background=element_blank(), axis.title.x=element_blank()) +
      scale_y_continuous(expand=c(0,0)) +
      geom_blank(aes(y=ymax)) + geom_blank(aes(ymin=0)) + labs(y="SUMO Iterations to Converge") +
      coord_flip()
    # + geom_segment(aes(x=x,y=y,yend=y,xend=xs), points, linetype="dashed")
    ggsave(paste0('inst/fig/rq2-boxplot-facet-',df$scale[1],'.pdf'), width=12,height=8)
  })

})()

(function(){

  results.75yr <- systematic_results[systematic_results$yesRatio == .75,]

  results.75yr$Predicted.BestCase <- with(results.75yr, (Packages * (Packages-1) / 2) + (Classes - Packages))
  results.75yr$Predicted.WorstCase <- with(results.75yr, Classes * (Classes-1) / 2)
  results.75yr$TotalRelations <- with(results.75yr, positiveRelations + negativeRelations)


  summary.results.75 <- ddply(results.75yr, .(CaseStudy,Classes,Packages),
                              summarise,
                              mean=mean(TotalRelations),
                              CI.Lower=(t.test(TotalRelations)$conf.int[1]),
                              CI.Upper=(t.test(TotalRelations)$conf.int[2]),
                              Predicted.BestCase = Predicted.BestCase[1],
                              Predicted.WorstCase = Predicted.WorstCase[1],
                              As.Percent = (100 * (1 - ((Predicted.WorstCase - mean) / Predicted.WorstCase)))[1]
  )

  summary.results.75 <- ddply(summary.results.75,
                              .(Classes),
                              transform,
                              cbmin=min(Predicted.BestCase),
                              cbmax=max(Predicted.BestCase))
  #TODO: fix this plot to the histogram and show the line of max for all yes ratios.
  #rq2_rels_vs_worst <- ggplot(summary.results.75, aes(x=As.Percent)) + geom_histogram() + theme_bw() + labs(x="Mean proportion of worst case (lower is better)", y="Count")


  rq2_rels_vs_worst <- ggplot(summary.results.75, aes(Classes, mean)) +
    geom_point(aes(group=CaseStudy), size=1, shape=3) +
    #	geom_errorbar(aes(ymin=Predicted.BestCase, ymax=Predicted.WorstCase,group=MDGmdg)) +
    #	geom_line(aes(x=Classes,y=cbmin), size=0.4)+
    #	geom_line(aes(x=Classes,y=cbmax), size=0.4)+
    #	geom_ribbon(aes(x=Classes,ymin=cbmin, ymax=cbmax), shape=1, size=1.5)+
    #	geom_errorbar(aes(ymin=CI.Lower, ymax=CI.Upper,group=MDGmdg), size = 5) +
    stat_function(fun=function(x)x*(x-1)/2, geom="line", linetype=5,colour="black",size=0.4) +
    # geom_line(data=worstCaseSizes, aes(Classes,Predicted.WorstCase), linetype=5, colour="black", size=0.4) +
    #	geom_line(data=worstCaseSizes, aes(Classes,Predicted.WorstCase/16), linetype=3, colour="black", size=0.4) +
    #	geom_line(data=worstCaseSizes, aes(Classes,Predicted.WorstCase/2), linetype=3, colour="black", size=0.4) +
    #	geom_line(data=worstCaseSizes, aes(Classes,Predicted.WorstCase/5.108), linetype=3, colour="black", size=0.4) +
    theme_bw() +
    labs(x="Classes", y="Mean Relations")

  ggsave('inst/fig/rq2-rels-vs-worst.pdf', rq2_rels_vs_worst,width=5,height=4)
  ggsave('inst/fig/rq2-rels-vs-worst.png', rq2_rels_vs_worst,width=5,height=4)

})()

(function(){
  levels <- c("0%","25%","50%","75%","100%")
  weighted_results <- systematic_results[systematic_results$yesRatio != -1,]
  weighted_results$facet <- weighted_results$Classes < 150

  rq3_median_yesRatio <- ggplot(weighted_results, aes(Classes,stepsToConverge, group=factor(yesRatio),linetype=factor(yesRatio))) +
    stat_summary(fun.y="median", geom="point") +
    stat_summary(fun.y="median", geom="line") +
    theme_bw() +
    labs(y="Steps to Converge", group="Yes Weight", linetype="Yes Weight") +
    facet_wrap(~facet,scales="free",drop=T,as.table=F,nrow=2,) +
    theme(strip.text.y=element_blank(), strip.text.x=element_blank(), strip.background=element_blank(), legend.key.width = unit(0.9, "cm"), legend.direction = "horizontal", legend.position = "top")
  ggsave('inst/fig/rq3-median-yesRatio.pdf', rq3_median_yesRatio, width=5, height=4)
})()
