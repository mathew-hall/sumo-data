(rq3_kw_tests <- function(){

  # First find cases that have more than one MoJo value:
  results.75yr <- systematic_results[systematic_results$yesRatio == .75,]
  multipleMoJos <- ddply(results.75yr, .(MDGmdg), summarise, multipleMoJos=length(unique(startMOJO)) > 1)

  subset <- results.75yr[join(results.75yr, multipleMoJos, by="MDGmdg")$multipleMoJos,]

  writeLines(paste("Total results with multiple MoJo values:", sum(multipleMoJos$multipleMoJos)))

  p.values <- ddply(subset, .(MDGmdg), function(df){
  	df$totalRels <- df$positiveRelations + df$negativeRelations
  	res <- kruskal.test(formula=totalRels ~ startMOJO, data=df)
  	return(res$p.value)
  })

  p.values$adjusted <- p.adjust(p.values$V1, method="BH")

  alpha <- 0.05

  writeLines(paste("Significant results at alpha =, ",alpha, " after adjustment:", sum(p.values$adjusted < alpha)))

  p.values$significant <- with(p.values, adjusted < alpha)
  p.values
})()
