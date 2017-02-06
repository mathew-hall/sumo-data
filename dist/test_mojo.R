results.75yr <- function(){
  systematic_results[systematic_results$yesRatio == .75,]
}

rq3_distinct_mojo_values <- function(){
  multipleMoJos <- ddply(results.75yr(),
                         .(MDGmdg),
                         summarise,
                         distinctMoJos=length(unique(startMOJO)),
                         multipleMoJos=length(unique(startMOJO)) > 1)

  metadata <- unique(systematic_results[,c("MDGmdg","Classes","Packages")])

  join(multipleMoJos, metadata, by="MDGmdg")
}


(rq3_kw_tests <- function(){

  # First find cases that have more than one MoJo value:
  multipleMoJos <- rq3_distinct_mojo_values()

  results.75yr <- results.75yr()

  subset <- results.75yr[join(results.75yr, multipleMoJos, by="MDGmdg")$multipleMoJos,]

  writeLines(paste("Total results with multiple MoJo values:", sum(multipleMoJos$multipleMoJos)))

  p.values <- ddply(subset, .(MDGmdg), function(df){
  	df$totalRels <- df$positiveRelations + df$negativeRelations
  	res <- kruskal.test(formula=totalRels ~ startMOJO, data=df)
  	res$p.value
  })

  p.values$adjusted <- p.adjust(p.values$V1, method="BH")

  alpha <- 0.05

  writeLines(paste("Significant results at alpha = ",alpha, " after adjustment:", sum(p.values$adjusted < alpha)))

  p.values$significant <- with(p.values, adjusted < alpha)
  join(multipleMoJos, p.values, by = 'MDGmdg')
})()


(rq3_signif_histogram <- function(){
  labels <- c('One MoJo Value', 'Multiple MoJo Values, Insignificant', 'Multiple MoJo Values, Significant')
  rq3_kw_tests() %>%
    mutate(
      fac =
        factor(ifelse(
          is.na(significant),
            labels[1],
            ifelse(significant,
                  labels[3],
                  labels[2])),levels = labels)
      ) %>%
      ggplot(aes(Classes)) +
      geom_histogram(binwidth=10,fill=NA, colour='black') +
      facet_grid(fac~.,)  +
      theme_bw() +
      labs(x="Subject Size (Classes)", y="Subject Count") -> g
    ggsave(filename = "inst/fig/rq3-mojo-signif.pdf", g, width=7,height=8)

    g
})()
