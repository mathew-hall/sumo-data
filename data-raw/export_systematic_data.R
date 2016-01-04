library(plyr)

#' Data frame of the case studies used in our experiemnts
#'
#' Columns: Directory, JAR, #Classes, #Packages
case_studies <- (function(){
  sizes <- read.table("data-raw/sizes.tsv")
  names(sizes) <- c("Directory", "JAR", "Classes", "Packages")
  sizes$MDGmdg <- paste(sizes$JAR,"mdg",sep=".")
  sizes
})()
devtools::use_data(case_studies)

#' Results from the automated experiment; one row per run of SUMO.
#'
#' There are 5 * 30 * 100 runs for each case study:
#' * 5 different positive feedback ratios
#' * 30 Bunch runs
#' * 100 SUMO runs
systematic_results <- (function(){
  load("data-raw/finished.runs.RData")

  finished.runs$MDGmdg <- gsub("mdg/[\\w-]+/", "", finished.runs$MDGmdg,perl=T)

  finished.runs.counts <- ddply(finished.runs, .(MDGmdg), summarise, count = length(MDGmdg))
  finished.runs.ok <- finished.runs.counts[finished.runs.counts$count==5 * 30 * 100,]
  finished.runs <- finished.runs[finished.runs$MDGmdg %in% finished.runs.ok$MDGmdg,]

  results <- merge(finished.runs,case_studies,by="MDGmdg", all.x=T)

  results$CaseStudy <- gsub(".jar.mdg","", results$MDGmdg, fixed=F)

  # Calculate general, worstcase
  results$Predicted.Negative  <- (results$Packages * (results$Packages-1)) / 2
  results$Predicted.Positive  <-  results$Classes - results$Packages
  results$Predicted.WorstCase <- (results$Classes * (results$Classes-1)) / 2
  results$Predicted.BestCase <- results$Predicted.Negative + results$Predicted.Positive

  results
})()

devtools::use_data(systematic_results)
