(function(){
  sizes <- case_studies
  sizes$MDG <- gsub(".jar", "", sizes$JAR)

  sizes <- sizes[order(sizes$MDG),]

  sizes_l <- sizes[1:50,]
  sizes_r <- sizes[51:100,]

  print(xtable::xtable(sizes_l[,c(6,3,4)]), include.rownames=F, only.contents=T, file="inst/tables/sizes_l.tex")
  print(xtable::xtable(sizes_r[,c(6,3,4)]), include.rownames=F, only.contents=T, file="inst/tables/sizes_r.tex")
})()
