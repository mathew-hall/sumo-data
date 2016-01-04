#' Generate a data frame of user overview (including link to sparklines)
gen.user.summary.table <- function(){

  summary.users <- user_results_summary

  summary.users$Plotname <- paste("figures/sparklines/sparkline_",summary.users$UID,".pdf",sep="")
  summary.users$Plotname <- gsub("spc", "",summary.users$Plotname, fixed=T)
  summary.users$Plotname <- gsub(' \\d{2}\\.\\d{2} \\d{2}\\.\\d{2}\\.\\d{2}',"",summary.users$Plotname,perl=T)


  subtable <- summary.users[,c("startMoJo", "Plotname", "totalTime", "Yes", "TotalNo", "usedLock")]
  subtable$Plotname <- paste("\\includegraphics{", subtable$Plotname, "}", sep="")

  subtable$Yes <- paste(ifelse(subtable$usedLock, "\\emph{", ""), subtable$Yes, ifelse(subtable$usedLock, "}", ""), sep = "")
  subtable$TotalNo <- paste(ifelse(subtable$usedLock, "\\emph{", ""), subtable$TotalNo, ifelse(subtable$usedLock, "}", ""), sep = "")

  subtable$totalTime <- format(subtable$totalTime, format="%H:%M:%S")
  names(subtable) <- c("MoJo", "Change","Time","Positive","Negative","Lock")
  subtable <- arrange(subtable, desc(MoJo), Lock)
  subtable <- subtable[,!(names(subtable) %in% c("Lock"))]
  subtable
}

#' Generate a histogram of user experience
#'
#' @return a ggplot2 object
user.experience.histogram <- function(){
  ggplot2::ggplot(user_survey_experience, aes(Years)) +
    ggplot2::geom_bar(stat="count") +
    ggplot2::scale_x_discrete(drop=F) +
    ggplot2::theme_bw() +
    ggplot2::labs(x="Years of Experience", y="Participants")
}

#' Generate a histogram of user MoJo scores (between start and end)
#'
#' @return a ggplot2 object
user.mojo.start.histogram <- function(){
  ggplot2::ggplot(user_mojo_history[user_mojo_history$Step==0,], aes(x=MoJo)) +
  ggplot2::geom_histogram(binwidth=10) +
  ggplot2::theme_bw() +
  ggplot2::labs(x="MoJo distance between start and finish", y="Count") +
  ggplot2::scale_x_continuous(limits=c(0,80))
}

(function(){
  ggplot2::ggsave(user.mojo.start.histogram(),
                  filename="inst/fig/observational-experience.pdf",
                  width=5,height=4)

  ggplot2::ggsave(user.experience.histogram(),
                  filename="inst/fig/rq1-hist-mojo.pdf",
                  width=5,height=4)

  write.table(gen.user.summary.table(), file="inst/tables/sparks.tex",quote=F,eol="\\\\\n", sep="\t&\t", row.names=F)
})()
