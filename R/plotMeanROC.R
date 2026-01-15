plotMeanROC <- function(out, col="orangered3", backCol="snow2"){
  
  ROCstats <- out$ROCstats
  
  if(is.null(ROCstats)){
  ROCstats <- out$ROC.stats  
  }
  df <- unstack(ROCstats, form = OneMinusSpecificity ~ CVNumber)
  checklengths <- c()
  for (n in names(df)) {
    checklengths <- c(checklengths, length(df[[n]]))
  }
  checklengths <- unique(checklengths)
  if (length(checklengths)==1) {
    OMSpec <- rowMeans(unstack(ROCstats, form = OneMinusSpecificity ~ CVNumber))
    Sens <- rowMeans(unstack(ROCstats, form = Sensitivity ~ CVNumber))
  } else { 
    message("STATUS: CV lengths not the same")
    dfoms <- unstack(ROCstats1, form = OneMinusSpecificity ~ CVNumber)
    df <- unstack(ROCstats1, form = Sensitivity ~ CVNumber)
    for (n in names(df)) {
      df[[n]] <- df[[n]][1:min(checklengths)]  
      dfoms[[n]] <- dfoms[[n]][1:min(checklengths)]
    }
    dfoms <- as.data.frame(dfoms)
    df <- as.data.frame(df)
    OMSpec1 <- rowMeans(dfoms)
    Sens1 <- rowMeans(df)
    OMSpec1 <- OMSpec1[order(OMSpec1)]
    Sens1 <- Sens1[order(Sens1)]
  }
  ## plotting with new colours
  plot(OMSpec, Sens, type = "l", lwd=3, col=col, xlab="1 - specificity (FPR)", ylab="sensitivity (TPR)", axes=F, cex.lab=1.5)
  polygon(c(1,OMSpec), c(0,Sens), lwd=0.01, col=backCol)
  text(0.75, 0.1, col=col, paste0("mean AUC = ",round(out$quality$ROC.AUC.MEAN, digits = 2)),cex = 1.5)
  axis(side=1, at=seq(0,1,0.1))
  axis(side=2, at=seq(0,1,0.1))
  
}
