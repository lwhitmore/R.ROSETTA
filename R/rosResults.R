rosResults<-function(path, path_logs, roc, index_of_mainclass){
  
  if(.Platform$OS.type=="unix")
  {
    statsTab=read.table(paste0(path,"/logMain.txt"),fill=T)}else{
    statsTab=read.table(paste0(path,"\\logMain.txt"),fill=T)
    }
  
  if(roc)
  {
    stats=statsTab[((dim(statsTab)[1]-16):dim(statsTab)[1]),]
  }else{
    stats=statsTab[((dim(statsTab)[1]-4):dim(statsTab)[1]),]
  }
  
  stats2<-as.data.frame(as.matrix(stats)[,c(1,3)])
  colnames(stats2)<-c("Measure","Value")

  txt_files_ls <- list.files(path=path_logs, pattern="*.txt", full.names = T) 
  precision <- c()
  recall <- c()
  f1score <- c()
  for(k in 1:length(txt_files_ls)){
        logs=read.table(txt_files_ls[k], fill=T)
        if (index_of_mainclass==0) {
          p=as.numeric(gsub("%", "", logs[nrow(logs), "V2"]))*.01
          TP <- as.numeric(logs[nrow(logs)-4,"V3"])
          FN <- as.numeric(logs[nrow(logs)-4,"V4"])
        } else { 
          p=as.numeric(gsub("%", "", logs[nrow(logs), "V3"]))*.01
          TP <- as.numeric(logs[nrow(logs)-3,"V4"])
          FN <- as.numeric(logs[nrow(logs)-3,"V3"])

        }
        precision <- c(precision,p)
        r <- TP/(TP+FN)
        if (!is.na(r)) {
          recall <- c(recall, r)
          f <- 2 *((p*r)/(p+r))
          f1score <- c(f1score, f)
        }
      }
      stats2 <- rbind(stats2, c("MEAN.precision", mean(precision)))
      stats2 <- rbind(stats2, c("MEAN.recall", mean(recall)))
      stats2 <- rbind(stats2, c("MEAN.f1score", mean(f1score)))
  
  return(stats2)

}
