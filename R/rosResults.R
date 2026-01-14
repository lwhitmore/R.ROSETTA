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
          print(index_of_mainclass)

          p=as.numeric(gsub("%", "", logs[nrow(logs), "V2"]))
          TP <- as.numeric(logs[nrow(logs)-4,"V3"])
          FN <- as.numeric(logs[nrow(logs)-4,"V4"])
        } else { 
          print ("test")
          print(index_of_mainclass)
          p=as.numeric(gsub("%", "", logs[nrow(logs), "V3"]))
          TP <- as.numeric(logs[nrow(logs)-3,"V4"])
          FN <- as.numeric(logs[nrow(logs)-3,"V3"])

        }
          p <-  as.numeric(gsub("%", "", logs[nrow(logs), "V3"]))*.01
        precision <- c(precision,p)
        r <- as.numeric(logs[nrow(logs)-3,"V4"])/(as.numeric(logs[nrow(logs)-3,"V4"])+as.numeric(logs[nrow(logs)-3,"V3"]))
        recall <- c(recall, r)
        f <- 2 *((p*r)/(p+r))
        f1score <- c(f1score, f)
      }
      stats2 <- rbind(stats2, c("MEAN.precision", mean(precision)))
      stats2 <- rbind(stats2, c("MEAN.recall", mean(recall)))
      stats2 <- rbind(stats2, c("MEAN.f1score", mean(f1score)))


  ##MCC
  #cts<-statsTab[which(statsTab$V2=="|"),]
  #if(length(which(cts$V1=="Undefined"))!=0){
  #cts<-cts[-which(cts$V1=="Undefined"),]
  #}
  #noc<-unique(as.numeric(as.character(cts$V1)))

  #MCC=c()

#for(i in seq(1, dim(cts)[1], by = length(noc))){
#if(length(noc) == 2){
  
  #TP=as.numeric(as.character(cts$V3[i]))
  #FN=as.numeric(as.character(cts$V3[i+1]))
  #FP=as.numeric(as.character(cts$V4[i]))
  #TN=as.numeric(as.character(cts$V4[i+1]))
  #MCC<-c(MCC,((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
    
#}else{
  
  #MCC<-c(MCC,NA)
  ##in progress##
  #TP=cts$V3[i]
  #FN=cts$V3[i+1]
  #FP=cts$V4[i]
  #TN=cts$V4[i+1]
  
#}
  #statsMCC=data.frame("MCC.mean",as.factor(mean(MCC)))
  #colnames(statsMCC)<-c("Measure","Value")
#}
  
  #stats2=rbind(stats2,statsMCC)
  
  return(stats2)

}
