library(gridExtra)
library(reshape)
library(ggplot2)


accPlot <- function(calib) {
  
  #New names
  calib$PID <- gsub("H28", "P08", calib$PID)
  calib$PID <- gsub("H29", "P09", calib$PID)
  calib$PID <- gsub("H30", "P10", calib$PID)
  calib$PID <- gsub("H31", "P11", calib$PID)
  calib$PID <- gsub("H32", "P12", calib$PID)
  calib$PID <- gsub("H33", "P13", calib$PID)
  calib$PID <- gsub("H34", "P14", calib$PID)
  calib$PID <- gsub("H35", "P15", calib$PID)
  calib$PID <- gsub("H36", "P16", calib$PID)

  calib$Sig <- rep(" ",nrow(calib))
  calib$Sig[calib$pValue < 0.05] <- "*"
  calib$Sig[calib$pValue < 0.001] <- "**"
  #avoid that gplot orders by PID
  calib$PID <- factor(calib$PID, levels = calib$PID)
  
  p <- ggplot(calib,aes(x=PID,y=CVError))
  p <- p+ geom_point(size=2)
  p <- p + geom_hline(yintercept = 0.5,linetype="dashed")
  p <- p + geom_hline(yintercept = mean(calib$CVError))
  p <- p + geom_text( aes(label=Sig),vjust = -0.25, hjust = 1, color="black",check_overlap=TRUE)
  p <- p + labs(x = "Participant code", y = "Area Under the ROC Curve") 
  p <- p + theme_bw()
  #remove grid lines
  p <- p + theme( panel.grid.minor = element_blank())
  return(p)
  
}

boxPl <- function(calib) {
  #format for ploting 
  calib <- melt(calib,id=c("PID","pValue"))


  #avoid that gplot orders by PID
  p <- ggplot(calib,aes(x=variable,y=value,fill=variable))
  p <- p + guides(fill=FALSE)
  p <- p + geom_boxplot()
  return (p)
}


##MAIN

DIR <- "/Users/barralme/Documents/MindSee/Online System/Online/Online analysis - Final Data/"

#Read calibration CV errors
calib <- read.csv(paste(DIR,"Calibration.csv",sep=""),stringsAsFactors = FALSE)

#sort by smaller CVerror
calib <- calib[order(calib$CVError),]


#Convert errors to Accuracy
calib$CVError <- 1 - calib$CVError

p2 <- accPlot(calib)

print(p2)

# 
# p1 <- boxPl(calib)
# grid.arrange(p1, p2, ncol=2, widths = 1:2)



