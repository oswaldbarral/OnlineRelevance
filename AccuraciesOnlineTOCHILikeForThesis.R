library(plotly)
library(gridExtra)
library(ggrepel)
library (pROC)

#Define relevant and irrelevant thresholds
REL_TH <<- 3
IRR_TH <<- 2
 




CertaintyToCfOut <- function(rel, cert) {
#This is the pyhton code. This function aims at reversing this
# # input: cfy_out from MATLAB (assuming - value means target (1) and + value means non-target (0)
# # output: [rel, unc]
# #   rel: relevance feedback in {0,1}
# #   unc: uncertainty value in (0,1]
# def computeRelevanceUncertainty( cfy_out ):
#   # Map classifier output to 1=targets and 0=non-targets
#   cfy_out = -cfy_out
#   rel_value = (math.copysign(1,cfy_out) + 1)/2
#   # use an increasing function on abs(cfy_out) to define the uncertainty. It should be in (0,1].
#   unc_value = math.sqrt(float(abs(cfy_out))/0.99) + 0.01;
#   unc_value = min(unc_value, 0.99)
#   return [rel_value,unc_value]
  
  cfy_out <- cert - 0.01
  cfy_out <- cfy_out * cfy_out
  cfy_out <- cfy_out * 0.99
  cfy_out[rel] <- -cfy_out[rel]
  #returns original cfy_out in the range [-1,1], negative = target, positive = non target.
  return(cfy_out)
  
}
acc <- function(a,rel_th,irr_th, type="acc") {

  #remove instance with Certainty == 1 (the one the participant provided exp.feedback for backend)
  a <- a[a$Certainty != 1, ]
  #remove instances with missing explicit feedback
  a <- a[!is.na(a$ExplicitFeedback),]
  #remove instances with "I don't know" explicit feedback
  a <- a[a$ExplicitFeedback != -1, ]
  #remove instances with ambiguous feedback
  a <- a[a$ExplicitFeedback <= irr_th | a$ExplicitFeedback >= rel_th,]
  #binarize explicit feedback (rel = True, irrel = False)
  a$ExplicitFeedback <- a$ExplicitFeedback >= rel_th
  
  ###For Implicit performance metric
  #binarize implicit feedback (1 = True, 0 = False)
  a$ImplicitFeedback  <- a$ImplicitFeedback ==1
  #compute implicit EEG feedback accuracy
  if (type == "auc") {
    #convert Implicit to original cf_out
    a$ImplicitFeedback <- CertaintyToCfOut(a$ImplicitFeedback,a$Certainty)
    #convert Explicit to factor
    a$ExplicitFeedback <- factor(a$ExplicitFeedback, levels=c("TRUE","FALSE"))
    accu <- auc(a$ExplicitFeedback,a$ImplicitFeedback,levels=c("TRUE","FALSE"),direction="<")
  }
  else if (type == "balancedAcc") {
    #convert to factor for confusion Matrix
    a$ExplicitFeedback <- factor(a$ExplicitFeedback, levels=c("TRUE","FALSE"))
    a$ImplicitFeedback <- factor(a$ImplicitFeedback, levels=c("TRUE","FALSE"))
    cm <- confusionMatrix(reference=a$ExplicitFeedback,data=a$ImplicitFeedback)
    accu <- cm$byClass[["Balanced Accuracy"]]
  }
  else accu <- sum(a$ExplicitFeedback == a$ImplicitFeedback) /nrow(a)
  
  ###For Inferred performance metric
  #remove instances with missing inferred feedback
  a <- a[!is.na(a$InferredRelevance),]
  #compute inferred feedback accuracy
  if (type == "auc") {
    accuInf <- auc(a$ExplicitFeedback,a$InferredRelevance,levels=c("TRUE","FALSE"),direction=">")
  }
  else {
    #binarize inferred feedback accuracy
    rel_zero_mean <- a$InferredRelevance - mean(a$InferredRelevance)
    a$InferredRelevance <- rel_zero_mean > 0
    if (type == "balancedAcc") {
      #convert to factor for confusion Matrix
      a$InferredRelevance <- factor(a$InferredRelevance, levels=c("TRUE","FALSE"))
      
      cm <- confusionMatrix(reference=a$ExplicitFeedback,data=a$InferredRelevance)
      accuInf <- cm$byClass[["Balanced Accuracy"]]
    }
    else accuInf <- sum(a$ExplicitFeedback == a$InferredRelevance) /nrow(a)
  }
  
  list(accu,accuInf)
}


##REject Participant

rejectPart <- function(N) {
  print (paste(N," - Rejected",sep=""))
  d <<- d[d$PID != N,]
  i <- which(parts == N)
  parts <<- parts[-i]
  cvErrors <<- cvErrors[-i]
  print(paste(length(parts)," participants left",sep=""))
}



#generates boxplots per accuracy type

boxplots <- function (yli){
  d1 <- d[d$Condition == 1,]
  d1 <- d1[,c(8:10)]
  nam <- c(1:3)
  s <- data.frame("Classification_Accuracy"=unlist(d1),"Type"=unlist(lapply(nam,rep,nrow(d1))))
  s$Type <- factor(s$Type, levels=c(1,2,3), labels=c("Calibration","Online Implicit","Intent Model"))

  return (ggplot(s, aes(x=Type, y=Classification_Accuracy, fill=Type)) + geom_boxplot() + guides(fill=FALSE) 
          + geom_hline(yintercept = 0.5,linetype="dashed") + labs(x = NULL, y = "Classification performance")  
          + scale_y_continuous (limits= yli, breaks = round(seq(yli[1],yli[2], by = 0.1),1) )
)
}


#generates dotplots per accuracy type
dotPlots <- function (yli){
  d1 <- d[d$Condition == 1,]
  d1 <- d1[,c(1,8:10)]
  
  #Add the mean values as additional row
  means <- unlist(lapply(d1[,c(2:4)],mean,na.rm=T))
  d1 <- rbind(d1,NA)
  d1$PID[nrow(d1)] <- "Mean"
  d1[nrow(d1),c(2:4)] <- means
  
  d2 <- data.frame("Accuracy" =c(d1$Acc_Calibration,d1$Acc_Implicit,d1$Acc_Inferred))
  d2$Type <- c(rep(1,length(d1$Acc_Calibration)),rep(2,length(d1$Acc_Implicit)),rep(3,length(d1$Acc_Inferred)))
  d2$PID <- rep(d1$PID,3)
  d2$Type <- factor(d2$Type, levels=c(1,2,3), labels=c("Calibration","Online Implicit","Intent Model"))
  
  
  p <- ggplot(d2[d2$PID != "Mean",], aes(x=Type, y=Accuracy, fill=Type, color = Type, group = PID)) 
  p <-  p + geom_point(size = 3)
  p <- p + labs(x = NULL,y =NULL)#+ labs(x = "Participant code", y = "Classification performance") 
  p <- p+ geom_line(color="grey",alpha=0.7)
  # p<- p+geom_boxplot(aes(x=Type, y=Accuracy, alpha = 0.01, group =Type) )
  p <- p + geom_hline(yintercept = 0.5,linetype="dashed")
  p <- p+geom_text_repel(data= d2[d2$PID != "Mean" & d2$Type == "Calibration",], aes(label=PID), nudge_x = -0.2,segment.alpha=0.7,color="black",segment.color ="grey")

  # p <- p + geom_text(data = d2[d2$PID != "Mean" & d2$Type == "Calibration",], aes(label=PID),  hjust = 1.5, color="black",check_overlap=FALSE)
  
  #add Mean info
  p <- p + geom_point(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),size = 20,alpha=0.5)
  p <- p + geom_line(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),size = 2,alpha=0.2,color = "black")
  
  p <- p+ guides(color=FALSE, fill =FALSE)
  p <- p + scale_y_continuous (limits= yli, breaks = round(seq(yli[1],yli[2], by = 0.1),1))
  
  return(p)
}



#generates dotplots per source type with superimposed boxplot
dotPlotsSuperImp <- function (yli,xla,yla){
  d1 <- d[d$Condition == 1,]
  d1 <- d1[,c(1,8:10)]
  
  #Add the mean values as additional row
  means <- unlist(lapply(d1[,c(2:4)],mean,na.rm=T))
  d1 <- rbind(d1,NA)
  d1$PID[nrow(d1)] <- "Mean"
  d1[nrow(d1),c(2:4)] <- means
  
  d2 <- data.frame("Accuracy" =c(d1$Acc_Calibration,d1$Acc_Implicit,d1$Acc_Inferred))
  d2$Type <- c(rep(1,length(d1$Acc_Calibration)),rep(2,length(d1$Acc_Implicit)),rep(3,length(d1$Acc_Inferred)))
  d2$PID <- rep(d1$PID,3)
  d2$Type <- factor(d2$Type, levels=c(1,2,3), labels=c("Calibration","Online - Neurophysiology","Online - Intent Model"))
  
  
  p <- ggplot(d2[d2$PID != "Mean",], aes(x=Type, y=Accuracy, fill=Type, color = Type, group = PID))
  
  #set the y_axis breaks
  p <- p + scale_y_continuous (limits=yli,breaks = round(seq(yli[1],yli[2], by = 0.1),1))

  #plot dots
  p <-  p + geom_point(size = 3)
  #add connecting line
  p <- p+ geom_line(color="grey",alpha=0.4)
  #add horizontal line at 0.5 (indicating random)
  p <- p + geom_hline(yintercept = 0.5,linetype=3,size=0.7,color="black")
  #overlay boxplot
  p <- p + geom_boxplot(data = d2[d2$PID != "Mean",],aes(x=Type, y=Accuracy, fill=Type, color = Type, group = Type),alpha=0.2,width=0.3)
  #add smal point for means
  p <- p + geom_point(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),size = 2, alpha = 0.7, color="black",shape=20)
  #add connecting line for means
  p <- p + geom_line(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),alpha=0.8,color="black",linetype=2)
  #add participant labels
  p <- p+geom_text_repel(data= d2[d2$PID != "Mean" & d2$Type == "Calibration",], aes(label=PID), nudge_x = -0.4,segment.alpha=0.7,segment.color ="grey",color="black")
  p <- p+geom_text_repel(data= d2[d2$PID != "Mean" & d2$Type == "Online - Intent Model",], aes(label=PID), nudge_x = 0.4,segment.alpha=0.7,segment.color ="grey",color="black")
  #remove legend
  p <- p+ guides(color=FALSE, fill =FALSE)
  #add plot labels
  p <- p + labs(x = xla, y = yla)
  #turn background in balck and white
  p <- p + theme_bw()
  #remove grid lines
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  #return plot
  return(p)
  # 
  # #########################
  # 
  # p <-  p + geom_point(size = 3)
  # p <- p + labs(x = NULL,y =NULL)#+ labs(x = "Participant code", y = "Classification performance") 
  # p <- p+ geom_line(color="grey",alpha=0.7)
  # # p<- p+geom_boxplot(aes(x=Type, y=Accuracy, alpha = 0.01, group =Type) )
  # p <- p + geom_hline(yintercept = 0.5,linetype="dashed")
  # p <- p+geom_text_repel(data= d2[d2$PID != "Mean" & d2$Type == "Calibration",], aes(label=PID), nudge_x = -0.2,segment.alpha=0.7,segment.color ="grey")
  # 
  # # p <- p + geom_text(data = d2[d2$PID != "Mean" & d2$Type == "Calibration",], aes(label=PID),  hjust = 1.5, color="black",check_overlap=FALSE)
  # 
  # # #add boxplot
  # p <- p + geom_boxplot(data = d2[d2$PID != "Mean",],aes(x=Type, y=Accuracy, fill=Type, color = Type, group = Type),alpha=0.2,width=0.3)
  # # 
  # 
  # 
  # # #add Mean info
  # # p <- p + geom_point(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),size = 20,alpha=0.5)
  # # p <- p + geom_line(data=d2[d2$PID == "Mean",], aes(x=Type,y=Accuracy),size = 2,alpha=0.2,color = "black")
  # # 
  # # p <- p+ guides(color=FALSE, fill =FALSE)
  # # p <- p + scale_y_continuous (limits= yli, breaks = round(seq(yli[1],yli[2], by = 0.1),1))
  # # 
  # return(p)
}


























##MAIN

DIR <- "/Users/barralme/Documents/MindSee/Online System/Online/Online analysis - Final Data/"


#Read calibration CV errors
cvErrors <<- read.csv(paste(DIR,"Calibration.csv",sep=""),stringsAsFactors = FALSE)
#sort by PID
cvErrors <<- cvErrors[order(cvErrors$PID),]
#Reject participant with not valid online data
cvErrors <<- cvErrors[cvErrors$PID != "P05",]
#get rid of unnecessary stuff
cvErrors <<- cvErrors$CVError
# 
# cvErrors <<- read.table(paste(DIR,"cvErrors.txt",sep=""))
# cvErrors <<- cvErrors$V1
# # cvErrors2 <<- c(0.2607159185,0.4066519487,0.3033596059,0.3773070822,0.3717404237,0.3713410908,
# #                0.4530739432,0.4462229686,0.4623351687,0.4516340784,0.4203425787,0.3264584148)
# 



d <<- read.csv(paste(DIR,"online-topic-with-condition.csv",sep=""),stringsAsFactors = FALSE)


parts <<- unique(d$PID)


#Reject participants with calibration not better than random
rejectPart("H34")
rejectPart("P01")
rejectPart("P06")


res <- lapply (parts,function(part) {
  fnames <- list.files(DIR,paste("Pilot_",part,"+",sep=""),full.names=TRUE)
  print(part)
  r <- lapply (fnames,function(fname) {
    accu <- NA
    accuInf <- NA
    a <- read.csv (fname,stringsAsFactors = FALSE)
    if(length(a$Condition) == 0) {
      print("NoDataForFile")
    }
    else if(a$Condition[1] == 3) {
      # print("ExplicitOnly - Skip")
    }
    else {
      # d <- acc(a,REL_TH,IRR_TH)
      # d <- acc(a,REL_TH,IRR_TH,"balancedAcc")
      d <- acc(a,REL_TH,IRR_TH, "auc")
      accu <- d[[1]]
      accuInf <- d[[2]]
    }
    list(a, data.frame("Acc_Calibration" = 1-cvErrors[which(parts == part)] ,"Acc_Implicit" = accu, "Acc_Inferred" = accuInf))
  })
  res <- rbind(r[[1]][[2]],r[[2]][[2]])
  res$Acc_Overall <- acc(rbind(r[[1]][[1]],r[[2]][[1]]),REL_TH,IRR_TH)[[1]]
  res
})

d <<- cbind(d,do.call(rbind,res))

yli <- c(0.26,0.8)
xlab <- "Within participant prediction"
ylab <- "Area Under the ROC Curve"

#New names
d$PID <- gsub("H28", "P08", d$PID)
d$PID <- gsub("H29", "P09", d$PID)
d$PID <- gsub("H30", "P10", d$PID)
d$PID <- gsub("H31", "P11", d$PID)
d$PID <- gsub("H32", "P12", d$PID)
d$PID <- gsub("H33", "P13", d$PID)
d$PID <- gsub("H34", "P14", d$PID)
d$PID <- gsub("H35", "P15", d$PID)
d$PID <- gsub("H36", "P16", d$PID)



# p1 <- boxplots(yli)
# #turn background in balck and white
# p1 <- p1 + theme_bw()
# #remove grid lines
# p1 <- p1 + theme( panel.grid.minor = element_blank())

# p2 <- dotPlots(yli)
# #turn background in balck and white
# p2 <- p2 + theme_bw()
# #remove grid lines
# p2 <- p2 + theme( panel.grid.minor = element_blank())
# 
# grid.arrange(p1, p2, ncol=2, widths = 1:2)

p <- dotPlotsSuperImp (yli,NULL,ylab)
print(p)



# ##generate barplots of d, including the mean
# barplots <- function() {
#   #Add the mean values as additional row
#   means <- unlist(lapply(d[,c(8:11)],mean,na.rm=T))
#   d <<- rbind(d,NA)
#   d$PID[nrow(d)] <- "_Mean_"
#   d[nrow(d),c(2:ncol(d))] <- c(1,rep(NA,5),means)
#   
#   d1 <- d[d$Condition == 1,]
#   d3 <- d[d$Condition == 3,]
#   
#   p <- plot_ly(
#     x = d1$PID,
#     y = d1$Acc_Calibration,
#     name = "Calibration",
#     type = "bar")
#   print(p)
#   
#   # p1 <- add_trace(p = last_plot(),
#   #                 x = d1$PID,
#   #                 y = d1$Acc_Overall,
#   #                 name = "Implicit_1+3",
#   #                 type = "bar")
#   # print(p1)
#   
#   p3 <- add_trace(p = last_plot(),
#                   x = d1$PID,
#                   y = d1$Acc_Implicit,
#                   name = "Implicit",
#                   type = "bar")
#   print(p3)
#   
#   
#   p2 <- add_trace(p = last_plot(),
#                   x = d1$PID,
#                   y = d1$Acc_Inferred,
#                   name = "Inferred",
#                   type = "bar")
#   print(p2)
#   
#   # p4 <- add_trace(p = last_plot(),
#   #   x = d3$PID,
#   #   y = d3$Acc_Implicit,
#   #   name = "Impplicit_3",
#   #   type = "bar")
#   # print(p4)
#   
#   
#   # 
#   # rel_zero_mean = a$InferredRelevance - mean(a$InferredRelevance)
#   # rel_bin  = rel_zero_mean > 0
#   # rel_1_0  = rel_zero_mean + abs(min(rel_zero_mean))
#   # rel_1_0  = rel_1_0/max(rel_1_0)
#   # 
#   # 
# }



# ##Renames participant ID
# changePartName <- function (namOld,namNew) {
#   d$PID[d$PID == namOld] <<- namNew
#   
# }
# changePartName("H28","P08")
# changePartName("H29","P09")
# changePartName("H30","P10")
# changePartName("H31","P11")
# changePartName("H32","P12")
# changePartName("H33","P13")
# changePartName("H34","P14")
# changePartName("H35","P15")
# changePartName("H36","P16")
# #Reject participants with calibration not better than random
# rejectPart("P14")
# rejectPart("P01")
# rejectPart("P06")





