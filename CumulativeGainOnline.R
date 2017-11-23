library(plotly)
library(gridExtra)
library(ggrepel)


#Define relevant and irrelevant thresholds
REL_TH <- 3
IRR_TH <- 2


##REject Participant

rejectPart <- function(N) {
  d <<- d[d$PID != N,]
  i <- which(parts == N)
  parts <<- parts[-i]
  cvErrors <<- cvErrors[-i]
}





#plot the number of iterations per participant (black = cond1, green = cond3)
plotNumScreens <- function(){
  plot(d$Screen,xaxt="n",col = d$Condition)
  axis(1,at=c(1:nrow(d)),labels=d$PID)
}


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

# plotNumScreens()

#For now look at the parts that had 10 iterations
# rejectPart("H28")
# rejectPart("H30")
# rejectPart("P04")




#merge all participants dataset
data <- c()
for (part in parts) {
  fnames <- list.files(DIR,paste("Pilot_",part,"+",sep=""),full.names=TRUE)
  for( fname in fnames) {
    d_ <- read.csv (fname,stringsAsFactors = FALSE)
    d_$PID = rep(part,nrow(d_))
    #add information about amount of relevant keywords
    d_$RelevantKeywordsBIN <- unlist(lapply(split(d_$ExplicitFeedback,d_$Iteration),function(x) rep(sum(x >= REL_TH, na.rm=T)/length(x),length(x))))
    d_$RelevantKeywords <- unlist(lapply(split(d_$ExplicitFeedback,d_$Iteration),function(x) rep(mean(x, na.rm=T),length(x))))
    
    #add information about amout of time per iteration
    d_$Time_perIt <- unlist(lapply(unique(d_$Iteration),function(x) {
      aux <- d[d$PID == part & d$Condition == d_$Condition[1],]$TimeSpent
      aux <- strsplit(aux,"-")[[1]]
      rep(as.numeric(aux[x]),sum(d_$Iteration == x))
    }))
    
    #add information about amout of bookmarked articles per iteration
    d_$Bookmarks_perIt <- unlist(lapply(unique(d_$Iteration),function(x) {
      aux <- d[d$PID == part & d$Condition == d_$Condition[1],]$BookmarkedArticles
      aux <- strsplit(aux,"-")[[1]]
      #compute the real amount of bookmarks per iteration (from the cumulative)
      aux <- diff(c(0,as.numeric(aux)))
      rep(aux[x],sum(d_$Iteration == x))
    }))
    
    data <- rbind(data,d_)
  }
}


# final <<- data

#generate grand average and std error of bookmarked articles per condition
res <- lapply(split(data,data$Condition), function (d){
  r <- lapply(split(d,d$Iteration),function(d1){
    aux <- unlist(lapply(split(d1$Bookmarks_perIt,d1$PID),function(x) x[1]))
    data.frame("Ga" = mean(aux), "Se" = 0.95*(sd(aux)/sqrt(length(aux))) )
  })
  r1 <- do.call(rbind,r)
  r1$Iteration <- unlist(lapply(c(1:10),rep,length(r[[1]]$Ga)))
  return(r1)
})
data_BM <- do.call(rbind,res)
data_BM$Condition <- factor(unlist(lapply(c("Full system","Baseline"),rep,length(res[[1]]$Ga))))



# data_BM <- data.frame("Iteration" = rep(1:10,2),"Condition"=c(rep("Full System",10),rep("Baseline",10)),"Ga" = unlist(ga_dataBM))
p <- ggplot(data_BM, aes(x=Iteration, y=Ga, color = Condition)) 
p <- p + theme_bw()
#remove grid lines
p <- p + theme( panel.grid.minor = element_blank())
p <- p + scale_x_continuous(breaks=c(1:10))
p <- p +  labs(x = "Iteration", y = NULL)
p <- p + geom_line()
p <- p + geom_point(aes( shape = Condition),size=3)
p <- p + guides(color = guide_legend(title = NULL), shape=guide_legend(NULL))
p <- p +  theme(legend.position = c(0.8, 0.8))
p <- p + geom_errorbar(aes(ymax = Ga + Se, ymin=Ga - Se), width=0.25,position=position_dodge(0.1))
p <- p + scale_y_continuous(limits = c(0.6, 3.2))


# #Plot overall Bookmarked/iteration
d$Condition <- factor(d$Condition, levels=c(3,1), labels=c("Baseline","Full system"))
d$TotalBookmarks <-  unlist(lapply(d$BookmarkedArticles,function(x){
  aux <- strsplit(x,"-")[[1]]
  as.numeric(aux[length(aux)])/length(aux)
}))

# #plot only boxplots
pl <- ggplot(d, aes(x=Condition, y=TotalBookmarks, fill=Condition)) + geom_boxplot() + guides(fill=FALSE)
pl <- pl + labs(x = " " , y = "Amount of bookmarked articles")
pl <- pl + theme_bw()
#remove grid lines
pl <- pl + theme( panel.grid.minor = element_blank())
pl <- pl + scale_y_continuous(limits = c(0.6, 3.2))
grid.arrange(pl, p, ncol=2, widths = 1:2)
# ##

#plot individual + boxplots
# pl <- ggplot(d, aes(x=Condition, y=TotalBookmarks, fill=Condition, color = Condition, group = PID)) + guides(fill=FALSE,color=FALSE)
# pl <- pl+ theme_bw()
# pl <-  pl + geom_point(size = 3)
# pl <- pl + labs(x = NULL,y ="Amount of bookmarked articles")
# pl <- pl+ geom_line(color="grey",alpha=0.7)
# pl <- pl+geom_text_repel(data= d[d$Condition == "Baseline",], aes(label=PID), nudge_x = -0.2,segment.alpha=0.7,segment.color ="grey",color="black")
# pl <- pl+geom_boxplot(aes(x=Condition, y=TotalBookmarks, group=Condition),alpha=0.2)
# print(pl)
##


# 


#####Plot distribution relevant Keywords /condition

# #####
# #Option 1 --> disregards iterations
# daux <-unlist(lapply(split(data,data$Condition),function(z){
#     unlist(lapply(split(z,z$PID),function(x){
#   sum(x$ExplicitFeedback > REL_TH,na.rm=T)
# }))}))
# daux<-data.frame("Rel"=daux)
# daux$Condition <- factor(c(rep("Baseline",12),rep("Full system",12)))
# 
# p1 <- ggplot(daux,aes(x=Condition,y=Rel,fill=Condition))
# p1<- p1 + geom_boxplot()
# print(p1)
# ######

#Option2 --> takes into account iterations
daux <- data[,c("PID","Iteration","Condition","RelevantKeywordsBIN")]
#get rid of repetitions
daux <- aggregate(RelevantKeywordsBIN ~ Iteration + Condition + PID, data = daux, function(x){x[1]})
#average relevant keywords per iterations (as some parts did not do full 10 iterations for some condition)
daux <- aggregate(RelevantKeywordsBIN ~ Condition + PID, data = daux, mean)
daux$Condition <- factor(daux$Condition, levels=c(3,1), labels=c("Baseline","Full system"))

# #plot only boxplots
p1 <- ggplot(daux, aes(x=Condition, y=RelevantKeywordsBIN, fill=Condition)) + geom_boxplot() + guides(fill=FALSE)
p1 <- p1 + labs(x = " " , y = "Amount of relevant keywords")
p1 <- p1 + theme_bw()
#remove grid lines
p1 <- p1 + theme( panel.grid.minor = element_blank())
print(p1)
# ##


# #plot individual + boxplots
# p1 <- ggplot(daux, aes(x=Condition, y=RelevantKeywordsBIN, fill=Condition, color = Condition, group = PID)) + guides(fill=FALSE,color=FALSE)
# p1 <- p1+ theme_bw()
# p1 <-  p1 + geom_point(size = 3)
# p1 <- p1 + labs(x = NULL,y = "Amount of relevant keywords")
# p1 <- p1+ geom_line(color="grey",alpha=0.7)
# p1 <- p1+geom_text_repel(data= daux[daux$Condition == "Baseline",], aes(label=PID), nudge_x = -0.2,segment.alpha=0.7,segment.color ="grey",color="black")
# p1 <- p1+geom_boxplot(aes(x=Condition, y=RelevantKeywordsBIN, group=Condition),alpha=0.2)
# print(p1)
##

grid.arrange(pl, p1, ncol=2, widths = c(1,1))

