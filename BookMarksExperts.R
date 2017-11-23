require(reshape2)

DIR <- "/Users/barralme/Documents/MindSee/Online System/Online/Online analysis - Final Data/"

a <- read.csv(paste(DIR,"rated.csv",sep=""),stringsAsFactors = FALSE)


#boxplot
# a <- a[,3:6]
# a1 <- melt(a, id="Condition")
# 
# p<-ggplot(a1, aes(x=variable,y=value,fill=Condition))
# p <- p+ geom_boxplot()
# print(p)





#bar plots
##split by topic
a <- a[,2:6]
a1 <- melt(a, id=c("topic","Condition"), measure=c("Novelty","Obviousness","Relevance"))
p<-ggplot(a1, aes(x=topic,y=value,fill=Condition))
p <- p + theme_bw()
#remove grid lines
p <- p + theme( panel.grid.minor = element_blank())
p <- p + geom_bar(stat="identity",position="dodge")
# p <- p + geom_boxplot()
p <- p + facet_grid(. ~ variable)
##
#
# ##regardless of topic
# a <- a[,3:6]
# a1 <- melt(a, id=c("Condition"), measure=c("Novelty","Obviousness","Relevance"))
# p<-ggplot(a1, aes(x=variable,y=value,fill=Condition))
# p <- p + theme_bw()
# #remove grid lines
# p <- p + theme( panel.grid.minor = element_blank())
# p <- p + geom_bar(stat="identity",position="dodge")
# ##

print(p)