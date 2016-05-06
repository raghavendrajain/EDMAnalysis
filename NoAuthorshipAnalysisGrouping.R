# 1. moving (M)
# 2. enlarging to maximum size (E)
# 3. resizing to medium size (N)
# 4. shrinking (S)
# 5. Rotating (M)
# 6. making unions with other data slips (U)
# 7. add data slip to a group (G) and 
# 8. remove a data slip from a group (R)           

### Note: In this analysis, I am making the groups, so that vizualization can be made properly. 

library("TraMineR")
no_colHigh <- max(count.fields("tranHigh.finalDB", sep = ",")) # I have not removed the duplicate consecutifve entteries. 
dataHigh <- read.table("tranHigh.finalDB",sep=",",fill=TRUE, col.names=1:no_colHigh)


high_task1 <- subset(dataHigh, X1 =='A') 
high_task2 <- subset(dataHigh, X1 =='W')
high_task3 <- subset(dataHigh, X1 =='O')


# for the low performers


no_colLow <- max(count.fields("tranLowA.finalDB", sep = ",")) 
dataLow <- read.table("tranLowA.finalDB",sep=",",fill=TRUE, col.names=1:no_colLow)


low_task1 <- subset(dataLow, X1 =='A')
low_task2 <- subset(dataLow, X1 =='W')
low_task3 <- subset(dataLow, X1 =='O')


#function to replace blanks with missing
#apply that function
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}


### Making all high and low together

high_task1 <- data.frame(sapply(high_task1[,3:no_colHigh],  blank2na))
high_task2 <- data.frame(sapply(high_task2[,3:no_colHigh],  blank2na))
high_task3 <- data.frame(sapply(high_task3[,3:no_colHigh],  blank2na))

low_task1 <- data.frame(sapply(low_task1[,3:no_colLow],  blank2na))
low_task2 <- data.frame(sapply(low_task2[,3:no_colLow],  blank2na))
low_task3 <- data.frame(sapply(low_task3[,3:no_colLow],  blank2na))

dataSeq.scode <- c("Enlarge", "Normal", "Union", "Add To Group", "Remove from Group", "Move", "Shrink")
dataSeq.lab <- c("E", "N", "U", "G", "R", "M", "S")
task.lab = c("Enlarge", "Add to Group", "Move", "Normal Size", "Remove from Group", "Shrink", "Union")

high_task1.seq <- seqdef(high_task1, xtstep = 6, labels = task.lab)
high_task2.seq <- seqdef(high_task2, xtstep = 6, labels = task.lab)
high_task3.seq <- seqdef(high_task3, xtstep = 6, labels = task.lab)

low_task1.seq <- seqdef(low_task1, xtstep = 6, labels = task.lab)
low_task2.seq <- seqdef(low_task2, xtstep = 6, labels = task.lab)
low_task3.seq <- seqdef(low_task3, xtstep = 6, labels = task.lab)

# Getting all the data together 
allHigh.seq <- rbind(high_task1.seq, high_task2.seq, high_task3.seq)
allLow.seq  <- rbind(low_task1.seq, low_task2.seq, low_task3.seq )


#Setting the parameters 

par(mar=c(3.8, 4.5, 5.0, 2.5), oma = c(1.5,1.2,4.5,1), xpd=TRUE)

# Plotting the mean-time spent in the states 

# Chart one by one for all the tasks together

par(mfrow = c(1, 2))
seqmtplot(allHigh.seq, ylim = c(0, 50), title = "High Achivers Group", withlegend = FALSE)
seqmtplot(allLow.seq, ylim = c(0, 50), title = "Low Achivers Group", withlegend = FALSE)
title(main = "Mean Time Spent in All Tasks", cex.main = 2, outer = TRUE)



# Mean time speent in Task 1

par(mfrow = c(1, 2))
seqmtplot(high_task1.seq, ylim = c(0, 50), title = "High Achivers Group", withlegend = FALSE )
seqmtplot(low_task1.seq, ylim = c(0, 50), title = "Low Achivers Group", withlegend = FALSE)
title(main = "Mean Time Spent in Task 1", cex.main = 2, outer = TRUE)

# Mean time speent in Task 2

par(mfrow = c(1, 2))
seqmtplot(high_task2.seq, ylim = c(0, 50), title = "High Achivers Group", withlegend = FALSE )
seqmtplot(low_task2.seq, ylim = c(0, 50), title = "Low Achivers Group", withlegend = FALSE)
title(main = "Mean Time Spent in Task 2", cex.main = 2, outer = TRUE)

# Mean time speent in Task 3

par(mfrow = c(1, 2))
seqmtplot(high_task3.seq, ylim = c(0, 50), title = "High Achivers Group", withlegend = FALSE )
seqmtplot(low_task3.seq, ylim = c(0, 50), title = "Low Achivers Group", withlegend = FALSE)
title(main = "Mean Time Spent in Task 3", cex.main = 2, outer = TRUE)

# Plotting sequence indexes 

#For all the tasks together 
par(mfrow = c(1, 2))
seqiplot(allHigh.seq, border = NA,  title = "High Achivers Group", withlegend = FALSE )
seqiplot(allLow.seq, border = NA, title = "Low Achivers Group", withlegend = FALSE )
title(main = "Sequence Index Plots for all Tasks", cex.main = 2, outer = TRUE)


# for Task 1

par(mfrow = c(1, 2))
seqiplot(high_task1.seq, title = "High Achivers Group", withlegend = FALSE )
seqiplot(low_task1.seq, title = "Low Achivers Group", withlegend = FALSE)
title(main = "Sequence Index Plots for Task 1", cex.main = 2, outer = TRUE)

# Task 2

par(mfrow = c(1, 2))
seqiplot(high_task2.seq, title = "High Achivers Group", withlegend = FALSE )
seqiplot(low_task2.seq, title = "Low Achivers Group", withlegend = FALSE)
title(main = "Sequence Index Plots for Task 2", cex.main = 2, outer = TRUE)


#Task 3
plot.new()
par(mfrow = c(1, 2))
seqiplot(high_task3.seq, withlegend = FALSE, title = "High Achivers Group" )
seqiplot(low_task3.seq, withlegend = FALSE, title = "Low Achivers Group")
title(main = "Sequence Index Plots for Task 3", cex.main = 2, outer = TRUE)

# Sequence Frequency Plots--> Weighted Frequencies

# For all the tasks 

par(mfrow = c(1, 2))
seqfplot(allHigh.seq, border = NA, withlegend = FALSE, title = "High Achievers Group")
seqfplot(allLow.seq, border = NA, withlegend = FALSE, title = "Low Achievers Group")
title(main = "Sequence Frequencies for All Tasks", cex.main = 2, outer = TRUE)

#For Task 1

plot.new()
par(mfrow = c(1, 2))
# Add extra space to right of plot area; change clipping to figure
seqfplot(high_task1.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqfplot(low_task1.seq, withlegend = FALSE, border = NA, title = "Low Achivers Group")
title(main = "Sequence Frequencies for Task 1", cex.main = 2, outer = TRUE)


#For Task 2
par(mfrow = c(1, 2))
seqfplot(high_task2.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqfplot(low_task2.seq, withlegend = FALSE, border = NA, title = "Low Achievers Group")

title(main = "Sequence Frequencies for Task 2", cex.main = 2, outer = TRUE)

#For Task 3

par(mfrow = c(1, 2))
seqfplot(high_task3.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqfplot(low_task3.seq, withlegend = FALSE, border = NA, title = "Low Achievers Group")

title(main = "Sequence Frequencies for Task 3", cex.main = 2, outer = TRUE)

#the state distribution by time points 

#For all Tasks 
par(mfrow = c(1, 2))
seqdplot(allHigh.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqdplot(allLow.seq, withlegend = FALSE, border = NA, title = "Low Achievers Group")
title(main = "State distribution plot for All Tasks", cex.main = 2, outer = TRUE)


#For Task 1
par(mfrow = c(1, 2))
seqdplot(high_task1.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqdplot(low_task1.seq, withlegend = FALSE, border = NA, title = "Low Achievers Group")
title(main = "State distribution plot for Task 1", cex.main = 2, outer = TRUE)

#For Task 2
par(mfrow = c(1, 2))
seqdplot(high_task2.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqdplot(low_task2.seq, withlegend = FALSE, border = NA, title =  "Low Achievers Group")
title(main = "State distribution plot for Task 2", cex.main = 2, outer = TRUE)

#For Task 3
par(mfrow = c(1, 2))
seqdplot(high_task3.seq, withlegend = FALSE, border = NA, title = "High Achievers Group")
seqdplot(low_task3.seq, withlegend = FALSE, border = NA, title =  "Low Achievers Group")
title(main = "State distribution plot for Task 3", cex.main = 2, outer = TRUE)

## Plotting the sequences frequency,
## the states distribution
## and the legend

# Sequence Representations for all the the tasks for High Achievers Group 
par(mfrow=c(2,2))
seqiplot(allHigh.seq, tlim=0, withlegend=FALSE, border=NA, space=0)
seqfplot(allHigh.seq, pbarw=TRUE, withlegend=FALSE)
seqdplot(allHigh.seq, withlegend=FALSE)
seqlegend(allHigh.seq)
title(main = "Sequence representations for High Achievers Group", cex.main = 2, outer = TRUE)


# Sequence Representations for all the the tasks for Low Achievers Group 
par(mfrow=c(2,2))

seqiplot(allLow.seq, tlim=0, withlegend=FALSE, border=NA, space=0, title = "Index plot (first 10 sequences)")
seqfplot(allLow.seq, pbarw=TRUE, withlegend=FALSE, title = "Sequence frequency plot")
seqdplot(allLow.seq, withlegend=FALSE, title = "State distribution plot")
seqlegend(allLow.seq)
title(main = "Sequence representations for Low Achievers Group", cex.main = 2, outer = TRUE)

# Traversal Entropy for all the tasks 

par(mfrow=c(1,2))
seqHtplot(allHigh.seq, title = "High Achievers Group")
seqHtplot(allLow.seq, title = "Low Achievers Group")
title(main = "Traversal Entropy Index", cex.main = 2, outer = TRUE)

# Within sequence entropies i.e. longitudinal entropy

# Result: The high performers group has more within sequence entropy compared to the lowers ones

# for all the tasks 

entropiesHigh <- seqient(allHigh.seq)
entropiesLow <- seqient(allLow.seq)

totalEntropy <- cbind(entropiesHigh, entropiesLow)
totalEntropy
par(mfrow=c(1,1))
plot.new()
colnames(totalEntropy) <- c("High", "Low")
matplot(totalEntropy[,c("High", "Low" )], pch = 15:16, cex = 1.8, col = c(2:3), type = "o",  
        xlab = "Number of Clients", ylab = "kbit/s", xaxt="n", yaxt="n", lwd = 3, cex.lab=2.0)



# we are going to see the longitudinal entropies for all the tasks together
plot.new()
par (mfrow = c(2,2))

# For Task 1

entropiesHigh <- seqient(high_task1.seq)
entropiesLow <- seqient(low_task1.seq)
totalEntropy <- cbind(entropiesHigh, entropiesLow)
totalEntropy
colnames(totalEntropy) <- c("High", "Low")

matplot(totalEntropy[,c("High", "Low" )], pch = 15:16, cex = 1.0, col = c(2:3), type = "o",  
        xlab = "Number of data slips", ylab = "Entropy", lwd = 2, cex.lab=1.5, main = "Task 1" )

# For Task 2
entropiesHigh <- seqient(high_task2.seq)
entropiesLow <- seqient(low_task2.seq)
totalEntropy <- cbind(entropiesHigh, entropiesLow)
totalEntropy
colnames(totalEntropy) <- c("High", "Low")
matplot(totalEntropy[,c("High", "Low" )], pch = 15:16, cex = 1.0, col = c(2:3), type = "o",  
        xlab = "Number of data slips", ylab = "Entropy", lwd = 2, cex.lab=1.5, main = "Task 2" )

# For Task 3
entropiesHigh <- seqient(high_task3.seq)
entropiesLow <- seqient(low_task3.seq)
totalEntropy <- cbind(entropiesHigh, entropiesLow)
totalEntropy
colnames(totalEntropy) <- c("High", "Low")

matplot(totalEntropy[,c("High", "Low" )], pch = 15:16, cex = 1.0, col = c(2:3), type = "o",  
        xlab = "Number of data slips", ylab = "Entropy", lwd = 2, cex.lab=1.5, main = "Task 3" )


#Sequence with highest entropy

high.ient<-seqient(allHigh.seq)
index <- which(high.ient == max(high.ient))
index
allHigh.seq[index, ]

# The same result can be obtained more simply 
#but also more mysteriously with a single command.
allHigh.seq[high.ient == max(high.ient), ]

# The distribution of the within sequence entropies

hist(high.ient, main = NULL, col = "cyan", xlab = "Entropy")



# Problem with legends..

plot(0,type='n',axes=FALSE,ann=FALSE)

legend("center", legend=c("High", "Low") ,pch = 15:16, col=c(2:3),
       title = "Achievers Group",title.col ="black", cex=1.5, pt.cex = 2.0, horiz = FALSE)

title(main = "Longitudinal Entropy of Individual DataSlips", cex.main = 2, outer = TRUE)


#### Calculating optimal matching Distances for all the sequences

# for all the high groups sequences
allHigh.seq.om <- seqdist(allHigh.seq, method = "OM", indel = 1, sm = "TRATE")

high_task1.seq.om <- seqdist(high_task1.seq, method = "OM", indel = 1, sm = "TRATE")
high_task2.seq.om <- seqdist(high_task2.seq, method = "OM", indel = 1, sm = "TRATE")
high_task3.seq.om <- seqdist(high_task3.seq, method = "OM", indel = 1, sm = "TRATE")

# for all the low groups sequences

allLow.seq.om <- seqdist(allLow.seq, method = "OM", indel = 1, sm = "TRATE")

low_task1.seq.om <- seqdist(low_task1.seq, method = "OM", indel = 1, sm = "TRATE")
low_task2.seq.om <- seqdist(low_task2.seq, method = "OM", indel = 1, sm = "TRATE")
low_task3.seq.om <- seqdist(low_task3.seq, method = "OM", indel = 1, sm = "TRATE")

library("cluster")

# creating different clusters
plot.new()
par(mfrow = c(1,1))
high_task1.seq.clusterward <- agnes(high_task1.seq.om, diss = TRUE, method = "ward")
plot(high_task1.seq.clusterward, which.plots = 2)

cluster3 <- cutree(high_task1.seq.clusterward, k = 3)
cluster3 <- factor(cluster3, labels = c("Type 1", "Type 2", "Type 3"))
table(cluster3)
seqfplot(high_task1.seq, group = cluster3, pbarw = T)


medoid <- seqrep(high_task1.seq, dist.matrix = high_task1.seq.om, criterion = "dist", nrep = 1)
print(medoid, format = "SPS")


# Representative Sequecnces for All Tasks
plot.new()
par(mfrow = c(1,2))
seqrplot(allHigh.seq, dist.matrix = allHigh.seq.om, border = NA, withlegend = FALSE)
seqrplot(allLow.seq, dist.matrix = allLow.seq.om, border = NA, withlegend = FALSE)
title(main = "Representative Sequences for All Tasks", cex.main = 2, outer = TRUE)


# Representative Sequecnces for Task 1
plot.new()
par(mfrow = c(1,2))
seqrplot(high_task1.seq, dist.matrix = high_task1.seq.om, border = NA, withlegend = FALSE)
seqrplot(low_task1.seq, dist.matrix = low_task1.seq.om, border = NA, withlegend = FALSE)
title(main = "Representative Sequences for Task 1", cex.main = 2, outer = TRUE)

# Representative Sequecnces for Task 2
plot.new()
par(mfrow = c(1,2))
seqrplot(high_task2.seq, dist.matrix = high_task2.seq.om, border = NA, withlegend = FALSE)
seqrplot(low_task2.seq, dist.matrix = low_task2.seq.om, border = NA, withlegend = FALSE)
title(main = "Representative Sequences for Task 2", cex.main = 2, outer = TRUE)



# Representative Sequecnces for Task 3
plot.new()
par(mfrow = c(1,2))
seqrplot(high_task3.seq, dist.matrix = high_task3.seq.om, border = NA, withlegend = FALSE)
seqrplot(low_task3.seq, dist.matrix = low_task3.seq.om, border = NA, withlegend = FALSE)
title(main = "Representative Sequences for Task 3", cex.main = 2, outer = TRUE)




# Event sequence analysis

# allHigh.seqe <- seqecreate(allHigh.seq)
# fsubseqHigh <- seqefsub(allHigh.seqe, pMinSupport = 0.5)
# allLow.seqe <- seqecreate(allLow.seq)
# fsubseqLow <- seqefsub(allLow.seqe, pMinSupport = 0.5)


# par(mfrow=c(1,2))
# plot(fsubseqHigh[1:15], col = "cyan")
# plot(fsubseqLow[1:15], col = "cyan")


#Using N-Grams

no_colHighNgram <- max(count.fields("nGramHigh_csv.nGram", sep = ",")) 
nGram_High <- read.table("nGramHigh_csv.nGram",sep=",",fill=TRUE, col.names=1:no_colHighNgram)

no_colLowNgram <- max(count.fields("nGramLowA_csv.nGram", sep = ",")) 
nGram_Low <- read.table("nGramLowA_csv.nGram",sep=",",fill=TRUE, col.names=1:no_colLowNgram)

nGram_High <- data.frame(sapply(nGram_High,  blank2na))
nGram_High.seq <- seqdef(nGram_High, xtstep = 6,  labels = task.lab)

nGram_Low <- data.frame(sapply(nGram_Low,  blank2na))
nGram_Low.seq <- seqdef(nGram_Low, xtstep = 6,  labels = task.lab)

nGram_High.om <- seqdist(nGram_High.seq, method = "OM", indel = 1, sm = "TRATE")
nGram_Low.om <- seqdist(nGram_Low.seq, method = "OM", indel = 1, sm = "TRATE")

# make clusters of high task n-grams 
clusterward_High <- agnes(nGram_High.om , diss = TRUE, method = "ward")
nGram_High.cl4 <- cutree(clusterward_High, k = 4)
nGram_High.cl4.lab <- factor(nGram_High.cl4, labels = paste("Cluster", 1:4))
#seqdplot(nGram_High.seq, group = nGram_High.cl4.lab, border = NA)

seqrplot(nGram_High.seq, group = nGram_High.cl4.lab, dist.matrix = nGram_High.om, trep = 0.35, border = NA)

# make clusters of low task n-grams 

clusterward_Low <- agnes(nGram_Low.om , diss = TRUE, method = "ward")
nGram_Low.cl4 <- cutree(clusterward_Low, k = 4)
nGram_Low.cl4.lab <- factor(nGram_Low.cl4, labels = paste("Cluster", 1:4))
#seqdplot(nGram_Low.seq, group = nGram_Low.cl4.lab, border = NA)
seqrplot(nGram_Low.seq, group = nGram_Low.cl4.lab, dist.matrix = nGram_Low.om, trep = 0.35, border = NA)


# Event sequence analysis

allHigh.seqe <- seqecreate(nGram_High.seq)
fsubseqHigh <- seqefsub(allHigh.seqe, pMinSupport = 0.05)
allLow.seqe <- seqecreate(nGram_Low.seq)
fsubseqLow <- seqefsub(allLow.seqe, pMinSupport = 0.05)


par(mfrow=c(1,2))
plot(fsubseqHigh[1:15], col = "cyan")
plot(fsubseqLow[1:15], col = "cyan")





