# 1. moving (M)
# 2. enlarging to maximum size (E)
# 3. resizing to medium size (N)
# 4. shrinking (S)
# 5. Rotating (M)
# 6. making unions with other data slips (U)
# 7. add data slip to a group (G) and 
# 8. remove a data slip from a group (R)    



library("TraMineR")
no_colHigh <- max(count.fields("tranHigh.finalDB", sep = ",")) # I have not removed the duplicate consecutifve entteries. 
dataHigh <- read.table("tranHigh.finalDB",sep=",",fill=TRUE, col.names=1:no_colHigh)
head(dataHigh)
sayYes <- rep("High Performers", 69) #The status indicator is included so that we can make "group" on that. Helps make visualization with indexing

dataHigh$yes <- as.factor(sayYes)
head(dataHigh)
dataHigh <- dataHigh[, c(1,94,2:93)]
head(dataHigh)

colnames(dataHigh) <- paste0(rep("V",length(dataHigh)),1:length(dataHigh))  # To enable indexing much easier, I rename all the column names



# for the low performers


no_colLow <- max(count.fields("tranLowA.finalDB", sep = ",")) 
dataLow <- read.table("tranLowA.finalDB",sep=",",fill=TRUE, col.names=1:no_colLow)
sayNo <- rep("Low Performers", 69)
dataLow$no  <- as.factor(sayNo)
head(dataLow)
dataLow <- dataLow[, c(1,151,2:150)]
head(dataLow)
colnames(dataLow) <- paste0(rep("V",length(dataLow)),1:length(dataLow))

# Now, we have all the data about high and low perfermers


library(plyr)
combined <- rbind.fill(dataHigh, dataLow) # Both the dataframes are now combined. 

# To make the legend explanotory

dataSeq.scode <- c("Enlarge", "Normal", "Union", "Add To Group", "Remove from Group", "Move", "Shrink")
dataSeq.lab <- c("E", "N", "U", "G", "R", "M", "S")
task.lab = c("Enlarge", "Add to Group", "Move", "Normal Size", "Remove from Group", "Shrink", "Union")

#function to replace blanks with missing
#apply that function
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}



#Setting the parameters for graphical display 

par(mar=c(3.8, 4.5, 5.0, 2.5), oma = c(1.5,1.2,4.5,1), xpd=TRUE)

entire <- data.frame(sapply(combined[,4:150],  blank2na))
entire.seq <- seqdef(entire, xtstep = 6, labels = task.lab)

# Figure 1: Total Mean Time Spent Across All Tasks 

seqmtplot(entire.seq, group = combined$V2,  ylim = c(0,50), legend.prop = 0.22 )
title(main = "Total Mean Time Spent", cex.main = 2, outer = TRUE)

## Output Explanation of Figure 1:  
# The high performers have spend much significatly lesser time in moving data slips around and also on performing unions. 
# The mean-time spent on rest of the activities are not significanly different however from mere visual observation.


combined_1 <- subset(combined, V1 =='A') # subsetting df based on the Task name. 
combined_2 <- subset(combined, V1 =='W')
combined_3 <- subset(combined, V1 =='O')

entire_1 <- data.frame(sapply(combined_1[,4:150],  blank2na))  # the first 3 columns comprise task name, status indicator, slip no.
                                                               # rest columns have the data about performed actions, thats' y they are kept.
entire_2 <- data.frame(sapply(combined_2[,4:150],  blank2na))
entire_3 <- data.frame(sapply(combined_3[,4:150],  blank2na))

entire_1.seq <- seqdef(entire_1, xtstep = 6, labels = task.lab, right = "DEL")
entire_2.seq <- seqdef(entire_2, xtstep = 6, labels = task.lab, right = "DEL")
entire_3.seq <- seqdef(entire_3, xtstep = 6, labels = task.lab, right = "DEL")


##### The data description i.e about the dataframes

##Entire data and the dataframe name
entire.seq 
combined

##Task 1
entire_1.seq
combined_1

## Task 2

entire_2.seq
combined_2

##Task 3

entire_3.seq
combined_3


### Now, we shall perform all the visualizations as they are performned in the TraMineR tutorial.

#Step 1: Load the library (already loaded)

###Operation 1:Build a typology of the observed three stage trajectories i.e. Information Gathering, Grouping, 
### Sequencing and Webbing; by clustering the sequences.


#Step 2: Compute pairwise optimal matching (OM) distances between sequences with an insertion/
#deletion cost of 1 and a substitution cost matrix based on observed transition
#rates:

entire.om <- seqdist(entire.seq, method = "OM", indel = 1, sm = "TRATE")

#Step 3: Proceed to an agglomerative hierarchical clustering using the obtained distance matrix,
#select the four-clusters solution and express it as a factor:

library("cluster")
entire_clusterward <- agnes(entire.om, diss = TRUE, method = "ward")
entire.cl4 <- cutree(entire_clusterward, k = 3)
entire.cl4.lab <- factor(entire.cl4, labels = paste("Cluster", 1:3))

# Step 4: Visualize the cluster patterns by plotting their transversal 
# state distributions

seqdplot(entire.seq, group = entire.cl4.lab, border = NA, legend.prop = 0.22)
title(main = "State distribution plots by cluster", cex.main = 2, outer = TRUE)

##OUTPUT --->>
#UNCLEAR, because I don't know how to interpret the sequence distripution plots as of now. 
# I will come back to it once I am clear about it. 


### Operation 2: Visualizing Individual State Sequences

## 2.a Sequence Index Plots:::We visualize, for each case, the individual longitudinal succession of states as  
## well as, through the length of each color segment 

# for all the tasks together 
seqiplot(entire.seq, group = combined$V2, border = NA, legend.prop = 0.22 )
title(main = "Sequence index plot of sequences 1 to 10", cex.main = 2, outer = TRUE)

# for Task 1
seqIplot(entire_1.seq, group = combined_1$V2, border = NA, legend.prop = 0.22 )
title(main = "Sequence index plot of sequences 1 to 10 for Task 1", cex.main = 2, outer = TRUE)
#The sequence index plots of "High Achievers" for task 1 is similar to that of "Low Achievers"

# for Task 2
seqiplot(entire_2.seq, group = combined_2$V2, border = NA, legend.prop = 0.22 )
title(main = "Sequence index plot of sequences 1 to 10 for Task 1", cex.main = 2, outer = TRUE)
#Output: The sequence index plots of "High Achievers" for task 2 is significantly bigger.


# for Task 3
seqiplot(entire_3.seq, group = combined_3$V2, border = NA, legend.prop = 0.22 )
title(main = "Sequence index plot of sequences 1 to 10 for Task 3", cex.main = 2, outer = TRUE)
#Output: The sequence index plots of "High Achievers" for task 3 is significantly bigger.

##TODO--> plot sequence index plots by using the distance to the most frequent sequence or the 
# scores of multidimensional scaling analysis of similarities between sequences


## 2.b Sequence Frequency Plots

seqmsplot(entire_3.seq )














# Figure 2: Mean Time Spent Across Task 1
seqmtplot(entire_1.seq, group = combined_1$V2, ylim = c(0,50), legend.prop = 0.22 )
title(main = "Mean Time Spent in Task 1", cex.main = 2, outer = TRUE)


# Figure 2: Mean Time Spent Across Task 2
seqmtplot(entire_2.seq, group = combined_2$V2, ylim = c(0,50), legend.prop = 0.22  )
title(main = "Mean Time Spent in Task 2", cex.main = 2, outer = TRUE)

# Figure 2: Mean Time Spent Across Task 3
seqmtplot(entire_3.seq, group = combined_3$V2, ylim = c(0,50), legend.prop = 0.22  )
title(main = "Mean Time Spent in Task 3", cex.main = 2, outer = TRUE)


# Statistics1: The summary statistics of the mean time spent in Task 1
by(entire_1.seq, INDICES  = combined_1$V2, FUN = seqmeant)


### Visualizing Indiviudal State Sequences

## 1. Sequence Index Plots

seqiplot(entire.seq, group = combined$V2, border = NA, legend.prop = 0.22 )

## 2. Sequence Frequency Plots
seqfplot(entire.seq, border =NA, group = combined$V2, legend.prop = 0.22 )

# Sequence Density Plot

seqdplot(entire.seq, border =NA, group = combined$V2, legend.prop = 0.22)

# Longitduinal Entropy

seqHtplot(entire.seq, group = combined$V2, legend.prop = 0.22)

high.trate<-seqtrate(entire.seq[1:69,])
round(high.trate,2)

low.trate<-seqtrate(entire.seq[70:138,])
round(low.trate,2)



complexityHigh <- seqici(entire.seq[1:69,])
complexityLow <- seqici(entire.seq[70:138,])
complexityEntire <- cbind(complexityHigh, complexityLow)
complexityEntire
par(mfrow=c(1,1))
plot.new()
colnames(complexityEntire) <- c("High", "Low")
matplot(complexityEntire[,c("High", "Low" )], pch = 15:16, cex = 1.8, col = c(2:3), type = "o",  
        xlab = "Number of Clients", ylab = "kbit/s", xaxt="n", yaxt="n", lwd = 3, cex.lab=2.0)

#plot(0,type='n',axes=FALSE,ann=FALSE)

legend("center", legend=c("High", "Low") ,pch = 15:16, col=c(2:3),
       title = "Achievers Group",title.col ="black", cex=1.5, pt.cex = 2.0, horiz = FALSE)

title(main = "Longitudinal Entropy of Individual DataSlips", cex.main = 2, outer = TRUE)







