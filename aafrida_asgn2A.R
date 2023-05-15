# Assignment 2A


#1. import family63A_tab.txt (tab delimited data) as A.data

setwd("P:/QAC/qac156/STUDENTS/aafrida/Assignments")
#sink(file = "myresults.txt", append = F, type = "output", split = T)#do I NEED this?

A <- read.delim("P:/QAC/qac156/data/family63A_tab.txt")# make it check by tutors

#2. import family63B_FreeForm.txt (variable names: id fs edu age exp m race region) as B.data
B <- read.table("P:/QAC/qac156/STUDENTS/mkaparakis/Assignments/asgn2/family63B_FreeForm.txt",
                                 quote="\"", comment.char="")
names(B) <- names(A)

#3. import family63C.xls as C.data with variable names set to lower case
install.packages("readxl")
library(readxl)
C <- read_excel("P:/QAC/qac156/STUDENTS/mkaparakis/Assignments/asgn2/family63C.xls", 1) 
names(C) <- tolower(names(C))

#4. Merge, append as appropriate to create a data file with all observation and variables
#that you can use to
AB <- rbind(A, B)
ABC <- merge(AB, C, by="id", all=TRUE)

#a. calculate appropriate summary statistics for edu, race, region , e, i, w, s
summary(ABC$e)
summary(ABC$i)
summary(ABC$w)
summary(ABC$s)
summary(ABC$edu)
table(ABC$race)
table(ABC$region)

#b. use appropriate graphs to show the distributions of region and e. 
#(Just include relevant code in your R script file. You don’t need to
#add these graphs to the pdf file along with the graphs of (c).
# categorical vars
barplot(table(ABC$region), xlab = "region", ylab = "Frequency", main = "Region Description") 

#quantitative vars
hist(ABC$e, freq = T, breaks = 10, xlab = "E", main = "Description of E") 

#c. to produce the following three graphs (If you run into any problems 
#in the previous steps (putting all the data together) you can use 
#family63.RData (in the course’s data directory) to create the graphs)
pdf("assgn2A graph.pdf", width = 11, height = 7)
ABC$region_new <- ifelse(ABC$region==1, "Northeast", 
                             ifelse (ABC$region==2, "North Central", 
                                       ifelse (ABC$region==3, "South", "West")))

plot3.data <- aggregate(cbind(e, i, w, s) ~ region_new, mean,
                        data = ABC, na.rm = T)

xx1 <- barplot(as.matrix(plot3.data[,2:5]), 
               xlab = "Different Regions", 
               ylab = "Average (in thousands)", 
               main = "Graph1.Family Finances: A Regional Profile", 
               ylim = c(0,16), 
               beside = T, 
               legend = plot3.data$region_new,
               names.arg = c("Earnings", "Income", "Wealth", "Savings"))

y <- round(as.matrix(plot3.data[,2:5]), 2)

text(xx1, y + 0.8, labels = as.character(y))

# took Gato's help on partA(4.c)


################################BONUS PART############################################
pairs(ABC[,c(3,9,10,11,12)], main = "Graph2. Exploring Simple Associations")

#graph 3 -> I got help from QAC Center on this part
pars <- par()
 par(oma = c(4,4,4,4), mfrow=c(2,2))
 
 plot(ABC$edu[ABC$region==1], ABC$e[ABC$region==1],
      type="p", xlim = c(0, range(ABC$edu)[2]), ylim=range(ABC$e),
      ylab = "", xlab = "Northeast")
 abline(lm(ABC$e[ABC$region==1] ~ ABC$edu[ABC$region==1]))
 
 
 plot(ABC$edu[ABC$region==2], ABC$e[ABC$region==2],
      type="p", xlim = c(0, range(ABC$edu)[2]), ylim=range(ABC$e),
      ylab = "", xlab = "North Central")
 abline(lm(ABC$e[ABC$region==2] ~ ABC$edu[ABC$region==2]))
 
 
 plot(ABC$edu[ABC$region==3], ABC$e[ABC$region==3],
      type="p", xlim = c(0, range(ABC$edu)[2]), ylim=range(ABC$e),
      ylab = "", xlab = "South")
 abline(lm(ABC$e[ABC$region==3] ~ ABC$edu[ABC$region==3]))
 
 
 plot(ABC$edu[ABC$region==4], ABC$e[ABC$region==4],
      type="p", xlim = c(0, range(ABC$edu)[2]), ylim=range(ABC$e),
      ylab = "", xlab = "West")
 abline(lm(ABC$e[ABC$region==4] ~ ABC$edu[ABC$region==4]))
 
 mtext("Years of Education", side=1, outer= T)
 mtext("Earnings Head, in thousands", side = 2, outer = T)
 mtext("Graph 3: Earnings and Education - Regional Similarities", side = 3, outer = T)
 par <- pars
 dev.off()