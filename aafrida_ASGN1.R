# Assignment 1

# due via Moodle  04/11 by 10:30 PM

# You may discuss parts of the assignment with the QAC tutors and course TAs 
# but you must include a comment next to the a line of code indicating that 
# you received help on that particular line of the script.

##### data codebook gss06_codebook.doc in asgn1 directory and in P:\QAC\qac156\data\


# 	1.	SET YOUR WORKING DIRECTORY
setwd("P:/QAC/qac156/STUDENTS/aafrida/Assignments")
                                 # Set working directory

# 	2.	START A "LOG" FILE TO DOCUMENT OUTPUT
sink(file = "myresults.txt", append = F, type = "output", split = T) 
                                 # Begin "log"

# 	3.	READ the CSV file (gss06.csv) form the asgn1 directory or from P:/QAC/qac156/data/gss06.csv
#		 AND ASSIGN IT TO A WORKSPACE OBJECT
gss98 <- read.csv("P:/QAC/qac156/data/gss06.csv")



# 	4. 	DEAL WITH MISSING OBSERVATIONS
##### data codebook gss06_codebook.doc in lab2 directory and in  P:\QAC\qac156\data\
gss98$age[gss98$age == 0 | gss98$age > 89 ] <- NA
gss98$sex[gss98$sex == 0] <- NA
gss98$educ[gss98$educ >= 97] <- NA #check this condition with one of the tutors
gss98$degree[gss98$degree > 4] <- NA
gss98$rincome[gss98$rincome == 0 | gss98$rincome > 13] <- NA
gss98$gunlaw[gss98$gunlaw == 0 | gss98$gunlaw > 2] <- NA
gss98$gunsales[gss98$gunsales == 0 | gss98$gunsales > 5]




                                 

# 	5.	CALCULATE APPROPRIATE DESCRIPTIVE STATISTICS FOR THESE VARIABLES:
#		AGE, EDUC, DEGREE, GUNLAW; THEN SAVE ONE OF THE OUTPUTS TO A TEXT FILE

# Summary statistics for quantitative variables
summary(gss98$educ)
summary(gss98$age)


capture.output((summary(gss98$gunlaw)), file = "statistics.txt")


# Frequency table for categorical variables
table(gss98$degree)
table(gss98$gunlaw)







#	6. 	GENERATE HISTOGRAMS or bar graphs FOR DEGREE AND EDUC, SAVE AS PDFS
#categorical vars
barplot(table(gss98$degree), xlab = "Degree", ylab = "Frequency", main = "Degree Attainment") 

#quantitative vars
hist(gss98$educ, freq = T, breaks = 10, xlab = "Years", main = "Years of Education") 

pdf("new.pdf", width = 11, height = 7)
barplot(table(gss98$degree), 
        xlab = "Degree", 
        ylab = "Frequency", 
        main = "Degree Attainment")
#pdf("new.pdf", width = 11, height = 7)
barplot(table(gss98$educ), 
        xlab = "Years", 
        ylab = "Frequency", 
        main = "Years of Education")
dev.off()

#	7.	GENERATE A BAR CHART THAT DISPLAYS THE AVERAGE AGE
#		OF THE RESPONDENT FOR DIFFERENT LEVELS OF THE DEGREE VARIABLE, SAVE AS PDF
# 		BONUS (2 pts): ADD AVERAGE VALUES ABOVE BARS (see example in GSS98_example.R lines 368-378)

plot3.data <- aggregate(gss98$age, list(gss98$degree), FUN = "mean", na.rm = T)
xx1 <- barplot(plot3.data[,2], xlab = "Different Levels of Degree", ylab = "Average Age", main = "THE RESPONDENT FOR DIFFERENT LEVELS OF THE DEGREE VARIABL", ylim = c(0, 1.2*max(plot3.data[,2])))


#bonus
xx <- barplot(plot3.data[,2], xlab = "Different Levels of Degree", ylab = "Average Age", 
              beside = T, horiz = F, 
              main = "DEGREE versus Age", 
              legend.text = levels(plot3.data$Group.1), 
              args.legend = list(x = "topleft", 
                                 inset = 0.05), 
              ylim = c(0, 55)) # Save x-coordinates of bars
axis(1, at = xx, labels = plot3.data[,1])
text(x = xx, y = plot3.data[,2], 
     label = round(plot3.data[,2],2), 
     pos = 3, cex = 0.8)


#	8. 	CLOSE THE "LOG"
sink() # Close "log" file




