# ASGN 3
#PART I 3


# modify this file to include your program and comments 
# and submit it via Moodle as yourusername_asgn3_partI3.R


#############################################
  
#  PLEASE NOTE: For this assignment you are expected to work alone
#    and not seek help (or help others) in preparing the answers. 
#    Giving or receiving any help will be a violation of our honor code 

#############################################


############################ QUESTION 1
# For this exercise we are going to work with two data files 
# extracted from the cumulative data files of the General Social Survey.  

# The first data file gss82.csv also P:\QAC\qac156\data\ 
#   includes CASEID, ABANY, ABFIRM, ABIMP, ABINFO for 1982.
# The second data file gss82_98.dta alos in P:\QAC\qac156\data\) 
#   includes degree, educ, id, realinc, realrinc, sex, year for both 1982 and 1998

# The codebook is GSS_codebook_asgn3.txt also in P:\QAC\qac156\data\


############################ 1a

# Import gss82.csv  as a dataframe (gss82) and change variable names to lower case. 
gss82 <- read.csv("P:/QAC/qac156/STUDENTS/mkaparakis/Assignments/asgn3/gss82.csv") 
names(gss82) <- tolower(names(gss82))


# From the caseid variable  we will create two new variables: year and id 
      # we will use them later to merge with second data file
      # (NOTE: take a look at substr and other string functions)
# and then drop the caseid variable
# Please add appropriate comments to the following lines  
# and  explain how they help prepare our data for the next step (in 1b)

#converting all the numeric caseids of the caseid column to string datatype caseids
gss82$caseid <- as.character(gss82$caseid)

#taking the substring from position 1 to 4 of the strings in caseid column and
#converting them back to numeric and assigning that to a new column named "year"
gss82$year <- as.numeric(substr(gss82$caseid, 1, 4))

#taking the substring from position 5 to the end of the string type caseids of
#the caseid column and assign it to the new dummy variable named "id"
gss82$id <- substring(gss82$caseid, 5)

#if stringr package not installed, install the package and load the stringr package 
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

#trims the characters string of the ids of id column and assigns it back to id columns
gss82$id <- str_trim(gss82$id)

#converts the string characters to numeric using the ids of id columns
gss82$id <- as.numeric(gss82$id)


############################ end 1a

############################ 1b 
# Import gss82_98.dta and merge the two files (for the 1982 observations only)
install.packages('haven')
library(haven)
gss82_98 <- read_dta('P:/QAC/qac156/STUDENTS/mkaparakis/Assignments/asgn3/gss82_98.dta')
gss_all<-merge(gss82_98, gss82, by="id", all=TRUE) 
gss_all<-head(gss_all, 1982)



############################ end 1b

############################  1c

# Do appropriate data management and account for missing observations for all vars 
# (see GSS_codebook_asgn3.txt).  
# For variables  "abany", "abfirm", "abimp", "abinfo"  recode 0, 8, and 9 to missing 
# first using a for loop and then using one of the apply functions
vars <- c("abany", "abfirm", "abimp", "abinfo")

for(i in 1:length(vars)){
  gss_all[,vars[i]] <- ifelse(gss_all[,vars[i]] == 8, NA, 
                              ifelse(gss_all[,vars[i]] == 9, NA,
                                     ifelse(gss_all[,vars[i]] == 0, NA, gss_all[,vars[i]])))
}

#do it with an apply function
gss_all[, vars] <- apply(gss_all[, vars], 2, function(x) ifelse(x == 0, NA, 
                                                     ifelse(x == 8, NA,
                                                            ifelse(x == 9, NA, x))))



############################  end 1c
  
############################  1d  
#  summarize your data with numbers and "pictures"

# 1d i.	 Calculate descriptive statistics 
# and try to create a compact table as close as you can to the one below 
install.packages("tidyverse")
library(tidyverse)
install.packages("gmodels")
library(gmodels)

stats <- gss_all %>% group_by(abany) %>%
  summarise(across(c("educ","realinc","realrinc"), ~list(mean(.x, na.rm = TRUE),
                                                         sd(.x, na.rm = TRUE),
                                                         min(.x, na.rm = TRUE),
                                                         max(.x, na.rm = TRUE)))) %>%
  filter(abany %in% c(1,2)) %>%
  mutate(metric = c("mean", "sd", "min", "max")) %>%
  select(abany, metric, educ, realinc, realrinc)


# abany_group		  educ	    realinc	  realrinc
# 1		      mean	12.79525	28644.63	19815
# 1		      sd	  2.916303	21952.49	17719.73
# 1		      min	  3	        603	      603
# 1		      max	  20	      90722	    97992
# 2		      mean	11.37813	22312.05	16254.64
# 2		      sd	  3.23417	  19449.72	15120.35
# 2		      min	  0	        603	      603
# 2		      max	  20	      90722	    97992



# 1d ii.  create a barplot showing  the average values of realrinc realinc 
#   (with these values displayed above the bar) for men and women and different 
#   values of abany ( a single graph; make sure to have appropriate titles, labels  etc. ) 

plots <- gss_all %>%
  filter((abany %in% c(1,2))) %>%
  group_by(sex, abany) %>%
  summarise(across(c("realinc", "realrinc"), ~ list(mean(.x, na.rm = TRUE)))) %>%
  mutate("Group" = paste0("Sex: ", sex, " ABANY: ", abany)) %>%
  ungroup() %>%
  select(Group, realinc, realrinc) %>%
  gather(income,
         value,
         realinc:realrinc,
         factor_key=TRUE) %>%
  spread(Group, value) %>%
  as.data.frame()

row.names(plots) <- c("Family Income", "Respondent's Income")
plots <- plots[,-1]
plots <- apply(plots, c(1,2), unlist)
plots <- as.matrix(plots)

bp <- barplot(height = plots,
              beside = TRUE,
              names.arg = c("Pro-Abortion for Any Reason Male", "Anti-Abortion Male", "Pro-Abortion Female", "Anti Abortion for Any Reason Female"),
              legend.text = TRUE,
              main = "Family and Respondent's Income grouped by Abortion Stance and Sex",
              ylab = "Constant $ (as fo the year 1986)",
              ylim = c(0, 35000),
              args.legend = list(x = "topright", cex = 0.75), axes= TRUE)

text(bp,
     round(plots, 0),
     round(plots, 0),
     pos = 3,
     offset = 0.4)


# 1d iii.  for each value of sex create two-way tables (crosstabs) 
#   with degree as the row variable and abany  as the column variable.  
#   Show counts first and then row percentages.
# crosstab examples in lab2\GSS98_example.R (start line 280) 

table(gss_all$degree, gss_all$abany, dnn = c("Degree", "Abortion"))

#Sex = 1
CrossTable(gss_all$degree[gss_all$sex==1],
           gss_all$abany[gss_all$sex==1], digits=2,
           prop.r=TRUE, prop.c=FALSE,
           prop.t=F, prop.chisq=FALSE, dnn = c("Degree", "Abortion Stance"))

#Sex = 2
CrossTable(gss_all$degree[gss_all$sex==2],
           gss_all$abany[gss_all$sex==2], digits=2,
           prop.r=TRUE, prop.c=FALSE,
           prop.t=F, prop.chisq=FALSE, dnn = c("Degree", "Abortion Stance"))

# 1d iv.  create a  horizontal bar graph that shows the row percentages 
#   from a two way table with degree as the row variable and  abany as the 
#   column variable
horz <- as.data.frame(prop.table(table(gss_all$degree, gss_all$abany), 1)) %>%
  spread(Var1, Freq) %>%
  select('0', '1', '2', '3', '4')

rownames(horz) <- c("Pro_Abortion", "Anti_Abortion")
colnames(horz) <- c("Did Not Complete High School", "High School", "Junior High", "Bachelor", "Graduate")
barplot(as.matrix(horz),
        beside = TRUE, horiz = TRUE,
        legend.text = TRUE,
        main = "Abortion Stance by Degree Level",
        xlab = "%",
        xlim = c(0, 0.8),
        agrs.legend = list(x = "topright", cex = 0.75))


############################  end 1d		




#######   Enjoy your Summer! 
#######   stay safe and as always, stay out of trouble  :)
