# ASGN # 2 PART B
# due  4/25 by 10:30 PM


# You may discuss parts of the assignment with the QAC tutors, course TAs, 
# and the instructor but I ask that you include -- you MUST include--
# a comment next to the line indicating that you 
# received help (and from whom) on that particular line of the script.

# Please write your R script (functions) following my comments/instructions
# the expectation is that you will use R functions to do the work not edit files
# or move files from their location before processing them with R

# please save and rename (replacing yourusername) this file  with your code as 
# yourusername_asgn2B.R and submit it via Moodle


# See references (outline files) and examples (.R files)  
# in the lab3 and lab4 directories



#####################    1 
#         The  file 
#         http://www.stata-press.com/data/dmus/cardio1.dta
#         is a Stata data file that contains measurements of systolic
#         blood pressure(bp1-bp5) and pulse rate (pl1-pl5)


###  import the data file in R
install.packages('haven')
library(haven)
data <- read_dta('http://www.stata-press.com/data/dmus/cardio1.dta')



###  We want to create a dummy variable to indicate whether 
#    a blood pressure measurement was high (greater than 130) 

#  using  the ifelse function please create a new variable 
#  hi1a_bp1=1 if bp1 is greater or equal to 130, zero otherwise

data$hi1a_bp1<-ifelse(data$bp1 >= 130, 1, 0)


#  using the recode {car}  function ( in car package/library) please create a new variable 
#  hi1b_bp1=1 if bp1 is greater or equal to 130, zero otherwise
install.packages("car")
library(car)
  
data$hi1b_bp1 <- as.integer(recode(data$bp1, "lo:129=0; 130:hi=1"))


# one can repeat this for the other variables with blood pressure measurements
# but maybe there are better ways
####  use a for loop to  create  new variables  
#     hi2_bpx=1 if bpx  (where x =1,...5) is greater or equal to 130, zero otherwise 

vars<- c("bp1", "bp2", "bp3", "bp4", "bp5") 

add_vars <- dim(data)[2]
for(i in 1:length(vars)){
  data[, add_vars + i] <- ifelse(data[, vars[i]]>=130, 1, 0)
  names(data)[add_vars + i] <- paste0("hi2_", vars[i])
}




####  now recreate these variables  but instead of using a for loop use apply functions
#   and  a) ifelse function
vars<- c("bp1", "bp2", "bp3", "bp4", "bp5") 

temp = apply(data[, vars] >= 130, 2, ifelse, yes = 1, no = 0)
colnames(temp)<- paste0("hi2_", vars)
data<-cbind(data, temp)

# and   b) recode function
vars<- c("bp1", "bp2", "bp3", "bp4", "bp5") 

temp = apply(data[, vars], 2, function(x) recode(x, "lo:129=0; 130:hi=1"))
colnames(temp)<- paste0("hi2_", vars)
data<-cbind(data, temp)



# create a new variable hi_inc to show the total number of high blood pressure
# measurements for each participant

data$hi_inc <- apply(apply(data[, vars] >= 130, 2, ifelse, yes = 1, no = 0), 1, sum)



#####################    2 
#  The file stata_dmu_gas_data.txt (unformated text file; also in P:/QAC/qac156/data/)
#  has data for the price of gas per gallon from 1974 to 1976 for different countries,
#  along with inflation factors to bring gas prices to current dollars
#  (original file http://www.stata-press.com/data/dmus/gaswide.dta)


###  import the text data file in R as a dataframe and call it gasdata

gasdata <- read.table("P:/QAC/qac156/data/stata_dmu_gas_data.txt", 
                       header=T)

#  write a for loop to calculate the product of 
#  (gas price in year x) * (inflation factor in year x)
#  i.e. gascur1974=gas1974*inf1974 etc.
var1 <- c("gas1974", "gas1975", "gas1976")
var2 <- c("inf1974", "inf1975", "inf1976")

add_vars <- dim(gasdata)[2]
for(i in 1:length(var1)){
  gasdata[, add_vars + i] <- gasdata[, var1[i]] * gasdata[, var2[i]]
  names(gasdata)[add_vars + i] <- paste0("product", as.character(1973 + i))
}
                
                
                
# restructure the data to long
# and create a new variable, GASCUR, that reflects gas prices in current dollars


GASCUR.long <- reshape(gasdata[,1:7], idvar = "cty", 
                     varying = list(2:4,5:7), v.names = c("gas", "inf"), 
                     timevar="year", times = 1976:1978, direction = "long") 
rownames(GASCUR.long) <- NULL

GASCUR.long$GASCUR <- GASCUR.long$gas * GASCUR.long$inf

# end

