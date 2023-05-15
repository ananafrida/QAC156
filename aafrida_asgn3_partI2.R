

############################ PART I (2) 

# For the following plotting exercises we will use the satarcadia.rdata file
# also in the course data directory. Please 'load' it.
setwd("P:/QAC/qac156/data")
load("satarcadia.rdata")


# Please add appropriate comments to the following lines so that 
# a person not very familiar with R will understand how this script is used
# to create graph 1 in asgn3GRAPHS.pdf  also in P:\QAC\qac156\students\mkaparakis\Assignments\asgn3 


# 2a
# GRAPH 1


# Creates a new linear regression model named sat.lm where sat is the outcome
#variable ans gpa is the predictor variable using data from the satarcadia dataset.
#this will allowing is to run regressions
sat.lm <- lm(sat ~ gpa, data =satarcadia)

#creates a sequence called newgpa that begins at 1.8, terminates at 
#4.3 ans has length of 100.
newgpa <- seq(1.8,4.3, length.out=100)

#Creates a prediction called pred.sat that references the linear regression
#sat/lm, that is using a data frame that consists of newgpa (a sequence
#that begins at 1.8 and terminates at 4.3 and has length of 100) as its input
#data to predict the values and has a confidence interval as its type of interval.
pred.sat <- predict(sat.lm, newdata = data.frame(gpa=newgpa), 
                    interval = 'confidence')

#creates a scatter plot of sat values on the y-axis and
#gpa values on the x-axis, with the label "SAT and GPA"
#the 'n' refers to there being no line that connects all
#the points in the diagram being connected being drawn.
plot(sat ~ gpa, data = satarcadia,
     main= " SAT and GPA",
    type = 'n')

#'draws a polygone whose vertices are equal to the standard error of the linear 
#'regression, which are filled in with grey.
polygon(c(rev(newgpa), newgpa), c(rev(pred.sat[ ,3]), pred.sat[ ,2]), col = 'grey90', border = NA)

#draws the linear regression sat.lm in the color blue (will be on top
#of the polygon drawn in the previous line of the code)
abline(sat.lm, col='blue')

#as said in the comments by the code, these draw dashed red lines at the upper and 
#lower bound of the polygon, which represents the standard error
# lines(newgpa, pred.sat[ ,3], lty = 'dashed', col = 'red') # upper bound (pred.sat$upr)
# lines(newgpa, pred.sat[ ,"lwr"], lty = 'dashed', col = 'red') # lower bound (pred.sat$lwr)


#draws each individual data point found in satarcadia, with each point 
#representing one individual's gpa ans sat score in blue.
points(satarcadia$gpa, satarcadia$sat, col = "blue")


####### end 2 a

#2 b
# reproduce  Graph 1 using ggplot2. see graph 2 
# in asgn3GRAPHS.pdf 
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

ggplot(satarcadia) + 
  geom_point(aes(x = gpa, y = sat),color = "blue") + 
  geom_smooth(aes(x = gpa, y = sat), method = lm) + 
  labs(title = "SAT and GPA") + 
  theme(plot.title = element_text(hjust = 0.5))




###### end 2b

#2c

# Add an new factor variable (ap) to the satarcadia data frame  
# that takes the value 1 if the student has taken either AP math or AP English; 
# zero otherwise 
satarcadia$ap <- factor(ifelse(((satarcadia$apmath == 1) | (satarcadia$apeng == 1)), 1, 0))




# end 2c

# 2d i
# Use ggplot 2 to show separate regresion lines for sat=f(gpa) with confidence intervals
# for the different levels of ap (see graph 3 in asgn3GRAPHS.pdf ) 

ggplot(satarcadia) + 
  geom_smooth(aes(x = gpa, y = sat, color = as.factor(ap)), method = "lm") + 
  geom_point(aes(x = gpa, y = sat, color = as.factor(ap))) +
  labs(title = "SAT and GPA", subtitle = "with (1) and without (0) AP courses") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# 2d ii
# reproduce graph 3 using qplot
qplot(gpa, sat, col = as.factor(ap), geom = c("point", "smooth"),
      method = "lm", data = satarcadia, 
      main = "SAT and GPA\nwith (1) and without (0) AP courses")+
  theme(plot.title = element_text(hjust = 0.5))





#end 2 d


# 2 e
# Now show the relationship between sat and gpa 
# for different levels of ap and gend as in graph 4 in asgn3GRAPHS.pdf

ggplot(satarcadia) + 
  geom_smooth(aes(x = gpa, y = sat), method = lm) + 
  geom_point(aes(x = gpa, y = sat, shape = ap)) +
  labs(title = "SAT and GPA", subtitle = "For Female (F) and Male (M) Students \n \n with (1) and without (0) AP courses") +
  facet_grid(ap~gend) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))






# end 2e
