#' #Introduction

#' Data: assignments.csv  

#' Information from 104_datafiles.pdf about variables:  

#' ASSIGNMENT: the dependent variable, the final assignment chosen by a student:  
#'     oral_exam vs. lab_report vs. thesis  
#' SEX: the sex of the student: female vs. male  
#' REGION: the geographical region of origin of the student:  
#'     central_european vs. hispanic vs. middle_eastern  
#' WORKHOURS: the number of hours students self-report they invested into the course  
#' MISTAKES: the number of mistakes in the last test before choosing the assignment  

#' #Overall question:  
#' Which independent variable(s) affect the assignment choice? 

####################################
# to do list
# choose better colors
# fix headings -- all heading1...
# explore workhours -- verbose... look at summary only? 
# cut some of the exploration that is simple, look at summary?
# explore data after dropping stuff visually

rm(list=ls(all=TRUE)) # clear memory

#' setting the working directory, ie where data is  
#' works for local machine, will have to change for others' though
setwd("~/LING 104/assignments 1") 

#' #Preliminary analysis of data

summary(x <- read.delim("assignments.csv"))

# to make things easier to see in plots
colors <- c("blue", "dark green", "red")

sort(x$WORKHOURS)

workhours.simple <- c()

for(num in 12:39)
{
    workhours.simple <- append(workhours.simple, length(which(x$WORKHOURS >= num & x$WORKHOURS < num+1)))
}

workhours.simple

names <- c(12:39)

barplot(workhours.simple, 
        ylab = "people", 
        xlab = "hours worked", 
        names.arg = names, 
        col = "dark blue")

barplot(table(x$MISTAKES), col = "dark blue") # looks like a pretty normal distribution of mistakes

barplot(table(x$SEX), col = colors) # about half and half

barplot(table(x$ASSIGNMENT), col = colors) # pretty equally divided

# so far nothing looks weird/particularly influential

#' ##Sex v Assignment
table(x$ASSIGNMENT, x$SEX)
# numbers look about even for lab_report
# but a lot more females chose to do a oral_exam than males
# and a lot more males chose to do a thesis than females
# easier to see it with a barplot
barplot(table(x$ASSIGNMENT, x$SEX), 
        beside = TRUE, 
        legend.text = TRUE, 
        col = colors,
        ylab = "number") 

#' ##Region v Assignment
table(x$ASSIGNMENT, x$REGION)
# sort of unclear, lots of things going on

barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[1,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Central European",
        ylab = "percentage") 
barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[2,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Hispanic",
        ylab = "percentage") 
barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[3,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Middle Eastern",
        ylab = "percentage") 

# GIVE NAME
# each group has a spike in numbers in a separate format
# ie, oral exam for CE, thesis for H, and lab report for ME
# easy to see the large spike in plot, all together
barplot(table(x$ASSIGNMENT, x$REGION), 
        beside = TRUE, 
        legend.text = TRUE, 
        col = colors,
        ylab = "number")

#' ##Mistakes v Assignment
table(x$ASSIGNMENT, x$MISTAKES)
# 0s appear in interesting places
# easier to see with a plot
barplot(table(x$ASSIGNMENT, x$MISTAKES), 
        legend.text = TRUE, 
        col = colors)
# shows the distribution of colors/assignment types over number of mistakes made

# everything looks pretty normal, nothing weird going on
# getting the data as vectors
attach(x)

table(x$WORKHOURS) # is really messy, cleaning it up a bit

for (num in c(1:40))
{
    print(sum(x$WORKHOURS[num:num+1]))
}

#' #Creating the regression model
# using a glm as the dependent variable is categorical
summary(model.01 <- glm(ASSIGNMENT ~ (SEX+REGION+WORKHOURS+MISTAKES)^3, 
        data = x, 
        family = binomial))

#' #Testing for significance
#' And dropping predictors that don't contribute to the model

drop1(model.01, test="Chisq") # p-value of the droppable predictor(s)
    # using Chisq because it's a glm

# dropping the predictor with highest pvalue/least significance
summary(model.02 <- update(model.01, ~. - SEX:REGION:MISTAKES))

drop1(model.02, test="Chisq")

summary(model.03 <- update(model.02, ~. - SEX:WORKHOURS:MISTAKES))

drop1(model.03, test="Chisq")

summary(model.04 <- update(model.03, ~. - SEX:REGION:WORKHOURS))

drop1(model.04, test="Chisq")

# now some things are significant!
# continuing to drop predictors that do not contribute

summary(model.05 <- update(model.04, ~. - REGION:WORKHOURS:MISTAKES))

drop1(model.05, test="Chisq")

summary(model.06 <- update(model.05, ~. - SEX:MISTAKES))

drop1(model.06, test="Chisq")

#summary(model.07 <- update(model.06, ~. - REGION:MISTAKES))

#drop1(model.07, test="Chisq")

#' Everything is now significant  

#' #Exploring the model 

# significance test for model
# comparing it to a null model, ie one with no predictors
# p-value of 2.2e-16, ie significant
anova(model.06,
      glm(ASSIGNMENT ~ 1,
          data=x,
          family=binomial),
      test="Chisq") 

rms::lrm(formula(model.06))$stats[c("C", "R2")]
# r squared with mistakes is 0.5776149
# model.07 is only 0.5647909 -- that's pretty low
# range is .5 to 1...



# sample summary

# summarizing everything #######################################################
# model.07 is significant (p-value of 2.2e-16) but has a low R-squared (about .565)
# value, indicating that the connection between the assignment chosen and 
# a number of independent variables is significant but noisy. 
# model.06 includes interactions between
# SEXmale:REGIONhispanic, SEXmale:REGIONmiddle eastern, SEXmale:WORKHOURS, 
# REGIONhispanic:WORKHOURS, REGIONmiddle eastern:WORKHOURS, WORKHOURS:MISTAKES  
# and SEXmale, WORKHOURS, MISTAKES, REGIONhispanic, REGIONmiddle eastern
