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
# explore data after dropping stuff

rm(list=ls(all=TRUE)) # clear memory
#' setting the working directory, ie where data is  
#' works for local machine, will have to change for others' though
setwd("~/LING 104/assignments 1") 

#' #Preliminary analysis of data

summary(x <- read.delim("assignments.csv"))

# to make things easier to see in plots
colors <- c("blue", "dark green", "red")

barplot(table(x$MISTAKES)) # looks like a pretty normal distribution of mistakes

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

table(x$ASSIGNMENT, x$REGION)
# sort of unclear, lots of things going on

#' ##Region v Assignment
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

#' #Creating the regression model
# using a glm as the dependent variable is categorical
summary(model.01 <- glm(ASSIGNMENT ~ (SEX+REGION+WORKHOURS+MISTAKES)^3, 
        data = x[complete.cases(x),], 
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

#' Everything is now significant  

#' #Exploring the model visually
# asdk;flsdfflkj