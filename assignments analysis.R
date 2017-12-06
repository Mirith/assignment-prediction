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

#' Overall question:  

#' Which independent variable(s) affect the assignment choice? 

####################################
# to do list
# change workhours simplifying (round)
# 
# explore data after dropping stuff visually
# change all numbers to ones that are from this assignment
# rewrite the conclusion

rm(list=ls(all=TRUE)) # clear memory

#' setting the working directory, ie where data is  
#' works for local machine, will have to change for others' though
setwd("~/LING 104/assignments 1") 

#' #Preliminary analysis of data

summary(x <- read.delim("assignments.csv"))

# to make things easier to see in plots
colors <- c("dark blue", "dark green", "red")

barplot(table(x$MISTAKES), col = "dark blue") # looks like a pretty normal distribution of mistakes

barplot(table(x$SEX), col = colors) # about half and half

barplot(table(x$ASSIGNMENT), col = colors) # pretty equally divided

# so far nothing looks weird/particularly influential

#' #Workhours

# x$WORKHOURS is really messy...

# plotting against assignment shows a general trend
barplot(table(x$ASSIGNMENT, x$WORKHOURS), col = colors, legend =TRUE)
# lower work hours correlated to oral exams, while high workhours to thesis
# middle work hours correlated to lab report

# simplifying workhours a little bit (by integer)
sort(x$WORKHOURS)
workhours.simple <- c()

for(num in 12:39)
{
    # appends the length of the vector of elements that are between num and num+1
    # ie, the number of people who worked num to num + 1 hours
    workhours.simple <- append(workhours.simple, 
                               length(which(x$WORKHOURS >= num & 
                                            x$WORKHOURS < num+1)))
}

# plotting simplified workhour data
barplot(workhours.simple, 
        ylab = "people", 
        xlab = "hours worked", 
        names.arg = c(12:39), 
        col = "dark blue")
# weird gap from 22-24
# probably won't be too problematic
# sort of peaks around 32

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

# each group has a spike in numbers in a separate format
# ie, oral exam for CE, thesis for H, and lab report for ME

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

#' #Creating the tree
#' CART because dependent variable has three levels and is categorical

library(tree)
set.seed(42)

summary(cart.1 <- tree(ASSIGNMENT ~ SEX+REGION+WORKHOURS+MISTAKES))

# copy pasted from CART 2
predictions.num <- # make predictions.num
    predict(cart.1)  # the predictions for the data from cart.1
predictions.cat <-      # make predictions.cat
    predict(cart.1,       # the predictions for the data from cart.1
            type="class") # but this time the categorical class predictions
table(ASSIGNMENT,    # cross-tabulate the actually produced constructions
      predictions.cat) # against the predictions

# from cart 2
#               predictions.cat
# CONSTRUCTION V_DO_PRT V_PRT_DO
# V_DO_PRT       88       12
# V_PRT_DO       28       72

# from this file
#              predictions.cat
# ASSIGNMENT   lab_report oral_exam thesis
# lab_report         97         0      3
# oral_exam           0       100      0
# thesis             21         0     79

# accuracy:
(97+100+79) / length(predictions.cat) # 0.92

(72+88) / length(predictions.cat) # (tp+tn) / (tp+tn+fp+fn) = 0.8

# replace with assignment numbers

# precision:
72/(72+12) # tp/(tp+fp) = 0.8571429

# recall/sensitivity:
72/(72+28) # tp/(tp+fn) = 0.72

# F:
2*((0.8571429*0.72)/(0.8571429+0.72)) # 2 * ((prec*recall)/(prec+recall)) = 0.7826087

# determine the nature of the effect(s) graphically
plot(cart.1)                        # plot the classification tree
    text(cart.1, pretty=5, all=TRUE) # add labels to it


# validation 1: comparing classification to prediction accuracy
sampler <- sample(            # make sampler the random ordering of
    rep(c("training", "test"), # the words "training" and "test"
        c(150, 50)))          # repeated 150 and 50 times respectively

cart.validation.training <-           # make cart.validation.training
    tree(formula(cart.1),              # a classification tree with the same formula as cart.1
         data=x[sampler=="training",]) # but applied only to the 302 training cases

predictions.validation.test <-          # make predictions.validation.test
    predict(cart.validation.training,    # the predictions from cart.validation.training
            newdata=x[sampler=="test",], # applied only to the 101 test cases
            type="class")                # return the categorical class predictions

sum(predictions.validation.test ==     # compute the number of cases where the prediction for the test data
        ASSIGNMENT[sampler=="test"]) /   # is the same as what actually happened in the test data
    length(predictions.validation.test) # and divide that by the number of test predictions (for a %)
# very similar to the accuracy of the whole data set



# validation 2: can we, or do we need to, prune the tree?
pruning <-                     # make pruning
    cv.tree(cart.1,             # the result of cross-validating the tree
            FUN=prune.misclass) # based on the number of misclassifications

plot(pruning$size,     # plot the pruned tree sizes
     pruning$dev,      # against the deviances those tree sizes come with
     type="b"); grid() # using points and lines; add a grid
# the deviances are lowest for 2 and 5 nodes, we pick 5 (because 2 uses only DO_LENGTH_SYLL)

cart.1.pruned <-         # make cart.1.pruned
    prune.misclass(cart.1, # a version of cart.1 pruned down to
                   best=5) # only 5 terminal nodes

plot(cart.1.pruned)                        # plot the classification tree
text(cart.1.pruned, pretty=0, all=TRUE) # add labels to it

# but does it do worse?
predictions.cat.pruned <- # make predictions.cat
    predict(cart.1.pruned, # the predictions for the data from cart.1.pruned
            type="class")  # the categorical class predictions

table(ASSIGNMENT,           # cross-tabulate the actually produced constructions
      predictions.cat.pruned) # against the predictions from the pruned tree

# change numbers for this assignment
# accuracy:
(74+86) / length(predictions.cat) # (tp+tn) / (tp+tn+fp+fn) = 0.8

# precision:
74/(74+14) # tp/(tp+fp) = 0.8409091

# recall/sensitivity:
74/(74+26) # tp/(tp+fn) = 0.74

# F:
2*((0.8409091*0.74)/(0.8409091+0.74)) # 2 * ((prec*recall)/(prec+recall)) = 0.787234

# visual exploration? or numerical only?

# new conclusion here: 


if (FALSE)
{
    # using a glm as the dependent variable is categorical
    summary(model.01 <- glm(ASSIGNMENT ~ (SEX+REGION+WORKHOURS+MISTAKES)^3, 
                            data = x, 
                            family = binomial))
    
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
    
    summary(model.07 <- update(model.06, ~. - REGION:MISTAKES))
    drop1(model.07, test="Chisq")
    
    # Everything is now significant
    
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
    # model.07 is only 0.5647909 -- that's pretty low
    # (model.06 r squared with mistakes is 0.5776149, slightly higher)

# summarizing everything #######################################################
# model.07 is significant (p-value of 2.2e-16) but has a low R-squared (about .565)
# value, indicating that the connection between the assignment chosen and 
# a number of independent variables is significant but noisy. 
# model.06 includes interactions between
# SEXmale:REGIONhispanic, SEXmale:REGIONmiddle eastern, SEXmale:WORKHOURS, 
# REGIONhispanic:WORKHOURS, REGIONmiddle eastern:WORKHOURS, WORKHOURS:MISTAKES  
# and the variablesSEXmale, WORKHOURS, MISTAKES, REGIONhispanic, 
# REGIONmiddle eastern
    
}
