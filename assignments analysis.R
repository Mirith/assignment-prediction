#' #Introduction

#' Data: assignments.csv  

#' Information from 104_datafiles.pdf about variables:  

#' **ASSIGNMENT**: the dependent variable, the final assignment chosen by a student:  
#'     oral_exam vs. lab_report vs. thesis  
#' **SEX**: the sex of the student: female vs. male  
#' **REGION**: the geographical region of origin of the student:  
#'     central_european vs. hispanic vs. middle_eastern  
#' **WORKHOURS**: the number of hours students self-report they invested into the course  
#' **MISTAKES**: the number of mistakes in the last test before choosing the assignment  

#' Overall question:  

#' Which independent variable(s) affect the assignment choice? 

rm(list=ls(all=TRUE)) # clear memory

#' Setting the working directory, ie where data is  
#' Works for local machine, will have to change for others' though
setwd("~/LING 104/assignments 1") 

# for making the trees later
library(tree)
set.seed(42)

#' #Preliminary analysis of data

summary(x <- read.delim("assignments.csv"))

# to make things easier to see in plots
colors <- c("dark blue", "dark green", "red")

#' ##Simple plots

barplot(table(x$MISTAKES), 
        main = "Mistakes",
        ylab = "persons making mistakes", 
        xlab = "number of mistakes", 
        col = "dark blue") 
# looks like a pretty normal distribution of mistakes

barplot(table(x$SEX), legend = TRUE, col = colors) # about half and half

barplot(table(x$ASSIGNMENT), legend = TRUE, col = colors) # pretty equally divided

# so far nothing looks weird/particularly influential

#' ##Sex

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

#' ##Region

table(x$ASSIGNMENT, x$REGION)
# sort of unclear, lots of things going on

barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[1,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Central European",
        ylab = "percentage that chose") 
barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[2,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Hispanic",
        ylab = "percentage that chose") 
barplot(prop.table(table(x$REGION, x$ASSIGNMENT)[3,]), 
        legend.text = TRUE, 
        col = colors, 
        main = "Middle Eastern",
        ylab = "percentage that chose") 
# each group has a spike in numbers in a separate format
# ie, oral exam for CE, thesis for H, and lab report for ME

#' ##Workhours 

# plotting against assignment shows a general trend
barplot(table(x$ASSIGNMENT, 
              x$WORKHOURS), 
        ylab = "people working",
        xlab = "workhours",
        col = colors, 
        legend =TRUE)
# lower work hours correlated to oral exams, while high workhours to thesis
# middle work hours correlated to lab report

# plotting simplified (by integer, not exact decimal) workhour data
barplot(table(factor(round(x$WORKHOURS), levels=12:39)), 
        ylab = "people", 
        xlab = "hours worked",
        col = "dark blue")
#' Weird gap from 22-24?  May be two separate distributions.  

#' ##Mistakes
table(x$ASSIGNMENT, x$MISTAKES)
# 0s appear in interesting places
# easier to see with a plot
barplot(table(x$ASSIGNMENT, x$MISTAKES), 
        legend.text = TRUE, 
        ylab = "number of people",
        xlab = "number of mistakes",
        col = colors)
# shows the distribution of colors/assignment types over number of mistakes made

#' ###Overall
#' Everything looks pretty normal, nothing weird going on.

attach(x)

#' #Creating the tree

summary(cart.1 <- tree(ASSIGNMENT ~ SEX+REGION+WORKHOURS+MISTAKES))

#' ##Classification accuracy
predictions.num <- predict(cart.1)  
predictions.cat <- predict(cart.1, type = "class") 

# this is really, really messy
plot(cart.1)
    text(cart.1, pretty=1, all=TRUE) 
# but first three nodes from the top all involve workhours, probably important
# harder to see all the terminal nodes, hopefully it can be simplified later

# actual versus prediction
table(ASSIGNMENT, predictions.cat) 

# accuracy:
(97+100+79) / length(predictions.cat)
# main diagonal of previous table added up
# accuracy looks pretty good

#' ##Making training/testing set to apply to data

# 300 data points, 75% to training set and 25% to testing
sampler <- sample(rep(c("training", "test"), c(225, 75)))

# tree from training data
cart.validation.training <- tree(formula(cart.1),              
         data=x[sampler=="training",])

# making predictions for test data based on training data
predictions.validation.test <- predict(cart.validation.training, 
            newdata=x[sampler=="test",], 
            type="class")

# percentage of times the predictions matched the actual data
sum(predictions.validation.test ==     
        ASSIGNMENT[sampler=="test"]) /   
    length(predictions.validation.test) 
# seems good

#' #Pruning (if necessary)

# based on number of misclassifications
pruning <- cv.tree(cart.1, FUN = prune.misclass)

plot(pruning$size, pruning$dev, type="b"); grid()
#' The deviances are lowest for 5 and 8 nodes
#' but 8 is the same tree... so pruning to 5 nodes

# pruned to 5 terminal nodes
cart.1.pruned <- prune.misclass(cart.1, best=5) 

# still really messy
# plot the new, pruned tree
plot(cart.1.pruned)
    text(cart.1.pruned, pretty=1, all=TRUE) 

# new tree's performance
predictions.cat.pruned <- predict(cart.1.pruned, type="class") 

#' ##Testing new accuracy
table(ASSIGNMENT, predictions.cat.pruned) 

# accuracy calculated as above
(91+100+79) / length(predictions.cat)
# so a teeny tiny bit worse (loses .02 accuracy...)
# but far less complicated, so probably better

#' ##Testing with subsets of data

# same training data as earlier, but with the pruned tree
cart.validation.training.1 <- tree(formula(cart.1.pruned), 
                                   data=x[sampler=="training",])

# making predictions for test data based on training data
predictions.validation.test.1 <- predict(cart.validation.training.1, 
                                       newdata=x[sampler=="test",], 
                                       type="class")

sum(predictions.validation.test.1 ==     
        ASSIGNMENT[sampler=="test"]) /   
    length(predictions.validation.test.1) 
# same data and pruned tree gives same result as original tree (0.8533333)
# so this tree doesn't give up performance when it becomes simpler
# this is good

#' #Conclusion
#' This dataset was effectively modeled by a CART (Classification and Regression Tree).  Both the original and the pruned model have high accuracy (.92 and .90 respectively) and do well (both have an accuracy of 0.8533333) when working with training and testing data subsets.  
#' The tree shows that students who worked less hours (specifically less than 23.4 hours) chose to do oral exams, whereas students who worked the most hours (specifically more than 35.05) chose to do theses, unless they were from the Central European or Middle Eastern regions.  Students who put in the middle range of workhours chose to do lab reports.  
#' Workhours seems to be a very influential predictor, as it shows up at three of the branches in the pruned tree.  The one other branch has region as a predictor.  Factors like gender and mistakes had, if any, negligible effect on predictions, as they do not show up in the tree.  
