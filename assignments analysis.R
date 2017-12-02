# first assignment, using assignments.csv

# information from .pdf about variables:

# <assignments.csv>
# ASSIGNMENT: the dependent variable, the final assignment chosen by a student:
    # oral_exam vs. lab_report vs. thesis
# SEX: the sex of the student: female vs. male
# REGION: the geographical region of origin of the student: 
    # central_european vs. hispanic vs. middle_eastern
# WORKHOURS: the number of hours students self-report they invested into the course
# MISTAKES: the number of mistakes in the last test before choosing the assignment

# question: which variables affect the assignment choice? 

rm(list=ls(all=TRUE)) # clear memory
setwd("~/LING 104/assignments 1")

# preliminary analysis

summary(x <- read.delim("assignments.csv"))

table(x$ASSIGNMENT, x$SEX)
# numbers look about even for lab_report
# but a lot more females chose to do a oral_exam than males
# and a lot more males chose to do a thesis than females
# statistically significant? 

# percentage of females v males doing an oral exam
prop.table(table(x$ASSIGNMENT, x$SEX)[2,])
# percentage of females v males doing a thesis
prop.table(table(x$ASSIGNMENT, x$SEX)[3,])

# both have the exact same numbers as above table because they all add up to 100...

# GGPLOT

# more analysis

table(x$ASSIGNMENT, x$REGION)
# each group has a spike in numbers in a separate format

# this is really verbose
# simplify output by hour range?
table(x$ASSIGNMENT, x$WORKHOURS)


table(x$ASSIGNMENT, x$MISTAKES)
# 0s appear in interesting places:
# lab_report has 0s on outer edges
# oral_exam has 0s toward the top
# thesis has 0s at the very start
# indicating a specific distribution of mistakes -- might still be coincidence
# how to test this?

# testing all factors at the same time... with glm or lm? dependent variable is categorical
# so use glm (lm is for numeric?)

attach(x)

# something about (asl;dkjsdflj; + as;dfjsalkd;fj + asldkfd)^3
# this isn't right... ignores the rest of data... how to start out with all data? 
summary(model.01 <- glm(ASSIGNMENT ~ SEX*REGION , data = x, family = binomial))


# from 104_04a_linreg.r

# model.01 <- lm(               # make model.01 a linear model modeling
# RT ~                        # RT as a function of
#    (FREQUENCY+FAMILIARITY+IMAGEABILITY+MEANINGFULNESS)^3, # all other vars & up to their 3-way interactions
# data=x[complete.cases(x),]) # using the complete cases of x
# summary(model.01)
# note the problem with the NAs: there is only 1 data point with FAMILIARITY:hi and IMAGEABILITY:lo

summary(model.01 <- glm(ASSIGNMENT ~ (SEX+REGION+ASSIGNMENT+WORKHOURS+MISTAKES)^5, 
        data = x[complete.cases(x),], family = binomial))

# did not converge -- is that okay? 
# use binomial or something else for dependent variable?
# ^5 or smaller number? 