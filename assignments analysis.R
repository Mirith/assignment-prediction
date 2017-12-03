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

attach(x)

# to make things easier to see in plots
colors <- c("blue", "dark green", "red")

barplot(table(MISTAKES))
# looks like a pretty normal distribution of mistakes

barplot(table(SEX))
# about half and half

barplot(table(ASSIGNMENT))
# equally divided

table(ASSIGNMENT, SEX)
# numbers look about even for lab_report
# but a lot more females chose to do a oral_exam than males
# and a lot more males chose to do a thesis than females
barplot(table(ASSIGNMENT, SEX), beside = TRUE, legend.text = TRUE, col = colors)
# easier to see it with a barplot
# but is that statistically significant? 

##################################
# math to test....
##################################

# GGPLOT

# more analysis

table(ASSIGNMENT, REGION)
# sort of unclear, lots of things going on
# plotting each region's assignment choice
barplot(prop.table(table(REGION, ASSIGNMENT)[1,]), 
        legend.text = TRUE, col = colors, main = "Central European") 
barplot(prop.table(table(REGION, ASSIGNMENT)[2,]), 
        legend.text = TRUE, col = colors, main = "Hispanic") 
barplot(prop.table(table(REGION, ASSIGNMENT)[3,]), 
        legend.text = TRUE, col = colors, main = "Middle Eastern") 
# each group has a spike in numbers in a separate format
# ie, oral exam for CE, thesis for H, and lab report for ME
# easy to see the large spike in plot, all together
barplot(table(ASSIGNMENT, REGION), beside = TRUE, legend.text = TRUE, col = colors)

table(ASSIGNMENT, MISTAKES)
# 0s appear in interesting places
# easier to see with a plot
barplot(table(ASSIGNMENT, MISTAKES), legend.text = TRUE, col = colors)
# oral exams were chosen by students who made fewer mistakes
# average amount of mistakes correlates to lab reports
# and those with lots of mistakes trended toward a thesis

##################################
# math to test....
##################################

# testing all factors at the same time... with glm or lm? dependent variable is categorical
# so use glm (lm is for numeric?)

# from 104_04a_linreg.r

# model.01 <- lm(               # make model.01 a linear model modeling
# RT ~                        # RT as a function of
#    (FREQUENCY+FAMILIARITY+IMAGEABILITY+MEANINGFULNESS)^3, # all other vars & up to their 3-way interactions
# data=x[complete.cases(x),]) # using the complete cases of x
# summary(model.01)
# note the problem with the NAs: there is only 1 data point with FAMILIARITY:hi and IMAGEABILITY:lo

summary(model.01 <- glm(ASSIGNMENT ~ (SEX+REGION+WORKHOURS+MISTAKES)^3, 
        data = x[complete.cases(x),], family = binomial))

# Call:
#     glm(formula = ASSIGNMENT ~ (SEX + REGION + WORKHOURS + MISTAKES)^3, 
#         family = binomial, data = x[complete.cases(x), ])
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.6501  -0.5035   0.1089   0.4261   3.2497  
# 
# Coefficients:
#                                           Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                              36.99706   16.74539   2.209   0.0271 *
#     SEXmale                              17.31933   21.02004   0.824   0.4100  
# REGIONhispanic                           -19.40765   20.16219  -0.963   0.3358  
# REGIONmiddle eastern                     12.90358   22.01289   0.586   0.5578  
# WORKHOURS                                -1.57113    0.66486  -2.363   0.0181 *
#     MISTAKES                             -1.42568    0.89516  -1.593   0.1112  
# SEXmale:REGIONhispanic                   -10.82643    9.60382  -1.127   0.2596  
# SEXmale:REGIONmiddle eastern             -19.35700    9.55904  -2.025   0.0429 *
#     SEXmale:WORKHOURS                    -0.12559    0.65917  -0.191   0.8489  
# SEXmale:MISTAKES                         -1.29266    1.11601  -1.158   0.2467  
# REGIONhispanic:WORKHOURS                 0.79511    0.76265   1.043   0.2972  
# REGIONmiddle eastern:WORKHOURS           -0.17868    0.80834  -0.221   0.8251  
# REGIONhispanic:MISTAKES                  0.69179    1.14286   0.605   0.5450  
# REGIONmiddle eastern:MISTAKES            -1.33500    1.22808  -1.087   0.2770  
# WORKHOURS:MISTAKES                       0.06255    0.03455   1.810   0.0703 .
# SEXmale:REGIONhispanic:WORKHOURS         -0.03684    0.19496  -0.189   0.8501  
# SEXmale:REGIONmiddle eastern:WORKHOURS   0.25559    0.25661   0.996   0.3192  
# SEXmale:REGIONhispanic:MISTAKES          0.50365    0.46748   1.077   0.2813  
# SEXmale:REGIONmiddle eastern:MISTAKES    0.42208    0.41576   1.015   0.3100  
# SEXmale:WORKHOURS:MISTAKES               0.02395    0.03448   0.695   0.4873  
# REGIONhispanic:WORKHOURS:MISTAKES        -0.02498    0.04151  -0.602   0.5474  
# REGIONmiddle eastern:WORKHOURS:MISTAKES  0.03179    0.04357   0.729   0.4657  
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 381.91  on 299  degrees of freedom
# Residual deviance: 187.37  on 278  degrees of freedom
# AIC: 231.37
# 
# Number of Fisher Scoring iterations: 8

# start dropping everything except the stuff with stars

# does order matter? 


