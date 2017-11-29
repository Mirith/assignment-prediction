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

