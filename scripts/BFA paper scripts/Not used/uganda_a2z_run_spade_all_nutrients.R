# Running Spade
# Running SPADE: Uganda Harvest Plus data
# File created on 11/10/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc


# As of 11_13_20, still waiting on the age variable for the A2Z dataset

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "uganda_a"))

summary(uganda_a)

###########################################################
# 1. RUN SPADE FOR B12

# Variance is too low: work with the exceedingly high entries


# Let's have a look at the highest b12 intakes

summary(uganda_a)

# number of intakes per person:
table(table(uganda_a$id))
#  1   2 
# 60 324   This means 60 women with one observation and 324 with 2 observations

# Let's have a look at the highest b12 intakes
sort(uganda_a$b12, decreasing = T)[1:10]

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_a_pos <- uganda_a[uganda_a$b12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_a_pos$b12,  uganda_a_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_a_pos[uganda_a_pos$id == idid,])

summary(tapply(uganda_a_pos$b12,  uganda_a_pos$id, var))
# 
# # Remove first 1  
uganda_a_2 <- uganda_a
for( idid in names(res)[1:1])
  uganda_a_2 <- uganda_a_2[uganda_a_2$id != idid, ]

range(uganda_a$age)


# RUN FOR B12
f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), 
        data=uganda_a_2, seed=123,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_a_b12",
        spade.output.path = "output/b12_SPADE_uganda_a/")

##################################################################

# 2. RUN SPADE FOR IRON

summary(uganda_a)

# Within to between ratio is still high
# Let's have a look at the highest b12 intakes
sort(uganda_a$iron, decreasing = T)[1:10]

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_a_pos <- uganda_a[uganda_a$iron > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_a_pos$iron,  uganda_a_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_a_pos[uganda_a_pos$id == idid,])

summary(tapply(uganda_a_pos$iron,  uganda_a_pos$id, var))
# 

# # # Remove first 2  
uganda_a_2_iron <- uganda_a
for( idid in names(res)[1:2])
  uganda_a_2_iron <- uganda_a_2_iron[uganda_a_2_iron$id != idid, ]

# There are some extremely high iron intakes

uganda_a_2_iron <- subset(uganda_a_2_iron, iron<60)

# RUN FOR WOMEN
f.spade(frml.ia=iron~fp(age), frml.if="no.if", 
        data=uganda_a_2_iron, seed=123,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_a_iron",
        spade.output.path = "output/iron_SPADE_uganda_a/")

# within to between ratio is still high (5.77) but might be because of 
# small number of second day recalls

##################################################################

# 3. RUN SPADE FOR ZINC

summary(uganda_a)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=uganda_a, seed=123,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_a_zinc",
        spade.output.path = "output/zinc_SPADE_uganda_a/")

##################################################################

# 4. RUN SPADE FOR VIT A

# Some extremely high intakes
uganda_a_vita <- subset(uganda_a, vita<9000)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=uganda_a, seed=123,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_a_vita",
        spade.output.path = "output/vita_SPADE_uganda_a/")

