# Running Spade
# Running SPADE: Uganda Harvest Plus data
# File created on 11/10/20 by Simone Passarelli
# All nutrients: b12, iron, vita, zinc

# Load packages
library(SPADE.RIVMNwCore)
library(here)

# DO NOT load tidyverse because it makes spade mad!
load(here("data", "processed", "uganda_h"))

summary(uganda_h)

###########################################################
# 1. RUN SPADE FOR B12

# Variance is too low: work with the exceedingly high entries


# Let's have a look at the highest b12 intakes

summary(uganda_h)

# number of intakes per person:
table(table(uganda_h$id))
#  1   2 
# 60 324   This means 60 women with one observation and 324 with 2 observations

# Let's have a look at the highest b12 intakes
sort(uganda_h$b12, decreasing = T)[1:10]

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_h_pos <- uganda_h[uganda_h$b12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_h_pos$b12,  uganda_h_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_h_pos[uganda_h_pos$id == idid,])

summary(tapply(uganda_h_pos$b12,  uganda_h_pos$id, var))
# 
# # Remove first 1  
 uganda_h_2 <- uganda_h
 for( idid in names(res)[1:1])
   uganda_h_2 <- uganda_h_2[uganda_h_2$id != idid, ]

range(uganda_h$age)


# RUN FOR B12
f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), 
        data=uganda_h_2, seed=123,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_h_b12",
        spade.output.path = "output/b12_SPADE_uganda_h/")

##################################################################

# 2. RUN SPADE FOR IRON

summary(uganda_h)

# Within to between ratio is still high
# Let's have a look at the highest b12 intakes
sort(uganda_h$iron, decreasing = T)[1:10]

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
uganda_h_pos <- uganda_h[uganda_h$iron > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(uganda_h_pos$iron,  uganda_h_pos$id, var),		decreasing = T)[1:10]


# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(uganda_h_pos[uganda_h_pos$id == idid,])

summary(tapply(uganda_h_pos$iron,  uganda_h_pos$id, var))
# 

# # # Remove first 2  
uganda_h_2_iron <- uganda_h
 for( idid in names(res)[1:2])
   uganda_h_2_iron <- uganda_h_2_iron[uganda_h_2_iron$id != idid, ]

# There are some extremely high iron intakes

uganda_h_2_iron <- subset(uganda_h_2_iron, iron<60)

# RUN FOR WOMEN
f.spade(frml.ia=iron~fp(age),  frml.if="no.if", 
        data=uganda_h_2_iron, seed=123,
        min.age=20,max.age=73,sex="female", dgts.distr = 2,  
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_h_iron",
        spade.output.path = "output/iron_SPADE_uganda_h/")

# within to between ratio is still high (5.77) but might be because of 
# small number of second day recalls

##################################################################

# 3. RUN SPADE FOR ZINC

summary(uganda_h)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=zinc~fp(age), frml.if="no.if", 
        data=uganda_h, seed=123,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_h_zinc",
        spade.output.path = "output/zinc_SPADE_uganda_h/")

##################################################################

# 4. RUN SPADE FOR VIT A

# Some extremely high intakes
uganda_h_vita <- subset(uganda_h, vita<9000)

# only positive intake values for iron so use 1 part model
f.spade(frml.ia=vita~fp(age), frml.if="no.if", 
        data=uganda_h, seed=123,
        min.age=20,max.age=67,sex="female", dgts.distr = 2, 
        age.classes=c(20, 25, 29, 34, 39, 44, 49, 54, 59, 64, 73),
        output.name = "uganda_h_vita",
        spade.output.path = "output/vita_SPADE_uganda_h/")

