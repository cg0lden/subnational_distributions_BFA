# re-install spade with new version from Arnold on 10/28/20

library(SPADE.RIVMNwCore)
# load(paste0("/Users/Simone/Dropbox/SPADE/BFA Analysis/data/zambia_wom.Rdata"))
load(paste0("C:/Users/sip996/Dropbox/SPADE/BFA Analysis/data/zambia_wom.Rdata"))
# re-install spade with new version from Arnold on 10/28/20

# load(paste0("/Users/Simone/Dropbox/SPADE/BFA Analysis/data/zambia_wom.Rdata"))

summary(zambia_wom)

#summary(zambia_wom)
#       id                 age             sex         mday      
# Min.   :1.032e+08   Min.   : 0.00   Min.   :2   Min.   :1.000  
# 1st Qu.:1.053e+08   1st Qu.:24.00   1st Qu.:2   1st Qu.:1.000  
# Median :2.014e+08   Median :28.00   Median :2   Median :1.000  
# Mean   :2.465e+08   Mean   :29.24   Mean   :2   Mean   :1.473  
# 3rd Qu.:2.056e+08   3rd Qu.:34.00   3rd Qu.:2   3rd Qu.:2.000  
# Max.   :2.081e+09   Max.   :67.00   Max.   :2   Max.   :2.000  
#      b12         
# Min.   :  0.000  
# 1st Qu.:  0.000  
# Median :  0.000  
# Mean   :  3.199  
# 3rd Qu.:  1.623  
# Max.   :150.060  
is.numeric(zambia_wom$id)
#[1] TRUE


zambia_wom$id <- cumsum(!duplicated(zambia_wom[1:2]))
is.integer(zambia_wom$id)
#[1] TRUE


summary(zambia_wom)

#summary(zambia_wom)
#       id                 age             sex         mday      
# Min.   :1.032e+08   Min.   : 0.00   Min.   :2   Min.   :1.000  
# 1st Qu.:1.053e+08   1st Qu.:24.00   1st Qu.:2   1st Qu.:1.000  
# Median :2.014e+08   Median :28.00   Median :2   Median :1.000  
# Mean   :2.465e+08   Mean   :29.24   Mean   :2   Mean   :1.473  
# 3rd Qu.:2.056e+08   3rd Qu.:34.00   3rd Qu.:2   3rd Qu.:2.000  
# Max.   :2.081e+09   Max.   :67.00   Max.   :2   Max.   :2.000  
#      b12         
# Min.   :  0.000  
# 1st Qu.:  0.000  
# Median :  0.000  
# Mean   :  3.199  
# 3rd Qu.:  1.623  
# Max.   :150.060  
is.numeric(zambia_wom$id)
#[1] TRUE
is.integer(zambia_wom$id)
#[1] TRUE


# Simone, this is what I get under Windows
# Maybe you can check what you get under Mac OS?
# Maybe the integers are too big ?

# You can try to recode it, maybe. I never heared before of your problem,
# neither from Windows users nor Mac users...

# number of intakes per person:
table(table(zambia_wom$id))
#  1   2 
# 60 324   This means 60 women with one observation and 324 with 2 observations

# Let's have a look at the highest b12 intakes
sort(zambia_wom$b12, decreasing = T)[1:10]
#  [1] 150.06  62.14  58.79  38.48  38.21  36.60  33.76  31.64  31.60  27.72

# Let's study the variances, BUT only the variances of the *positive* intakes, 
# since in the 2-part model, 
# for the amounts, only the positive intakes are used, and
# for the frequencies all intakes are used, recoded:  with zero for the zeroes
# and 1 for the positive intakes

# Take all the positive intakes
zambia_wom_pos <- zambia_wom[zambia_wom$b12 > 0, ]

# Calculate the variances per person 
# where NA means 1 observation, 0 mean the same observation, probably zero)
res <- sort(
  tapply(zambia_wom_pos$b12,  zambia_wom_pos$id, var),		decreasing = T)[1:10]

#206340523 203210123 205120083 203210443 204230413 103621363 104141073 208631243 
#1915.1861 1653.1250  684.5000  454.8128  454.5113  360.1928  336.4418  220.2901 
#203320403 203210083 
# 210.1250  155.5848 

# Let's see what the intakes are for these 10 persons
for (idid in names(res))
  print(zambia_wom_pos[zambia_wom_pos$id == idid,])
#           id age sex mday   b12
#568 206340523  24   2    1  0.25
#569 206340523  24   2    2 62.14
#           id age sex mday   b12
#423 203210123  29   2    1  1.29
#424 203210123  29   2    2 58.79
#           id age sex mday   b12
#509 205120083  23   2    1 38.21
#510 205120083  23   2    2  1.21
#           id age sex mday   b12
#440 203210443  27   2    1  1.44
#441 203210443  27   2    2 31.60
#           id age sex mday   b12
#483 204230413  30   2    1  1.49
#484 204230413  30   2    2 31.64
#          id age sex mday   b12
#77 103621363  26   2    1  0.16
#78 103621363  26   2    2 27.00
#           id age sex mday   b12
#101 104141073  35   2    1 26.88
#102 104141073  35   2    2  0.94
#           id age sex mday   b12
#671 208631243  32   2    1 21.57
#672 208631243  32   2    2  0.58
#           id age sex mday  b12
#448 203320403  29   2    1 21.0
#449 203320403  29   2    2  0.5
#           id age sex mday   b12
#421 203210083  50   2    1  9.24
#422 203210083  50   2    2 26.88
summary(tapply(zambia_wom_pos$b12,  zambia_wom_pos$id, var))
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#	0.0000   0.0000   0.1081  29.6365  13.8138 740.3552       60 

# If we remove only the first two, the between person variance is positive
# but still too low. So let's try to remove the three persons (completely, as
# a try) to see what happens

zambia_wom_2 <- zambia_wom
for( idid in names(res)[1:3])
  zambia_wom_2 <- zambia_wom_2[zambia_wom_2$id != idid, ]

f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), data=zambia_wom_2, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2)

#   within-individual variance  = 2.45
#   between-individual variance = 0.0511   <<<<<<<<<<<<<<<<<< still to low
#   ratio var.inner/var.between for intake amounts = 48.1  << still too high
#
#* * *  Statistics of observed and habitual intakes  * * *
#                             (weighted) sample mean intake = 3.005
#                          overall mean of habitual intakes = 3.362
#    difference of mean HI mean from (weighted) sample mean = 11.861 %

# Remove first 8 
zambia_wom_2 <- zambia_wom
for( idid in names(res)[1:8])
  zambia_wom_2 <- zambia_wom_2[zambia_wom_2$id != idid, ]

f.spade(frml.ia=b12~fp(age), frml.if=b12~cs(age), data=zambia_wom_2, seed=123,
        min.age=18,max.age=67,sex="female", dgts.distr = 2)

summary(zambia_wom$id)

# Last part of the output

#within-individual variance  = 2.38
#between-individual variance = 0.121
#ratio var.inner/var.between for intake amounts = 19.7  << should be < 4
#
#* * *  Statistics of observed and habitual intakes  * * *
#	    (weighted) sample mean intake = 2.926
#  overall mean of habitual intakes = 3.264
#difference of mean HI mean from (weighted) sample mean = 11.542 %  << high
#	
#	spade output table
#AM   P5  P10  P25  P50  P75  P90  P95
#(17,67] 3.26 0.41 0.72 1.57 2.92 4.55 6.24 7.36
#(17,18] 3.63 0.53 0.85 1.82 3.32 4.98 6.99 8.18
#(18,19] 3.69 0.61 0.99 1.95 3.32 5.06 6.68 7.79
#(19,20] 3.48 0.39 0.80 1.73 3.18 4.76 6.61 7.81
#(20,21] 3.28 0.38 0.70 1.38 2.91 4.65 6.43 7.56
#(21,22] 3.15 0.37 0.64 1.42 2.75 4.45 6.20 7.21
#(22,23] 3.12 0.33 0.66 1.36 2.75 4.37 6.10 7.32
#(23,24] 3.14 0.33 0.56 1.40 2.81 4.46 6.13 7.22
#(24,25] 3.05 0.30 0.58 1.35 2.58 4.33 6.08 7.22
#(25,26] 3.06 0.35 0.60 1.38 2.59 4.36 6.11 7.11
#(26,27] 3.20 0.35 0.66 1.55 2.87 4.44 6.33 7.24
#(27,28] 3.13 0.33 0.63 1.45 2.80 4.49 6.05 7.12
#(28,29] 3.24 0.41 0.75 1.54 2.85 4.57 6.16 7.22
#(29,30] 3.33 0.49 0.82 1.63 2.98 4.62 6.26 7.55
#(30,31] 3.30 0.35 0.69 1.59 2.96 4.61 6.33 7.42
#(31,32] 3.30 0.48 0.82 1.59 2.98 4.47 6.40 7.40
#(32,33] 3.38 0.61 0.92 1.73 3.05 4.69 6.30 7.19
#(33,34] 3.36 0.52 0.85 1.74 3.08 4.58 6.28 7.37
#(34,35] 3.34 0.57 0.92 1.77 3.08 4.53 6.13 7.02
#(35,36] 3.43 0.52 0.82 1.74 3.07 4.64 6.45 7.84
#(36,37] 3.42 0.39 0.81 1.71 2.98 4.91 6.47 7.43
#(37,38] 3.34 0.48 0.80 1.83 3.06 4.59 5.87 7.05
#(38,39] 3.40 0.50 0.86 1.78 3.03 4.55 6.25 7.34
#(39,40] 3.36 0.57 0.92 1.76 3.01 4.61 6.19 7.22
#(40,41] 3.47 0.40 0.81 1.90 3.04 4.92 6.33 7.59
#(41,42] 3.36 0.65 0.88 1.79 3.06 4.65 6.01 6.95
#(42,43] 3.59 0.45 0.80 2.03 3.34 4.70 6.44 7.59
#(43,44] 3.50 0.60 0.91 1.72 3.20 4.71 6.38 7.51
#(44,45] 3.68 0.66 1.06 2.19 3.30 4.94 6.67 7.90
#(45,46] 3.25 0.64 0.82 1.49 3.04 4.65 6.14 6.59
#(46,47] 3.66 0.86 1.23 2.24 3.41 4.80 6.17 7.13
#(47,48] 3.45 0.57 0.94 1.67 3.25 4.80 6.46 7.22
#(48,49] 3.30 0.86 1.23 1.95 2.97 4.31 5.95 6.87
#(49,50] 3.59 0.73 1.08 2.02 3.49 4.76 6.46 7.24
#(50,51] 3.23 0.57 0.82 1.96 3.21 4.21 5.82 6.48
#(52,53] 3.24 0.60 0.73 1.72 3.07 4.34 6.37 7.18
#(54,55] 2.70 0.29 0.54 1.21 2.55 4.06 5.08 5.94
#(55,56] 2.77 0.26 0.58 1.51 2.57 3.80 5.09 5.97
#(60,61] 1.91 0.15 0.32 0.69 1.81 2.65 4.51 4.79
#(66,67] 1.15 0.02 0.05 0.35 0.91 1.58 2.66 3.37


# You have to try a little bit. Maybe the high observations are too important
# to remove, so remove of the persons with a high variance only the lowest
# because then they are partecipating but not in the calculation of
# the variances (between en within person).

# Need to get ratio of var inner/outer down

zambia_wom_pos <- aggregate(zambia_wom_pos, by=id, FUN=diff
