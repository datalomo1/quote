library(reshape2)
library(data.table)
library(dplyr)
library(Hmisc) # for computing weighted median
library(foreach)

#
# data path
#
dataPath <- "../data"

## Changed the cutoff Quote to include all BUs

# 0. Part name contains dummy / not a valid part'
BAD_PART_NAMES = c("DUMMY PART", "NOT A VALID PART")
# 0. Quotes' amounts over 50M
CUTOFF1 <- 50000000
# 0. Quotes or sales with <=0 amounts, <=0 quantities
# 0. Quotes with per unit amounts over 100K
CUTOFF2 <- 100000 #100000 #
# 0. Quotes with per unit amounts over 1K AND quantities over 1K
CUTOFF3 <- 1000
CUTOFF4 <- 10000 #1000 #
# 1. Quotes with per unit amount over 50x the median sales per unit amount
CUTOFF5 <- 100
# 2. Quotes with per unit amount over 20x the median sales per unit amount where the latter is over 10 cents
CUTOFF6 <- 20
CUTOFF7 <- 0.1
# 3. Quotes with per unit amount over 5x the top sales percentiles (98%) except where the median sales per unit are less than 10 cents
CUTOFF8 <- 10 #10
# 4. Quotes with over 100x quantities of the median quantity sales
CUTOFF9 <- 200
# 5. Quotes with over 20x quantities of the top sales percentiles (98%)
CUTOFF10 <- 20
# 6. Quotes with per unit amounts 3x larger than the top 98 percentile sales amount
CUTOFF11 <- 10
# 7. Quotes with quantities over 10x the top percentiles (98%) quoted quantities
CUTOFF12 <- 10
# A. Quotes with per unit amount over 10x the median sales per unit amount
CUTOFF13 <- 10
# B. Quotes with per unit amount over 2.5x the top sales percentiles (98%)
CUTOFF14 <- 2.5
# C. Quotes with over 10x quantities of the top sales percentiles (98%)
CUTOFF15 <- 10
# D. Quotes with quantities over 5x the top percentiles (98%) quoted quantities
CUTOFF16 <- 5

print('Setup Complete...')
