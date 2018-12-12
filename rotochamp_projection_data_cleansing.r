rm(list=ls(all=TRUE)) 
setwd('C:\\Users\\Pragan\\Dropbox\\bb\\')
#setwd('/Users/paulragan/Dropbox/bb')

library(XLConnect)
library(ggplot2)
library(dplyr)
library(stringr)
library(Rglpk)
#hitters <- readWorksheetFromFile("pecota_2017_03_18_76461.xlsx", sheet = 2) 
#pitchers <- readWorksheetFromFile("pecota_2017_03_18_76461.xlsx", sheet = 3) 
hitters <- read.csv("HitterProjections_2018_2.csv", header = TRUE)
pitchers <- read.csv("PitcherProjections_2018_2.csv", header = TRUE)

# ##################### #
# Set budget parameters #
# ##################### #
totalBudget <- 260
teams <- 14

keeperSpend <- 65
# spend % based on 2016 draft results
hitterSpendPercent <- .60
pitcherSpendPercent <- .40
myHitterSpend <- (totalBudget - keeperSpend) * hitterSpendPercent
myPitcherSpend <- (totalBudget - keeperSpend) * pitcherSpendPercent

startingHitters <- 11
startingPitchers <- 5
startingRelievers <- 2
totalStarters <- 18
totalBench <- 7
totalHitters <- 15
totalPitchers <- 10

# ######### #
# PREP DATA #
# ######### #

# #################################################### #
# hitters: C, 1B, 2B, 3B, SS, IF, CF, OF, OF, OF, UTIL #
# pitchers: SP, RP ################################### #
# #################################################### #

# Detect Positional Eligibility Using Regular Expressions
hitters$eligibility_C <- ifelse(str_detect(hitters$EligiblePositions, "(C\\b)"), 1, 0)
hitters$eligibility_1B <- ifelse(str_detect(hitters$EligiblePositions, "(1B\\b)"), 1, 0)
hitters$eligibility_2B <- ifelse(str_detect(hitters$EligiblePositions, "(2B\\b)"), 1, 0)
hitters$eligibility_3B <- ifelse(str_detect(hitters$EligiblePositions, "(3B\\b)"), 1, 0)
hitters$eligibility_SS <- ifelse(str_detect(hitters$EligiblePositions, "(SS\\b)"), 1, 0)
hitters$eligibility_IF <- ifelse(str_detect(hitters$EligiblePositions, "(1B\\b)"), 1, ifelse(str_detect(hitters$EligiblePositions, "(2B\\b)"), 1, ifelse(str_detect(hitters$EligiblePositions, "(3B\\b)"), 1, ifelse(str_detect(hitters$EligiblePositions, "(SS\\b)"), 1, 0))))
hitters$eligibility_CF <- ifelse(str_detect(hitters$EligiblePositions, "(CF\\b)"), 1, 0)
hitters$eligibility_OF <- ifelse(str_detect(hitters$EligiblePositions, "(OF\\b)"), 1, 0)
hitters$eligibility_UTIL <- 1

# net stolen bases
hitters <- as.data.frame(append(hitters, list(net_sb = (hitters$SB - hitters$CS)), after=18))


# ######################################### #
# Hitters Statistics: R	HR	RBI	BB	SBN	AVG #
# ######################################### #
hitters_min <- subset(hitters, AB >= 240)   # At least 250 ABs projected

hitters_min$rbi_rank <- ave(hitters_min$RBI, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min$bb_rank <- ave(hitters_min$BB, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min$hr_rank <- ave(hitters_min$HR, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min$sb_rank <- ave(hitters_min$net_sb, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min$r_rank <- ave(hitters_min$R, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min$avg_rank <- ave(hitters_min$AVG, FUN = function(x) rank(-x, ties.method = "average"))

# use combined ranks above to get overall rank
hitters_min$summed_rank <- (hitters_min$rbi_rank + hitters_min$bb_rank + hitters_min$hr_rank + hitters_min$sb_rank + hitters_min$r_rank + hitters_min$avg_rank)
hitters_min$combined_rank <- ave(hitters_min$summed_rank, FUN = function(x) rank(x, ties.method = "average"))
# head(hitters_min[order(hitters_min$combined_rank),])

# ############### #
# positional_rank #
# ############### #
hitters_min <- transform(hitters_min, positional_rank = ave(hitters_min$combined_rank, hitters_min$eligiblePosition, FUN = function(x) rank(x, ties.method = "first")))

# hitter standard deviations
hitters_min$rbi_stdev <- (hitters_min$RBI - mean(hitters_min$RBI))/sd(hitters_min$RBI)
hitters_min$bb_stdev <- (hitters_min$BB - mean(hitters_min$BB))/sd(hitters_min$BB)
hitters_min$hr_stdev <- (hitters_min$HR - mean(hitters_min$HR))/sd(hitters_min$HR)
hitters_min$sb_stdev <- (hitters_min$net_sb - mean(hitters_min$net_sb))/sd(hitters_min$net_sb)
hitters_min$r_stdev <- (hitters_min$R - mean(hitters_min$R))/sd(hitters_min$R)
hitters_min$avg_stdev <- (hitters_min$AVG - mean(hitters_min$AVG))/sd(hitters_min$AVG)

hitters_min$combined_stdev <- (hitters_min$rbi_stdev + hitters_min$bb_stdev + hitters_min$hr_stdev + hitters_min$sb_stdev + hitters_min$r_stdev + hitters_min$avg_stdev)

hitters_min$stdev_rank <- ave(hitters_min$combined_stdev, FUN = function(x) rank(-x, ties.method = "average"))
hitters_min <- transform(hitters_min, positional_stdev_rank = ave(hitters_min$stdev_rank, hitters_min$eligiblePosition, FUN = function(x) rank(x, ties.method = "first")))

# ORDER
hitters_min <- hitters_min[order(hitters_min$eligiblePosition,hitters_min$positional_stdev_rank),]

# Run Standard deviations again using averages among positions
hitters_min$likely_drafted <- 0
hitters_min$likely_drafted <- ifelse((hitters_min$positional_stdev_rank <= 65 & hitters_min$eligibility_OF == 1) |
                                       (hitters_min$positional_stdev_rank <= 20 & hitters_min$eligibility_1B == 1) |
                                       (hitters_min$positional_stdev_rank <= 20 & hitters_min$eligibility_2B == 1) |
                                       (hitters_min$positional_stdev_rank <= 20 & hitters_min$eligibility_3B == 1) |
                                       (hitters_min$positional_stdev_rank <= 20 & hitters_min$eligibility_SS == 1) |
                                       (hitters_min$positional_stdev_rank <= 16 & hitters_min$eligibility_C == 1), 1,0)

hitters_draftable <- subset(hitters_min, likely_drafted >= 1)

hitters_draftable$rbi_stdev2 <- (hitters_draftable$RBI - mean(hitters_draftable$RBI))/sd(hitters_draftable$RBI)
hitters_draftable$bb_stdev2 <- (hitters_draftable$BB - mean(hitters_draftable$BB))/sd(hitters_draftable$BB)
hitters_draftable$hr_stdev2 <- (hitters_draftable$HR - mean(hitters_draftable$HR))/sd(hitters_draftable$HR)
hitters_draftable$sb_stdev2 <- (hitters_draftable$net_sb - mean(hitters_draftable$net_sb))/sd(hitters_draftable$net_sb)
hitters_draftable$r_stdev2 <- (hitters_draftable$R - mean(hitters_draftable$R))/sd(hitters_draftable$R)
hitters_draftable$avg_stdev2 <- (hitters_draftable$AVG - mean(hitters_draftable$AVG))/sd(hitters_draftable$AVG)

hitters_draftable$combined_stdev2 <- (hitters_draftable$rbi_stdev2 + hitters_draftable$bb_stdev2 + hitters_draftable$hr_stdev2 + hitters_draftable$sb_stdev2 + hitters_draftable$r_stdev2 + hitters_draftable$avg_stdev2)




write.csv(hitters_min, file="rotochamp_hitters.csv", row.names = FALSE)
write.csv(hitters_draftable, file="hitters_draftable.csv", row.names = FALSE)

# kmeans(hitters_min$rbi_stdev, 6, nstart=20)
# ggplot(hitters_min, aes(hitters_min$avg_stdev, hitters_min$r_stdev, color = hitters_min$EligiblePositions)) + geom_point()
# 
# set.seed(20)
# hitters_cluster_RBI <- kmeans(hitters_min$RBI, 3, nstart=20)
# table(hitters_cluster_RBI$cluster, hitters_min$POS)
# hitters_cluster_RBI$cluster <- as.factor(hitters_cluster_RBI$cluster)
# ggplot(hitters_min, aes(hitters_min$RBI, hitters_min$AB, color = hitters_cluster_RBI$cluster)) + geom_point()

# dropoff to next ranked in position
# assign "points" 

# ########################################## #
# Pitchers STatistics: HR	K	QS	SV	ERA	WHIP #
# ########################################## #
pitchers$EligiblePositions <- ifelse(pitchers$QS >= 5, "SP", "RP")

pitchers$eligibility_SP <- ifelse(pitchers$QS >= 5, 1, 0)
pitchers$eligibility_RP <- ifelse(pitchers$QS < 5, 1, 0)

pitchers_min <- subset(pitchers, IP > 60 | SV >= 25)

#per inning stats
pitchers_min$hr_per_ip <- pitchers_min$HR/pitchers_min$IP
pitchers_min$k_per_ip <- pitchers_min$SO/pitchers_min$IP

#pitching ranks
pitchers_min$hr_ip_rank <- ave(pitchers_min$hr_per_ip, FUN = function(x) rank(x,ties.method = "average")) #rank(x) to sort in reverse order
pitchers_min$k_ip_rank <- ave(pitchers_min$k_per_ip, FUN = function(x) rank(-x,ties.method = "average"))
pitchers_min$qs_rank <- ave(pitchers_min$QS, FUN = function(x) rank(-x,ties.method = "average"))
pitchers_min$sv_rank <- ave(pitchers_min$SV, FUN = function(x) rank(-x,ties.method = "average"))
pitchers_min$era_rank <- ave(pitchers_min$ERA, FUN = function(x) rank(x,ties.method = "average")) #rank(x) to sort in reverse order
pitchers_min$whip_rank <- ave(pitchers_min$WHIP, FUN = function(x) rank(x,ties.method = "average")) #rank(x) to sort in reverse order
pitchers_min$ip_rank <- ave(pitchers_min$IP, FUN = function(x) rank(-x,ties.method = "average"))

#standard deviation
pitchers_min$k_ip_stdev <- (pitchers_min$k_per_ip - mean(pitchers_min$k_per_ip))/sd(pitchers_min$k_per_ip)
pitchers_min$hr_ip_stdev <- (pitchers_min$hr_per_ip - mean(pitchers_min$hr_per_ip))/sd(pitchers_min$hr_per_ip)
pitchers_min$qs_stdev <- (pitchers_min$QS - mean(pitchers_min$QS))/sd(pitchers_min$QS)
pitchers_min$sv_stdev <- (pitchers_min$SV - mean(pitchers_min$SV))/sd(pitchers_min$SV)
pitchers_min$era_stdev <- (pitchers_min$ERA - mean(pitchers_min$ERA))/sd(pitchers_min$ERA)
pitchers_min$whip_stdev <- (pitchers_min$WHIP - mean(pitchers_min$WHIP))/sd(pitchers_min$WHIP)
pitchers_min$ip_stdev <- (pitchers_min$IP - mean(pitchers_min$IP))/sd(pitchers_min$IP)

# sum stdev
pitchers_min$combined_stdev <- ifelse(pitchers_min$EligiblePositions == "SP", (pitchers_min$k_ip_stdev + (-1*pitchers_min$hr_ip_stdev) + pitchers_min$qs_stdev + (-1*pitchers_min$era_stdev) + (-1*pitchers_min$whip_stdev) + pitchers_min$ip_stdev),
                                         (pitchers_min$k_ip_stdev + (-1*pitchers_min$hr_ip_stdev) + pitchers_min$sv_stdev + (-1*pitchers_min$era_stdev) + (-1*pitchers_min$whip_stdev) + pitchers_min$ip_stdev))

# overall rank and positional ranking
pitchers_min$stdev_rank <- ave(pitchers_min$combined_stdev, FUN = function(x) rank(-x, ties.method = "average"))
pitchers_min <- transform(pitchers_min, positional_stdev_rank = ave(pitchers_min$stdev_rank, pitchers_min$EligiblePositions, FUN = function(x) rank(x, ties.method = "first")))

# order
pitchers_min <- pitchers_min[order(pitchers_min$eligibility_SP,pitchers_min$positional_stdev_rank),]

# export
write.csv(pitchers_min, file="pitchers.csv", row.names = FALSE)


#clustering
#kmeans(pitchers_min$k_ip.stdev, 6, nstart=20)



