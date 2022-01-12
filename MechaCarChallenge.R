# Deliverable 1

# 3. load dplyr 
library(dplyr)

# 4. read file
MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# 5. perform linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= MechaCar)

# 6. perform summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= MechaCar))

# Deliverable 2

# 2. read file
Suspension <- read.csv(file='Suspension_coil.csv',check.names=F,stringsAsFactors = F)

# 3. create total_summary
total_summary <- Suspension %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI)) 

# 4. create lot_summary
lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))

# Deliverable 3

# 1. Perform t-test across all Lots
t.test(Suspension$PSI,mu = 1500)
# 2.1 Perform t-test on Lot 1
t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# 2.2 Perform t-test on Lot 2
t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# 2.3 Perform t-test on Lot 3
t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
