
source('functions.R')

library(MASS)
library(plyr)
library(numbers)
options(digits=8)


########################################################################
## set N
########################################################################

setN <- c(100,200, 350, 500)

########################################################################
## nominal level
########################################################################
## equal hypothesis
PI <- c(0.1,0.3,0.5)
XI1 <- c(0.5,0.75,0.85)
XI2 <- c(0.5,0.75,0.85)
XI <- cbind(XI1, XI2)
PPV1 <- c(0.65,0.75,0.85)
PPV2 <- c(0.65,0.75,0.85)
PPV <- cbind(PPV1, PPV2)

prob <- wrap_prob_generate(PI, XI, PPV, equal_hypothesis = TRUE)
result <- wrap_simulation("nominal", equal_hypothesis = TRUE, setN, prob)
write.csv(result, "./data/ppv_nominal_equal_1019.csv")

## not equal hypothesis
PI <- c(0.1,0.3,0.5)
XI1 <- c(0.5,0.75,0.85,0.5,0.7)
XI2 <- c(0.6,0.85,0.95,0.7,0.95)
XI <- cbind(XI1, XI2)
PPV1 <- c(0.65,0.75,0.85)
PPV2 <- c(0.65,0.75,0.85)
PPV <- cbind(PPV1, PPV2)

prob <- wrap_prob_generate(PI, XI, PPV, equal_hypothesis = FALSE)
result <- wrap_simulation("nominal", equal_hypothesis = FALSE, setN, prob)
write.csv(result, "./data/ppv_nominal_nonequal_1019.csv")
View(result)

########################################################################
## power
########################################################################
## equal hypothesis
PI <- c(0.1,0.3,0.5)
XI1 <- c(0.5,0.75,0.85)
XI2 <- c(0.5,0.75,0.85)
XI <- cbind(XI1, XI2)
PPV1 <- c(0.65,0.75,0.85,0.65,0.75)
PPV2 <- c(0.75,0.85,0.95,0.85,0.95)
PPV <- cbind(PPV1, PPV2)

prob <- wrap_prob_generate(PI, XI, PPV, equal_hypothesis = TRUE)
result <- wrap_simulation("power", equal_hypothesis = TRUE, setN, prob)
write.csv(result, "./data/ppv_power_equal_1019.csv")

## not equal hypothesis
PI <- c(0.1,0.3,0.5)
XI1 <- c(0.5,0.75,0.85,0.5,0.7,0.6,0.85,0.95,0.7,0.95)
XI2 <- c(0.6,0.85,0.95,0.7,0.95,0.5,0.75,0.85,0.5,0.7)
XI <- cbind(XI1, XI2)
PPV1 <- c(0.65,0.75,0.85,0.65,0.75)
PPV2 <- c(0.75,0.85,0.95,0.85,0.95)
PPV <- cbind(PPV1, PPV2)

prob <- wrap_prob_generate(PI, XI, PPV, equal_hypothesis = FALSE)
result <- wrap_simulation("power", equal_hypothesis = FALSE, setN, prob)
write.csv(result, "./data/ppv_power_nonequal_1019.csv")
