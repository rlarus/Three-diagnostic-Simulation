
ppv_power_equal <- read.csv("./data/ppv_power_equal_1019.csv")
ppv_power_nonequal <- read.csv("./data/ppv_power_nonequal_1019.csv")

# case 1
case1 <- ppv_power_equal[ppv_power_equal$pi == 0.3 & ppv_power_equal$ppv1 == 0.65 & ppv_power_equal$ppv2 == 0.85,]

png('./plot/case1.png',600,400)
plot(N200~xi1, data=case1, xlab=expression(xi), ylab="power", type='b', ylim=range(0,1), xlim=range(0.5,0.9))
lines(N350~xi1, data=case1, type='b')
lines(N500~xi1, data=case1, type='b')
text(0.87, 1, "n=500")
text(0.87, 0.94, "n=350")
text(0.87, 0.7, "n=200")
dev.off(3)

# case 2
png('./plot/case2.png',600,400)
case2_1 <- ppv_power_equal[ppv_power_equal$pi == 0.3 & ppv_power_equal$ppv1 == 0.65 & ppv_power_equal$ppv2 == 0.75,]
case2_2 <- ppv_power_equal[ppv_power_equal$pi == 0.3 & ppv_power_equal$ppv1 == 0.65 & ppv_power_equal$ppv2 == 0.85,]
case2_3 <- ppv_power_equal[ppv_power_equal$pi == 0.3 & ppv_power_equal$ppv1 == 0.75 & ppv_power_equal$ppv2 == 0.95,]
plot(N500~xi1, data=case2_1, xlab=expression(xi), ylab="power", type='b', ylim=range(0,1), xlim=range(0.5,0.9))
lines(N500~xi1, data=case2_2, type='b')
lines(N500~xi1, data=case2_3, type='b')
text(0.6, 0.94, "PPV1=0.65, PPV2=0.75")
text(0.6, 0.65, "PPV1=0.65, PPV2=0.85")
text(0.6, 0.27, "PPV1=0.75, PPV2=0.95")
dev.off(3)

# case 3
#ppv_power_nonequal[ppv_power_nonequal$pi == 0.3 & ppv_power_nonequal$ppv1 == 0.65 & ppv_power_nonequal$ppv2 == 0.85,]

case3_1 <- ppv_power_nonequal[ppv_power_nonequal$pi == 0.3 & ppv_power_nonequal$ppv1 == 0.65 & ppv_power_nonequal$ppv2 == 0.85 & ppv_power_nonequal$xi1 == 0.5,]
case3_2 <- ppv_power_nonequal[ppv_power_nonequal$pi == 0.3 & ppv_power_nonequal$ppv1 == 0.65 & ppv_power_nonequal$ppv2 == 0.85 & ppv_power_nonequal$xi1 == 0.75,]
case3_3 <- ppv_power_nonequal[ppv_power_nonequal$pi == 0.3 & ppv_power_nonequal$ppv1 == 0.65 & ppv_power_nonequal$ppv2 == 0.85 & ppv_power_nonequal$xi1 == 0.85,]
plot(N500~xi2, data=case3_1, xlab=expression(xi), ylab="power", type='b', ylim=range(0,1), xlim=range(0.5,1))
lines(N500~xi2, data=case3_2, type='b')
lines(N500~xi2, data=case3_3, type='b')
