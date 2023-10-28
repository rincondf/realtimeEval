library(parallel)

n_cores <- 22
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(ExtDist)
  library(SuppDists)
})



clusterExport(cl, c("simuOC2", "prod_obs0", "key", "key1", "upper", "lower", "cont", "controlP2", "test2"))



#700

jj2_2_90_5 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 5, pred = 700, eff = 0.90))
jj2_2_90_10 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 10, pred = 700, eff = 0.90))
jj2_2_90_15 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 15, pred = 700, eff = 0.90))
jj2_2_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 20, pred = 700, eff = 0.90))
jj2_2_90_25 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.90))
jj2_2_90_30 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 30, pred = 700, eff = 0.90))


jj2_2_99_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.99))
jj2_2_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.90))
jj2_2_80_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.80))
jj2_2_70_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.70))
jj2_60_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.60))
jj2_50_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.50))
jj2_40_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.40))
jj2_30_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.30))
jj2_20_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.20))
jj2_10_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0.10))
jj2_0_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 700, eff = 0))



#600

jj2_600_90_5 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 5, pred = 600, eff = 0.90))
jj2_600_90_10 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 10, pred = 600, eff = 0.90))
jj2_600_90_15 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 15, pred = 600, eff = 0.90))
jj2_600_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 20, pred = 600, eff = 0.90))
jj2_600_90_25 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.90))
jj2_600_90_30 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 30, pred = 600, eff = 0.90))


jj2_600_99_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.99))
jj2_600_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.90))
jj2_600_80_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.80))
jj2_600_70_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.70))
jj2_600_60_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.60))
jj2_600_50_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.50))
jj2_600_40_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.40))
jj2_600_30_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.30))
jj2_600_20_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.20))
jj2_600_10_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0.10))
jj2_600_0_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 600, eff = 0))



#800

jj2_800_90_5 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 5, pred = 800, eff = 0.90))
jj2_800_90_10 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 10, pred = 800, eff = 0.90))
jj2_800_90_15 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 15, pred = 800, eff = 0.90))
jj2_800_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 20, pred = 800, eff = 0.90))
jj2_800_90_25 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.90))
jj2_800_90_30 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 30, pred = 800, eff = 0.90))


jj2_800_99_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.99))
jj2_800_90_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.90))
jj2_800_80_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.80))
jj2_800_70_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.70))
jj2_800_60_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.60))
jj2_800_50_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.50))
jj2_800_40_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.40))
jj2_800_30_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.30))
jj2_800_20_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.20))
jj2_800_10_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0.10))
jj2_800_0_20 <- parSapply(cl, 1:1000, function(x) simuOC2(den = 20, n = 25, pred = 800, eff = 0))






stopCluster(cl)


OC12 <- c(sum(jj2_2_90_5) / 1000,
         sum(jj2_2_90_10) / 1000,
         sum(jj2_2_90_15) / 1000,
         sum(jj2_2_90_20) / 1000,
         sum(jj2_2_90_25) / 1000,
         sum(jj2_2_90_30) / 1000)
plot(seq(5, 30, 5), OC12, type = "o", ylim = c(0, 1))
lines(seq(5, 30, 5), OC1_6002, type = "o", col = "red")
lines(seq(5, 30, 5), OC1_8002, type = "o", col = "blue")

OC2 <- c(sum(jj2_2_99_20) / 1000,
        sum(jj2_2_90_20) / 1000,
        sum(jj2_2_80_20) / 1000,
        sum(jj2_2_70_20) / 1000,
        sum(jj2_60_20) / 1000,
        sum(jj2_50_20) / 1000,
        sum(jj2_40_20) / 1000,
        sum(jj2_30_20) / 1000,
        sum(jj2_20_20) / 1000,
        sum(jj2_10_20) / 1000,
        sum(jj2_0_20) / 1000)
plot(seq(0, 100, 10), OC2, type = "o", ylim = c(0, 1))
lines(seq(0, 100, 10), OC_6002, type = "o", col = "red")
lines(seq(0, 100, 10), OC_8002, type = "o", col = "blue")









OC1_6002 <- c(sum(jj2_600_90_5) / 1000,
             sum(jj2_600_90_10) / 1000,
             sum(jj2_600_90_15) / 1000,
             sum(jj2_600_90_20) / 1000,
             sum(jj2_600_90_25) / 1000,
             sum(jj2_600_90_30) / 1000)
plot(seq(5, 30, 5), OC1_6002, type = "o", ylim = c(0, 1))


OC_6002 <- c(sum(jj2_600_99_20) / 1000,
            sum(jj2_600_90_20) / 1000,
            sum(jj2_600_80_20) / 1000,
            sum(jj2_600_70_20) / 1000,
            sum(jj2_600_60_20) / 1000,
            sum(jj2_600_50_20) / 1000,
            sum(jj2_600_40_20) / 1000,
            sum(jj2_600_30_20) / 1000,
            sum(jj2_600_20_20) / 1000,
            sum(jj2_600_10_20) / 1000,
            sum(jj2_600_0_20) / 1000)
plot(seq(0, 100, 10), OC_6002, type = "o", ylim = c(0, 1))








OC1_8002 <- c(sum(jj2_800_90_5) / 1000,
             sum(jj2_800_90_10) / 1000,
             sum(jj2_800_90_15) / 1000,
             sum(jj2_800_90_20) / 1000,
             sum(jj2_800_90_25) / 1000,
             sum(jj2_800_90_30) / 1000)
plot(seq(5, 30, 5), OC1_8002, type = "o", ylim = c(0, 1))


OC_8002 <- c(sum(jj2_800_99_20) / 1000,
            sum(jj2_800_90_20) / 1000,
            sum(jj2_800_80_20) / 1000,
            sum(jj2_800_70_20) / 1000,
            sum(jj2_800_60_20) / 1000,
            sum(jj2_800_50_20) / 1000,
            sum(jj2_800_40_20) / 1000,
            sum(jj2_800_30_20) / 1000,
            sum(jj2_800_20_20) / 1000,
            sum(jj2_800_10_20) / 1000,
            sum(jj2_800_0_20) / 1000)
plot(seq(0, 100, 10), OC_8002, type = "o", ylim = c(0, 1))

