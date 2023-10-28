
library(parallel)

n_cores <- 22
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(ExtDist)
  library(SuppDists)
})



clusterExport(cl, c("simuOC", "prod_obs0", "key", "key1", "upper", "lower", "cont", "controlP", "test"))



#700

jj90_5 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 5, pred = 700, eff = 0.90))
jj90_10 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 10, pred = 700, eff = 0.90))
jj90_15 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 15, pred = 700, eff = 0.90))
jj90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 20, pred = 700, eff = 0.90))
jj90_25 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.90))
jj90_30 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 30, pred = 700, eff = 0.90))


jj99_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.99))
jj90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.90))
jj80_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.80))
jj70_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.70))
jj60_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.60))
jj50_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.50))
jj40_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.40))
jj30_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.30))
jj20_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.20))
jj10_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0.10))
jj0_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 700, eff = 0))



#600

jj600_90_5 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 5, pred = 600, eff = 0.90))
jj600_90_10 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 10, pred = 600, eff = 0.90))
jj600_90_15 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 15, pred = 600, eff = 0.90))
jj600_90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 20, pred = 600, eff = 0.90))
jj600_90_25 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.90))
jj600_90_30 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 30, pred = 600, eff = 0.90))


jj600_99_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.99))
jj600_90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.90))
jj600_80_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.80))
jj600_70_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.70))
jj600_60_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.60))
jj600_50_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.50))
jj600_40_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.40))
jj600_30_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.30))
jj600_20_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.20))
jj600_10_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0.10))
jj600_0_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 600, eff = 0))



#800

jj800_90_5 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 5, pred = 800, eff = 0.90))
jj800_90_10 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 10, pred = 800, eff = 0.90))
jj800_90_15 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 15, pred = 800, eff = 0.90))
jj800_90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 20, pred = 800, eff = 0.90))
jj800_90_25 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.90))
jj800_90_30 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 30, pred = 800, eff = 0.90))


jj800_99_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.99))
jj800_90_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.90))
jj800_80_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.80))
jj800_70_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.70))
jj800_60_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.60))
jj800_50_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.50))
jj800_40_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.40))
jj800_30_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.30))
jj800_20_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.20))
jj800_10_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0.10))
jj800_0_20 <- parSapply(cl, 1:1000, function(x) simuOC(den = 20, n = 25, pred = 800, eff = 0))






stopCluster(cl)



OC1 <- c(sum(jj90_5) / 1000,
        sum(jj90_10) / 1000,
        sum(jj90_15) / 1000,
        sum(jj90_20) / 1000,
        sum(jj90_25) / 1000,
        sum(jj90_30) / 1000)
plot(seq(5, 30, 5), OC1, type = "o", ylim = c(0, 1))
lines(seq(5, 30, 5), OC1_600, type = "o", col = "red")
lines(seq(5, 30, 5), OC1_800, type = "o", col = "blue")

OC <- c(sum(jj99_20) / 1000,
        sum(jj90_20) / 1000,
        sum(jj80_20) / 1000,
        sum(jj70_20) / 1000,
        sum(jj60_20) / 1000,
        sum(jj50_20) / 1000,
        sum(jj40_20) / 1000,
        sum(jj30_20) / 1000,
        sum(jj20_20) / 1000,
        sum(jj10_20) / 1000,
        sum(jj0_20) / 1000)
plot(seq(0, 100, 10), OC, type = "o", ylim = c(0, 1))
lines(seq(0, 100, 10), OC_600, type = "o", col = "red")
lines(seq(0, 100, 10), OC_800, type = "o", col = "blue")





OC1_600 <- c(sum(jj600_90_5) / 1000,
             sum(jj600_90_10) / 1000,
             sum(jj600_90_15) / 1000,
             sum(jj600_90_20) / 1000,
             sum(jj600_90_25) / 1000,
             sum(jj600_90_30) / 1000)
plot(seq(5, 30, 5), OC1_600, type = "o", ylim = c(0, 1))


OC_600 <- c(sum(jj600_99_20) / 1000,
            sum(jj600_90_20) / 1000,
            sum(jj600_80_20) / 1000,
            sum(jj600_70_20) / 1000,
            sum(jj600_60_20) / 1000,
            sum(jj600_50_20) / 1000,
            sum(jj600_40_20) / 1000,
            sum(jj600_30_20) / 1000,
            sum(jj600_20_20) / 1000,
            sum(jj600_10_20) / 1000,
            sum(jj600_0_20) / 1000)
plot(seq(0, 100, 10), OC_600, type = "o", ylim = c(0, 1))








OC1_800 <- c(sum(jj800_90_5) / 1000,
             sum(jj800_90_10) / 1000,
             sum(jj800_90_15) / 1000,
             sum(jj800_90_20) / 1000,
             sum(jj800_90_25) / 1000,
             sum(jj800_90_30) / 1000)
plot(seq(5, 30, 5), OC1_800, type = "o", ylim = c(0, 1))


OC_800 <- c(sum(jj800_99_20) / 1000,
            sum(jj800_90_20) / 1000,
            sum(jj800_80_20) / 1000,
            sum(jj800_70_20) / 1000,
            sum(jj800_60_20) / 1000,
            sum(jj800_50_20) / 1000,
            sum(jj800_40_20) / 1000,
            sum(jj800_30_20) / 1000,
            sum(jj800_20_20) / 1000,
            sum(jj800_10_20) / 1000,
            sum(jj800_0_20) / 1000)
plot(seq(0, 100, 10), OC_800, type = "o", ylim = c(0, 1))


