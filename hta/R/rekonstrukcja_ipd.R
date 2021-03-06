rekonstrukcja_ipd <- function(getpoints_arm0, getpoints_arm1, time_risk, nrisk_arm0, interval, quantile, arms)
{
  library(IPDfromKM)
  library(dplyr)


  dir.create("rekonstrukcja_ipd")

  getpoints_arm0 <- getpoints("image.png",
                              x1 = min_x,
                              x2 = max_x,
                              y1 = min_y,
                              y2 = max_y)
  getpoints_arm1 <- getpoints("image.png",
                              x1 = min_x,
                              x2 = max_x,
                              y1 = min_y,
                              y2 = max_y)

  getpoints <- rbind(
    data.frame(getpoints_arm0, "arm" = "0"),
    data.frame(getpoints_arm1, "arm" = "1"))
  write.csv2(getpoints,"getpoints.csv", row.names = F)

  preprocess_arm0  =  preprocess(getpoints_arm0,
                                 trisk = time_risk,
                                 nrisk = nrisk_arm0,
                                 totalpts = nrisk_arm0[1],
                                 maxy = max_y)
  preprocess_arm1 = preprocess(getpoints_arm1,
                               trisk = time_risk,
                               nrisk = nrisk_arm1,
                               totalpts = nrisk_arm1[1],
                               maxy = max_y)
  preprocessdat <- rbind(
    data.frame(preprocess_arm0$preprocessdat, "arm" = "0"),
    data.frame(preprocess_arm1$preprocessdat, "arm" = "1"))
  write.csv2(preprocessdat,"preprocessdat.csv", row.names = F)

  intervalIndex <-
    rbind(
      data.frame(preprocess_arm0$intervalIndex, "arm" = "0"),
      data.frame(preprocess_arm1$intervalIndex, "arm" = "1"))
  write.csv2(intervalIndex,"intervalIndex.csv", row.names = F)

  inputdat <- rbind(
    data.frame(preprocess_arm0$inputdat, "arm" = "0"),
    data.frame(preprocess_arm1$inputdat, "arm" = "1"))
  write.csv2(inputdat,"inputdat.csv", row.names = F)

  getIPD_arm0 <-
    getIPD(preprocess_arm0,
           armID = 1)
  getIPD_arm1 <-
    getIPD(preprocess_arm1,
           armID = 2)

  ipd <- rbind(
    data.frame(getIPD_arm0$IPD, "arm" = "0", "group" = label_arm0),
    data.frame(getIPD_arm1$IPD, "arm" = "1", "group" = label_arm1))
  write.csv2(ipd, "IPD.csv", row.names = F)

  points <- rbind(
    data.frame(getIPD_arm0$Points, "group" = label_arm0),
    data.frame(getIPD_arm1$Points, "group" = label_arm1))
  write.csv2(points,"points.csv", row.names = F)

  riskmat <- rbind(
    data.frame(getIPD_arm0$riskmat, "arm" = "0", "group" = label_arm0),
    data.frame(getIPD_arm1$riskmat, "arm" = "1", "group" = label_arm1))
  write.csv2(riskmat,"riskmat.csv", row.names = F)

  ks_test <- cbind(
    data.frame("arm0" = getIPD_arm0$kstest),
    data.frame("arm1" = getIPD_arm1$kstest))

  precision <- cbind(
    data.frame("arm0" = getIPD_arm0$precision),
    data.frame("arm1" = getIPD_arm1$precision))

  estim_table <-
    rbind(ks_test, precision) %>%
    t()
  write.csv2(estim_table,"estim_table.csv", row.names = F)


  survreport = survreport(getIPD_arm0$IPD,
                          getIPD_arm1$IPD,
                          arms = arms,
                          interval = interval,
                          s = quantile,
                          showplot = F)
  survprob <- cbind(
    data.frame(survreport$arm1$survtime, "arm" = "0","group" = label_arm0 , check.names = F),
    data.frame(survreport$arm2$survtime, "arm" = "1","group" = label_arm1, check.names = F))
  write.csv2(survtime,"survtime.csv", row.names = F)

  survtime <- cbind(
    data.frame("arm" = "0", "group" = label_arm0, survreport$arm1$survprob, fix.empty.names = T),
    data.frame("arm" = "1", "group" = label_arm1, survreport$arm2$survprob, fix.empty.names = T))
  write.csv2(survprob,"survprob.csv", row.names = F)

  return(list(
    getpoints = getpoints,
    preprocessdat = preprocessdat,
    intervalIndex = intervalIndex,
    inputdat = inputdat,
    ipd = ipd,
    points = points,
    riskmat = riskmat,
    estim_table = estim_table,
    plot_estim0 = plot(getIPD_arm0),
    plot_estim1 = plot(getIPD_arm1),
    survtime = survtime,
    survtime = survtime))
}


