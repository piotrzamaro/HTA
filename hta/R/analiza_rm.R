analiza_rmean <- function(ipd)
{
  rmean <-rmst2(time = ipd$time, status = ipd$status, ipd$arm)
  tau <- rmean$tau

  rmst_table <- data.frame(rmean$RMST.arm0$rmst, rmean$RMST.arm1$rmst) %>%
  t()
colnames(rmst_table) <- c("RMST", "SE", "0.95LCI", "0.95UCI")
row.names(rmst_table) <- c("arm0", "arm1")

rmtl_table <- data.frame(rmean$RMST.arm0$rmtl, rmean$RMST.arm1$rmtl) %>%
  t()
colnames(rmtl_table) <- c("RMTL", "SE", "0.95LCI", "0.95UCI")
row.names(rmtl_table) <- c("arm0", "arm1")
rmean_table <- cbind(rmst_table, rmtl_table) %>%
  round(2)

diff_rmean_table <- rbind(
  rmst_table[2,]-rmst_table[1,],
  rmst_table[1,]-rmst_table[2,],
  rmst_table[2,]/rmst_table[1,],
  rmst_table[1,]/rmst_table[2,],
  rmtl_table[2,]/rmtl_table[1,],
  rmtl_table[1,]/rmtl_table[2,]) %>%
  round(2)%>%
  as.data.frame() %>%
  select(-SE)
colnames(diff_rmean_table) <- c("Time", "0.95LCI", "0.95UCI")
row.names(diff_rmean_table) <- c("RMST arm1-arm0",
                                 "RMST arm0-arm1",
                                 "RMST arm1/arm0",
                                 "RMST arm0/arm1",
                                 "RMTL arm1/arm0",
                                 "RMTL arm0/arm1")

rmean_plot <- plot(rmean,
     xlab="Czas w miesiącach",
     ylab="Prawdopodobieństwo przeżycia",
     col = "lightgrey",
     col.RMST = "darkslategray4",
     col.RMTL = "steelblue",
     density = 75,
     angle = 10)

return(list(
  rmst_table = rmst_table,
  rmtl_table = rmtl_table,
  rmean_table = rmean_table,
  diff_rmean_table = diff_rmean_table,
  rmean_plot = rmean_plot))
}
