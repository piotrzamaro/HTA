analiza_km <- function(ipd)
{

  library(IPDfromKM)
  library(dplyr)
  library(survminer)
  library(survRM2)
  library(survHE)
  library(utile.tables)
  library(rmarkdown)

  time = ipd$time
  status = ipd$status
  group.by = ipd$treat
  arm = ipd$arm
  data = ipd
  formula = Surv(time, status) ~ arm


  fit_km <- survfit(Surv(time, status) ~ arm, data = ipd)
  fit_model <- unclass(summary(survfit(formula)))
  km_table <- round(fit_model$table,2)
  km_table <- data.frame("group" = c("arm=0", "arm=1"),round(fit_model$table,2), check.names = F)
  rownames(km_table) <- NULL

  write.csv2(km_table, "km_table.csv", row.names = F)

  km_plot <- ggsurvplot(c,
                        data = ipd,
                        pval = TRUE,
                        pval.method = T,
                        conf.int = TRUE,
                        conf.int.style = "ribbon",
                        xlab = "Time",
                        break.time.by = 5,
                        ggtheme = theme_pubclean(),
                        surv.median.line = "hv",
                        palette = c("darkslategray4","orange4"),
                        xlim = c(min(time_risk), max(time_risk)))



  logrank_km <- pairwise_survdiff(formula, data = ipd)$p.value

  km_hypotheses <- ifelse(logrank_km$p.value[1] > 0.05, "H0: brak istotnych różnic w rozkładzie przeżycia", "H1: pomiędzy rozkładami zachodzą istotne różnice")

  return(list(
    km_plot = km_plot,
    km_table = km_table,
    km_hypotheses = km_hypotheses,
    logrank_km = logrank_km))
}

