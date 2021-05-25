analiza_hr <- function(ipd)
  {
    time = ipd$time
    status = ipd$status
    group.by = ipd$treat
    arm = ipd$arm
    data = ipd
    formula_ref0 = Surv(time, status) ~ arm
    formula_ref1 = Surv(time, status) ~ relevel(factor(ipd$arm), ref = "1")

    fit_cox_ref0 <- coxph(formula_ref0)
    fit_cox_ref1 <- coxph(formula_ref1)

    model_arm0 <- summary(fit_cox_ref0) %>%
      unclass()
    model_arm1 <- summary(fit_cox_ref1) %>%
      unclass()
    hrtable <- rbind(model_arm0$conf.int,
                      model_arm1$conf.int) %>%
      round(2)
    rownames(hrtable) <- c("arm0", "arm1")

    hr_table <- rbind(
      data.frame("Reference" = "arm0", round(model_arm0$conf.int,2),  "Wald test p-value" = round(model_arm0$waldtest[3],2)),
      data.frame("Reference" = "arm1", round(model_arm1$conf.int,2),  "Wald test p-value" = round(model_arm1$waldtest[3],2))) %>%
      select(-exp..coef.)
    colnames(hr_table) <- c("Reference", "HR", "0.95LCI", "0.95UCI", "Wald test p-value")
    row.names(hr_table) <- NULL
    write.csv2(hr_table, "hr_table.csv")

    cumhaz_plot <-  ggsurvplot(fit_km,
                               fun = "cumhaz",
                               conf.int = TRUE,
                               conf.int.style = "ribbon",
                               xlab = "Time",
                               break.time.by = 5,
                               ggtheme = theme_pubclean(),
                               palette = c("darkslategray4","orange4"),
                               xlim = c(min(time_risk), max(time_risk)))
    hr_hypotheses <- ifelse(hr_ratio[1] < 1, "spadek ryzyka zgonu", "wzrost ryzyka zgonu")

    prophaz_hypotheses <- ifelse(model_cox$logtest[3] > 0.05, "H0: brak proporcjonalnego hazardu", "H1:zachodzi proporcjonaly hazard")

    return(list(
      hr_table = hr_table,
      prophaz_hypotheses = prophaz_hypotheses,
      cumhaz_plot = cumhaz_plot,
      hr_hypotheses = hr_hypotheses))
  }

