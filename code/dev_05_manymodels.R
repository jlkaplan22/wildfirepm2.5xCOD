#### TWFEs with varying model specifications ####
#All-cause mortality and mean smoke PM2.5

source("code/quickprepfinaldata_AC.R")


fes <- c("fipsihme + yearmonth", "fipsihme^month + year", "fipsihme + year")
temp <- c("", "weighted_temp", "temp", "ns(weighted_temp, df=3)", "ns(weighted_temp, df=5)")
precip <- c("", "weighted_precip", "precip", "ns(weighted_precip, df=3)", "ns(weighted_precip, df=5)")

grid <- 
    expand.grid(fes = fes, temp = temp, precip = precip) %>% 
    tibble() %>% 
    mutate(
        controls = 
            dplyr::case_when(
                temp == "" & precip == "" ~ "",
                temp == "" & precip != "" ~ as.character(precip),
                precip == "" & temp != "" ~ as.character(temp),
                TRUE ~ paste0(as.character(precip), "+", as.character(temp))
            ),
        .keep = c("unused")
    )
    
modspecs <- as.data.frame(matrix(nrow=0, ncol=14))
colnames(modspecs) <-
    c(
        "county_fe", "year_fe", "year-month_fe", "county-month_fe",
        "area-weighted_pm2.5", "pop-weighted_pm2.5",
        "area-weighted_temp", "pop-weighted_temp",
        "area-weighted_precip", "pop-weighted_precip",
        "linear_temp", "ns=3_temp", "ns=5_temp",
        "countypop_weighted"
    )

#County-month and year FEs ==========================
modspecs[1,] <- c("0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1", "0", "1")
cm_y_main <-
    cbind(
        modspecs,
        modeler(final, "mean_pm2.5", "+ns(weighted_temp, df=3) + weighted_precip", "fipsihme^month + year", final$fipsihme)
    )

modspecs[1,] <- c("0", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1")
cm_y_nocontrols <-
    cbind(
        modspecs,
        modeler(final, "mean_pm2.5", "", "fipsihme^month + year", final$fipsihme)
    )

allmods <-
    rbind(
        cm_y_main,
        cm_y_nocontrols
    )
