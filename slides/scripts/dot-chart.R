# initial data carpentry

library(data.table)
library(graphclassmate)

data(infant_mortality, package = "graphclassmate")
dt <- data.table(infant_mortality)

# recoding
dt[region == "CENS-R1", region := "Northeast"]
dt[region == "CENS-R2", region := "Midwest"]
dt[region == "CENS-R3", region := "South"]
dt[region == "CENS-R4", region := "West"]
dt[race != "Black", race := "Non-Black"]
dt[, county_id := NULL]
dt[age == "15", age := NA]
dt[age == "45-49", age := NA]
dt <- na.omit(dt)

# dot chart
fig1 <- copy(dt)
fig1 <- dt[, .(deaths = sum(deaths), births  = sum(births)), by = age]
fig1[, per_thou := 1000 * deaths / births]

# multiway, age and race
fig2 <- copy(dt)
fig2 <- fig2[, .(deaths = sum(deaths), births  = sum(births)),
           by = c("age", "race")]
fig2[, per_thou := round(1000 * deaths / births, 1)]

# multiway, age and region
fig3 <- copy(dt)
fig3 <- fig3[, .(deaths = sum(deaths), births  = sum(births)), 
           by = c("age", "region")]
fig3[, per_thou := round(1000 * deaths / births, 1)]
fig3 <- fig3[, .(age, region, per_thou)]

# multiway superposed, age, region, race
# order factors to match previous chart
fig4 <- copy(dt)
fig4 <- fig4[, .(deaths = sum(deaths), births  = sum(births)), 
           by = c("age", "region", "race")]
fig4[, per_thou := round(1000 * deaths / births, 1)]
fig4[, region := factor(region, 
                        levels = c("West", "Northeast", "Midwest", "South"))]

saveRDS(fig1, file = "data/dot-chart.rds")
saveRDS(fig2, file = "data/multiway-2-panel.rds")
saveRDS(fig3, file = "data/multiway-4-panel.rds")
saveRDS(fig4, file = "data/multiway-superpose.rds")
