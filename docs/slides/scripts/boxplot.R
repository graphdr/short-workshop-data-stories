library(graphclassmate)
library(data.table)

dt <- data.table(nontraditional)

fig1 <- copy(dt)
fig1 <- fig1[, .(enrolled)]
saveRDS(fig1, file = "data/boxplot.rds")


fig2 <- copy(dt)
fig2 <- fig2[, .(path, enrolled)]
saveRDS(fig2, file = "data/boxplot-compare.rds")


fig3 <- copy(dt)
fig3[, sex_path := paste(sex, path)]
fig3 <- fig3[, .(path, sex_path, enrolled)]
saveRDS(fig3, file = "data/boxplot-merge-category.rds")
