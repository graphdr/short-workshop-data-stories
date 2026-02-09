# time series

dt <- readRDS(file = "data/life-exp.rds")

# US time series
dt1 <- copy(dt)
dt1 <- dt1[country == "United States"]


# superposed three countries time series
dt2 <- copy(dt)
dt2 <- dt2[country %chin% c("United States", "Canada", "Peru")]


# 8-facet time series
x <- copy(dt)
x <- na.omit(x)
# x <- x[year == max(year)]
x <- x[, max_lifeExp := max(lifeExp), by = "eight_regions"]
x <- x[, .(country, lifeExp, max_lifeExp, eight_regions)]
x <- x[lifeExp == max_lifeExp]
x <- unique(x)
select_country <- x$country

dt3 <- copy(dt)
dt3 <- dt3[, .(country, year, lifeExp, eight_regions)]
dt3 <- dt3[country %chin% select_country] 
dt3[, country := factor(
  country, 
  levels = c("Mauritius", "Israel", "Tunisia", "Finland", 
             "Peru", "Canada", "Singapore", "Iceland"))]

# time series, cyclic
dt4 <- fread("data/ice_extent_NH.csv", header = TRUE)
dt4 <- dt4[, 1:13]
setnames(dt4, "V1", "year")
dt4 <- melt(dt4, 
           id.vars = "year", 
           variable.name = "month", 
           value.name = "extent")
dt4 <- dt4[year != 1978]
dt4[, hemis := "Arctic"]
dt4 <- unique(dt4)
dt4 <- dt4[, month := factor(month, levels = c("September", 
                                             "October", 
                                             "November", 
                                             "December", 
                                             "January", 
                                             "February", 
                                             "March", 
                                             "April", 
                                             "May", 
                                             "June", 
                                             "July", 
                                             "August"))]
dt4 <- dt4[order(month, year)]

# same, southern hemisphere
SH <- fread("data/ice_extent_SH.csv", header = TRUE)
SH <- SH[, 1:13]
setnames(SH, "V1", "year")
SH <- melt(SH, 
           id.vars = "year", 
           variable.name = "month", 
           value.name = "extent")
SH <- SH[year != 1978]
SH[, hemis := "Antarctic"]
SH <- unique(SH)
SH <- SH[, month := factor(month, levels = c("September", 
                                             "October", 
                                             "November", 
                                             "December", 
                                             "January", 
                                             "February", 
                                             "March", 
                                             "April", 
                                             "May", 
                                             "June", 
                                             "July", 
                                             "August"))]
SH <- SH[order(month, year)]
dt4 <- rbindlist(list(dt4, SH))




# 4-country time series in 2 dimensions
dt <- readRDS(file = "data/life-exp.rds")
dt5 <- copy(dt)
dt5 <- dt5[country %chin% c("United States", "Russia", "South Korea", "Peru")]
setorderv(dt5, c("country", "year"))
dt5[, gdp_k := per_capita_gdp/1000]
dt5 <- dt5[, .(country, year, lifeExp, gdp_k)]





# save 
saveRDS(dt1, file = "data/time-series.rds")
saveRDS(dt2, file = "data/time-series-superpose.rds")
saveRDS(dt3, file = "data/time-series-facet.rds")
saveRDS(dt4, file = "data/time-series-cyclic.rds")
saveRDS(dt5, file = "data/time-series-2d.rds")
