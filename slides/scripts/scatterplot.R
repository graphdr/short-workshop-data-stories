# scatterplot
library(data.table)

# gapminder: world regions
regions <- fread("data/world-regions.csv", header =  TRUE)
regions <- regions[, 
                   .(name, four_regions, six_regions, eight_regions)]
setnames(regions, "name", "country")

# gapminder: life expectancy data
life_exp <- fread("data/life_expectancy_years.csv", header =  TRUE)
life_exp <- life_exp[, c("country", "1971":"2021")] 
life_exp <- melt(life_exp,
                 id.vars       = "country",
                 variable.name = "year",
                 value.name    = "lifeExp")
life_exp[, year := as.character(year)]
life_exp[, year := as.integer(year)]

# gapminder: GDP in constant 2011 PPP dollars
gdp <- fread("data/gdp_percapita_ppp.csv", header =  TRUE)
gdp <- gdp[, c("country", "1971":"2021")] 
gdp <- melt(gdp,
            id.vars       = "country",
            variable.name = "year",
            value.name    = "per_capita_gdp")

# convert factors to integers
gdp[, year := as.character(year)]
gdp[, year := as.integer(year)]

# isolate GDPs with a "k", delete "k", multiply by 1000
gdp_sub1 <- gdp[grep("k", per_capita_gdp)]
gdp_sub1[, per_capita_gdp := unlist(strsplit(per_capita_gdp, "k"))]
gdp_sub1[, per_capita_gdp := 1000 * as.double(per_capita_gdp)]

# GDPs no "k"
gdp_sub2 <- gdp[!grep("k", per_capita_gdp)]
gdp_sub2[, per_capita_gdp := as.double(per_capita_gdp)]

# recombine subsets
gdp <- rbindlist(list(gdp_sub1, gdp_sub2))
gdp <- gdp[order(country)]

# merge three data sets
dt <- merge(life_exp, gdp, by = c("country", "year"), all.x = FALSE)
dt <- merge(dt, regions, by = c("country"), all.x = FALSE)

# recoding
dt[eight_regions == "america_south", eight_regions := "South America"]
dt[eight_regions == "america_north", eight_regions := "North/Central America"]
dt[eight_regions == "east_asia_pacific", eight_regions := "Pacific/East Asia"]
dt[eight_regions == "europe_west", eight_regions := "Western Europe"]
dt[eight_regions == "europe_east", eight_regions := "Eastern Europe"]
dt[eight_regions == "africa_sub_saharan", eight_regions := "Sub-saharan Africa"]
dt[eight_regions == "asia_west", eight_regions := "Western Asia"]
dt[eight_regions == "africa_north", eight_regions := "Northern Africa"]

dt[four_regions == "europe"   , four_regions := "Europe"]
dt[four_regions == "asia"     , four_regions := "Asia"]
dt[four_regions == "americas" , four_regions := "Americas"]
dt[four_regions == "africa"   , four_regions := "Africa"]

# max year for scatterplots
dt1 <- copy(dt)
dt1 <- dt1[year == max(year)]


# all years for time series
dt2 <- copy(dt)

# save 
saveRDS(dt1, file = "data/scatterplot.rds")
saveRDS(dt2, file = "data/life-exp.rds")






