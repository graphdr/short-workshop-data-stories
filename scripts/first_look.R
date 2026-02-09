library(data.table)
library(ggplot2)
library(graphclassmate)

options(
	datatable.print.nrows = 30,
	datatable.print.topn = 30,
	datatable.print.class = TRUE
)

dt <- fread("data/sdu-peak-energy.csv",
						colClasses = list(
							double = c("minutes", "scale_5k", "fuel_cell", "meter", "total")
						))



dt <- dt[is.na(meter), meter := total - fuel_cell]




dt[, solar := total - (fuel_cell + meter)]

dt[, fuel_cell_MW := round(fuel_cell * 5/scale_5k, 2)]
dt[, meter_MW     := round(meter * 5/scale_5k, 2)]
dt[, solar_MW     := round(solar * 5/scale_5k, 2)]
dt[, total_MW     := fuel_cell_MW + meter_MW + solar_MW]

dt

dt <- dt[, .(item, bill_year, bill_month,  date_peak,  minutes,
						 fuel_cell_MW, meter_MW, solar_MW, total_MW)]

setnames(dt,
				 old = c("fuel_cell_MW", "meter_MW", "solar_MW", "total_MW"),
				 new = c("fuel_cell", "meter", "solar", "total"))

dt





dt_pct <- copy(dt)
dt_pct[, fuel_cell_pct := round(100 * fuel_cell / total, 2)]
dt_pct[, meter_pct     := round(100 * meter     / total, 2)]
dt_pct[, solar_pct     := round(100 * solar     / total, 2)]
dt_pct$fuel_cell <- NULL
dt_pct$meter <- NULL
dt_pct$solar <- NULL
dt_pct$total <- NULL

dt_pct <- melt(dt_pct,
					 id.vars = c("item", "bill_year", "bill_month",  "date_peak", "minutes"),
					 measure_vars = c("fuel_cell_pct", "meter_pct", "solar_pct"),
					 variable.name = c("source"),
					 variable.factor = FALSE,
					 value.name = c("percent")
)
dt_pct





dt <- melt(dt,
					 id.vars = c("item", "bill_year", "bill_month",  "date_peak", "minutes"),
					 measure_vars = c("fuel_cell_MW", "meter_MW", "solar_MW", "total_MW"),
					 variable.name = c("source"),
					 variable.factor = FALSE,
					 value.name = c("MW")
)
dt


dt[, source_med := median(MW), by = c("source", "bill_month")]

dt_pct[, source_med := median(percent), by = c("source", "bill_month")]



month_levels <- c("Jul", "Aug", "Sep",
									"Oct", "Nov", "Dec",
									"Jan", "Feb", "Mar",
									"Apr", "May", "Jun")

dt <- dt[bill_month %chin% month_levels]

dt_pct <- dt_pct[bill_month %chin% month_levels]

dt[, bill_month := factor(bill_month,levels = month_levels)]

dt_pct[, bill_month := factor(bill_month,levels = month_levels)]


dt[source == "fuel_cell", source := "Fuel cell"]
dt[source == "total", source := "Total"]
dt[source == "meter", source := "Meter"]
dt[source == "solar", source := "Solar"]

dt_pct[source == "fuel_cell_pct", source := "Fuel cell"]
dt_pct[source == "meter_pct", source := "Meter"]
dt_pct[source == "solar_pct", source := "Solar"]



ggplot(data = dt, aes(x = minutes, y = MW, color = source)) +
	geom_line(color = "gray") +
	geom_point(data = dt[MW > 0], size = 2) +
	# facet_wrap(vars(reorder(source, source_med)), ncol = 1, as.table = FALSE) +
	facet_grid(rows = vars(reorder(source, source_med, na.rm = TRUE)),
						 cols = vars(bill_month),
						 as.table = FALSE) +
	labs(x = "One peak demand day per month, time of day (0000 to 2400 hours)",
			 y = "Demand by source (MW)",
			 title = "USD Electricity Usage, FY 2018-2019") +
	scale_x_continuous(limits = c(0, 1440),
										 breaks = c(0, 420, 1140, 1440),
										 labels = c("0", "7", "19", "")) +
	theme_graphclass(font_size = 14) +
	theme(legend.position = "none",
				panel.grid.major.y = element_line(color = "gray"),
				panel.spacing.y = unit(15, "mm"),
				strip.text.y.right  = element_text(size = 14, hjust = 0, angle = 0))



# ggsave(filename = "sdu-elec-by-month.png",
# 			 path = "images",
# 			 width = 16,
# 			 height = 9,
# 			 units = "in")



# percent version

ggplot(data = dt_pct, aes(x = minutes, y = percent, color = source)) +
	geom_line(color = "gray") +
	geom_point(data = dt_pct[percent > 0], size = 2) +
	# facet_wrap(vars(reorder(source, source_med)), ncol = 1, as.table = FALSE) +
	facet_grid(rows = vars(reorder(source, source_med, na.rm = TRUE)),
						 cols = vars(bill_month),
						 as.table = FALSE) +
	labs(x = "One peak demand day per month, time of day (0000 to 2400 hours)",
			 y = "Demand by source (percent)",
			 title = "USD Electricity Usage, FY 2018-2019") +
	scale_x_continuous(limits = c(0, 1440),
										 breaks = c(0, 420, 1140, 1440),
										 labels = c("0", "7", "19", "")) +
	theme_graphclass(font_size = 14) +
	theme(legend.position = "none",
				panel.grid.major.y = element_line(color = "gray"),
				panel.spacing.y = unit(15, "mm"),
				strip.text.y.right  = element_text(size = 14, hjust = 0, angle = 0))





#------------------------------------------

x <- copy(dt)

x[, hour := ceiling(minutes/60)]
x[, hour_mean := round(mean(MW), 2), by = c("bill_month", "source", "hour")]
x <- x[4:nrow(x)]
x <- x[seq(1, nrow(x), 4)]



ggplot(data = x, aes(x = bill_month, y = MW, color = source)) +
	geom_point(data = x[MW > 0], size = 2) +
	facet_grid(rows = vars(reorder(source, source_med)),
						 cols = vars(hour),
						 as.table = FALSE,
						 switch = "x") +
	labs(x = "Same hour in each month's demand day",
			 y = "Demand by source (MW)",
			 title = "USD Electricity Usage, FY 2018-2019") +
	geom_text(data = x[source == "Total" & hour == 1],
						aes(x = -1,
								y = 2,
								label = "Jul"),
						hjust = 0,
						vjust = 1) +
	# geom_text(data = x[source == "Total" & hour == 1],
	# 					aes(x = 6,
	# 							y = 1.8,
	# 							label = "Jan"),
	# 					hjust = 0.5,
	# 					vjust = 1) +
	geom_text(data = x[source == "Total" & hour == 1],
						aes(x = 12.5,
								y = 2.7,
								label = "Jun"),
						hjust = 1,
						vjust = 1) +
	theme_graphclass(font_size = 14) +
	theme(legend.position = "none",
				panel.grid.major.y = element_line(color = "gray"),
				panel.grid.major.x = element_blank(),
				panel.spacing.y = unit(15, "mm"),
				panel.spacing.x = unit(1, "mm"),
				strip.text.y.right  = element_text(size = 14, hjust = 0, angle = 0),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank())

# ggsave(filename = "sdu-elec-by-hour.png",
# 			 path = "images",
# 			 width = 16,
# 			 height = 9,
# 			 units = "in")





# percent version

x <- copy(dt_pct)

x[, hour := ceiling(minutes/60)]
x[, hour_mean := round(mean(percent), 2), by = c("bill_month", "source", "hour")]
x <- x[4:nrow(x)]
x <- x[seq(1, nrow(x), 4)]

ggplot(data = x, aes(x = bill_month, y = percent, color = source)) +
	geom_point(data = x[percent > 0], size = 2) +
	facet_grid(rows = vars(reorder(source, source_med)),
						 cols = vars(hour),
						 as.table = FALSE,
						 switch = "x") +
	labs(x = "Same hour in each month's demand day",
			 y = "Demand by source (% of total MW)",
			 title = "USD Electricity Usage, FY 2018-2019") +
	geom_text(data = x[source == "Meter" & hour == 12],
						aes(x = -1,
								y = 40,
								label = "Jul"),
						hjust = 0,
						vjust = 1) +
	# geom_text(data = x[source == "Total" & hour == 1],
	# 					aes(x = 6,
	# 							y = 1.8,
	# 							label = "Jan"),
	# 					hjust = 0.5,
	# 					vjust = 1) +
	# geom_text(data = x[source == "Total" & hour == 1],
	# 					aes(x = 12.5,
	# 							y = 2.7,
	# 							label = "Jun"),
	# 					hjust = 1,
	# 					vjust = 1) +
	theme_graphclass(font_size = 14) +
	theme(legend.position = "none",
				panel.grid.major.y = element_line(color = "gray"),
				panel.grid.major.x = element_blank(),
				panel.spacing.y = unit(15, "mm"),
				panel.spacing.x = unit(1, "mm"),
				strip.text.y.right  = element_text(size = 14, hjust = 0, angle = 0),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank())


ggsave(filename = "sdu-elec-by-hour-pct.png",
			 path = "images",
			 width = 16,
			 height = 9,
			 units = "in")


















ggplot(data = dt, aes(x = minutes, y = MW, color = source)) +
	geom_line(color = "gray") +
	geom_point(data = dt[MW > 0], size = 2) +
	facet_grid(rows = vars(reorder(source, source_med, na.rm = TRUE)),
						 cols = vars(bill_month),
						 as.table = FALSE) +
	labs(x = "One peak demand day per month, time of day (0000 to 2400 hours)",
			 y = "Demand by source (MW)",
			 title = "USD Electricity Usage, FY 2018-2019") +
	scale_x_continuous(limits = c(0, 1440),
										 breaks = c(0, 420, 1140, 1440),
										 labels = c("0", "7", "19", "")) +
	theme_graphclass(font_size = 10) +
	theme(legend.position = "none",
				panel.grid.major.y = element_line(color = "gray"),
				strip.text.y.right  = element_text(size = 10, hjust = 0, angle = 0))




# --------------- distributional

y <- copy(dt)
y <- y[, .(source, MW, source_med)]
y



ggplot() +
	geom_boxplot(data = y,
							 aes(x = MW, y = reorder(source, source_med), color = source),
							 size = 1,
							 width = 0.45,
							 outlier.shape = NA) +
	labs(x = "Demand (MW)",
			 y = "",
			 title = "USD Electricity Summary, FY 2018-2019") +
	theme_graphclass(font_size = 20) +
	theme(legend.position = "none",
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y  = element_blank(),
				axis.ticks.y       = element_blank())

# ggsave(filename = "usd-energy-boxplot.png",
# 			 path = "images",
# 			 width = 18,
# 			 height = 6,
# 			 units = "in")


#-----------------
# saveRDS(dt, file = "data/usd-elec-data.rds")
