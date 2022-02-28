# Max Carter-Brown, Feb 2022
# main analysis script

# read in excel files
library("openxlsx")
# data manipulation
library(data.table)
# visualisation
library(ggplot2)
library(scales)
library(ggrepel)

setwd("~/Documents/JOBS/applications/bank_of_england/data_assessment/analysis/")

## Functions ##

# format both data sheets.
format_data <- function(data_path, sheet_number = 1, first_col_name = "Firm", sep_char = "==") {
  # pull data in
  data <- as.data.table(openxlsx::read.xlsx(data_path, sheet = sheet_number))
  # pasta column names
  colnames(data) <- paste(sep = sep_char, colnames(data), as.character(unlist(data[1,])))
  # remove first row
  data <- data[-1, ]
  # rename first column name
  colnames(data)[1] <- first_col_name
  # some data manipulation
  melt_data <- melt(data = data, id.vars = "Firm")
  melt_data[, c("Variable", "Year") := tstrsplit(variable, sep_char, fixed=TRUE)]
  melt_data[, variable := NULL]
  # parse the year properly
  melt_data[, Year := as.Date(gsub(pattern = "YE", replacement = "", x = Year, fixed = TRUE), format = "%Y")]
  # value to numeric
  melt_data[, value := as.numeric(value)]
  return(melt_data)
}
# aggregate by mean + filter
avg_value_filter <- function(data, threshold = 10000, 
                             direction = "greater", 
                             plot = FALSE,
                             colour = FALSE) {
  # depending on direction
  if(direction == "greater") {
    inner_data <- data[, .(mean(value)), by = .(Firm)][V1 > threshold]  
  } else {
    inner_data <- data[, .(mean(value)), by = .(Firm)][V1 < threshold]  
  }
  
  firms <- inner_data$Firm
  # by_colour <- if(colour) { "Firm" } else { "None" }
  
  
  if(plot) {
    plot <- ggplot(data[Firm %in% firms], aes(x = Year, y = value)) +
      geom_line(aes(group = Firm, colour = if(colour) { get("Firm") } else { NULL }))
    
    return(plot)
  } else {
    return(inner_data)
  }
}

### GENERAL DATA SHEET ###

general_data <- format_data("data/Data for technical assessment.xlsx", sheet_number = 1)

# look at the main variables in the assessment
# filter for GWP
gwp <- general_data[Variable == "GWP.(£m)"]
# filter for NWP
nwp <- general_data[Variable == "NWP.(£m)"]
# data in same order, so we can do simple divisions here
nwp_div_gwp <- data.table(Firm = gwp$Firm, Year = gwp$Year, nwp_div_gwp = nwp$value / gwp$value, gwp = gwp$value, nwp = nwp$value)
scr <- general_data[Variable == "SCR.coverage.ratio"]

## GWP ##
# too much to see
ggplot(gwp, aes(x = Year, y = value)) +
  geom_line(aes(group = Firm))

# look at variance
# write to table
table_1 <- head(gwp[, .(var(value)), by = .(Firm)][order(-V1)])
fwrite(x = table_1, file = "./tables/variance_table.tsv")

# GWP by average over 4 years

gwp_plot <- avg_value_filter(data = gwp, threshold = 10000, direction = "greater", plot = TRUE, colour = TRUE) + 
  ylab(label = "£m GWP") +
  labs(colour = "Firm")

ggsave(plot = gwp_plot, filename = "./figs/gwp_plot_avg_10000.jpeg", units = "in", width = 7.53, height = 6.51)

## NWP ##

nwp_plot <- avg_value_filter(data = nwp, threshold = 10000, direction = "greater", plot = TRUE, colour = TRUE) + 
  ylab(label = "£m NWP") +
  labs(colour = "Firm") +
  scale_y_continuous(labels = scales::comma)

ggsave(plot = nwp_plot, filename = "./figs/nwp_plot_avg_10000.jpeg", units = "in", width = 7.53, height = 6.51)

# missing firms
setdiff(avg_value_filter(data = gwp, threshold = 10000, direction = "greater", plot = FALSE)$Firm,
avg_value_filter(data = nwp, threshold = 10000, direction = "greater", plot = FALSE)$Firm)

# NWP / GWP
nwp_div_gwp[nwp_div_gwp > 0 & nwp_div_gwp < 1]
nwp_gwp_firms <- head(nwp_div_gwp[, .(var(nwp_div_gwp)), by = .(Firm)][order(-V1)])$Firm
# two firms stick out here
nwp_div_gwp_plot <- ggplot(nwp_div_gwp[Firm %in% nwp_gwp_firms ], aes(x = Year, y = nwp_div_gwp)) +
  geom_line(aes(group = Firm))

ggsave(plot = nwp_div_gwp_plot, filename = "./figs/nwp_div_gwp_plot.jpeg", units = "in", width = 7.53, height = 6.51)

# we can see them here
# These are the firms which are re-insuring heavily. 
table_2 <- nwp_div_gwp[, .(mean(nwp_div_gwp)), by = .(Firm)][V1 < 0.25][order(V1)]
fwrite(x = table_2, file = "./tables/nwp_div_gwp.tsv")

# quick linear model
lm1 <- lm(nwp ~ gwp, data = nwp_div_gwp)
jpeg(filename = "./figs/regression_nwp_gwp.jpeg", units = "in", width = 7.53, height = 6.51, res = 1000)
plot(nwp ~ gwp, data = nwp_div_gwp, xlab = "GWP (£m)", ylab = "NWP (£m)")
abline(a = 0, b = 1, col = "red")
abline(a = coef(lm1)[1], b = coef(lm1)[2], col = "grey", lty =2)
#nwp_div_gwp[, is_less_than_line := nwp < lm1$fitted.values & gwp > 15000]
#text(x = nwp_div_gwp$gwp, y = nwp_div_gwp$nwp, labels = ifelse(nwp_div_gwp$is_less_than_line, yes = nwp_div_gwp$Firm, ""))
dev.off()

## SCR ##
# avg less than 100% over the 4 year period
# as we understand the data and text
# 272 firms
scr[, .(mean(value)), by = .(Firm)][V1 > 1]

avg_scr_less_1 <- scr[, .(mean(value)), by = .(Firm)][V1 > 15 & V1 < 10000]
avg_scr_less_1$Firm

high_scr_plot <- ggplot(scr[Firm %in% avg_scr_less_1$Firm], aes(x = Year, y = value)) +
  geom_line(aes(group = Firm, colour = Firm)) +
  scale_y_continuous(labels = scales::comma)

ggsave(plot = high_scr_plot, filename = "./figs/high_scr_plot.jpeg", units = "in", width = 7.53, height = 6.51)

# firms with suspiciously high scr
suspect_scr1 <- scr[value > 1000][order(-value)]
fwrite(x = suspect_scr1, file = "./tables/suspect_scr1.tsv")

### UNDERWRITING DATA SHEET

# gross claims incurred and net combined ratio in this dataset
underwriting <- format_data("data/Data for technical assessment.xlsx", sheet = 2)

gross_claims_incurred <- underwriting[Variable == "Gross.claims.incurred.(£m)"]
net_combined_ratio <- underwriting[Variable == "Net.combined.ratio"]

## Gross Claims Incurred ##

# 128 firms  
gross_claims_incurred[, .(mean(value)), by = .(Firm)][V1 < 1]

# cut off at 1000
gross_claims_incurred_plot <- avg_value_filter(data = gross_claims_incurred, threshold = 1000, direction = "greater", plot = TRUE, colour = TRUE) + 
  ylab(label = "£m Gross Claims Incurred") +
  labs(colour = "Firm") +
  scale_y_continuous(labels = scales::comma)

ggsave(plot = gross_claims_incurred_plot, filename = "./figs/gross_claims_incurred_plot.jpeg", units = "in", width = 7.53, height = 6.51)

# variance
var_firms_gci <- head(gross_claims_incurred[, .(var(value)), by = .(Firm)][order(-V1)])[-c(1,2)]$Firm
variable_gci_plot <- ggplot(gross_claims_incurred[Firm %in% var_firms_gci], aes(x = Year, y = value)) +
  geom_line(aes(group = Firm)) +
  facet_wrap(~ Firm) + 
  ylab(label = "£m Gross Claims Incurred")

ggsave(plot = variable_gci_plot, filename = "./figs/variable_gci_plot.jpeg", units = "in", width = 7.53, height = 6.51)


## Net Combined Ratio ##
# simple histogram
jpeg(filename = "./figs/hist_ncr.jpeg", units = "in", width = 7.53, height = 6.51, res = 1000)
hist(net_combined_ratio$value[net_combined_ratio$value < 4 & net_combined_ratio$value > -0.5], 
     breaks = 40, 
     main = "", 
     xlab = "NCR < 400%, and NCR > -50%")
dev.off()

# remove very extreme outliers
NCR_exclude1 <- net_combined_ratio[value > 500000]$Firm
NCR_exclude2 <- net_combined_ratio[value < -1000]$Firm

net_combined_ratio_minus_outliers <- net_combined_ratio[!Firm %in% c(NCR_exclude1, NCR_exclude2)]

net_combined_ratio_minus_outliers_plot <- ggplot(net_combined_ratio_minus_outliers[, if(all(value > 0 & value < 0.8)) .SD, by = Firm], aes(x = Year, y = value)) +
  geom_line(aes(group = Firm, colour = Firm)) + 
  ylab(label = "Net Combined Ratio %") + 
  scale_y_continuous(labels = function(x) {x * 100})
  

ggsave(plot = net_combined_ratio_minus_outliers_plot, filename = "./figs/net_combined_ratio_minus_outliers_plot.jpeg", units = "in", width = 7.53, height = 6.51)

# table of possible high profit firms
pos_high_prof_firms <- net_combined_ratio_minus_outliers[value < -1][order(value)]
fwrite(x = pos_high_prof_firms, file = "./tables/pos_high_profit_firms.tsv")


## PCA

master <- cbind(dcast(data = general_data, formula = Firm ~ Variable, fun.aggregate = mean),
      dcast(data = underwriting, formula = Firm ~ Variable, fun.aggregate = mean))

pca_data <- master[`Gross.claims.incurred.(£m)` < 1000 & Net.combined.ratio < 1000 & SCR.coverage.ratio < 1000 & Net.combined.ratio > -1000]
# remove firm for now
pca <- prcomp(pca_data[, -c("Firm")])

pca_plot <- ggplot(data.frame(pca$x), aes(x = PC1, y = PC2)) +
  geom_point() + 
  geom_text_repel(nudge_x = -10, nudge_y =-1000, aes(label = ifelse(test = pca$x[,1] < -1e+05 | pca$x[,2] < -10000, yes = pca_data$Firm, no = "")))

ggsave(plot = pca_plot, filename = "./figs/pca_plot.jpeg", units = "in", width = 7.53, height = 6.51)
