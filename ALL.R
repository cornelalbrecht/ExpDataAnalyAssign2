# Load Data

library(dplyr)
library(ggplot2)

# Read Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#########################################################################################
# Plot 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to
#2008? Using the base plotting system, make a plot showing the total PM2.5
#emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Sum Data
tot.emiss <- NEI %>% group_by(year) %>% summarize(sum(Emissions))

# Plot data with base plot
png("plot1.png")
barplot(tot.emiss$`sum(Emissions)`, names.arg = tot.emiss$year, col = "red", main = "Total US PM 2.5 Emissions", xlab = "Year", ylab = "Tons of PM 2.5")
dev.off()

## Much nicer way if GGPlot2 was allowed here:
# g <- ggplot(NEI, aes(x = factor(year), y = Emissions))
# g  + stat_summary(fun.y = sum, geom = "bar") + labs(x = "Year", y = "Tons of PM 2.5") + labs(title = "Total US PM 2.5 Emissions")

#########################################################################################
# Plot 2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips
#== "24510") from 1999 to 2008? Use the base plotting system to make a plot
#answering this question.

# Sum and Filter Data
tot.emiss <- NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarize(sum(Emissions))

# Plot data with base plot
png("plot2.png")
barplot(tot.emiss$`sum(Emissions)`, names.arg = tot.emiss$year, col = "red", main = "Total Baltimore PM 2.5 Emissions", xlab = "Year", ylab = "Tons of PM 2.5")
dev.off()

## Much nicer way if GGPlot2 was allowed here:
# g <- ggplot(NEI, aes(x = factor(year), y = Emissions))
# g  + stat_summary(data = subset(NEI, fips == 24510), fun.y = sum, geom = "bar") + labs(x = "Year", y = "Tons of PM 2.5 Emissions") + labs(title = "Total Maryland PM2.5 Emissions") + facet_grid(.~ type)

#########################################################################################
# Plot 3
#Of the four types of sources indicated by the type (point, nonpoint, onroad,
#nonroad) variable, which of these four sources have seen decreases in emissions
#from 1999–2008 for Baltimore City? Which have seen increases in emissions from
#1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# Everything done in ggplot
png("plot3.png", width = 600)
g <- ggplot(NEI, aes(x = factor(year), y = Emissions))
g  + stat_summary(data = subset(NEI, fips == 24510), fun.y = sum, geom = "bar") + labs(x = "Year", y = "Tons of PM 2.5 Emissions") + labs(title = "Total Maryland PM2.5 Emissions") + facet_grid(.~ type)
dev.off()

#########################################################################################
# Plot 4
#Across the United States, how have emissions from coal combustion-related
#sources changed from 1999–2008?

# Grab Coal and Combustion Related SCC IDs
ids <- SCC$SCC[grep("Coal", SCC$EI.Sector)]

# Filter data by IDs
coal.comb <- NEI %>% filter(SCC %in% ids)

# Plot
png("plot4.png")
g <- ggplot(coal.comb, aes(x = factor(year), y = Emissions))
g  + stat_summary(fun.y = sum, geom = "bar") + labs(x = "Year", y = "Tons of PM 2.5 Emissions") + labs(title = "Total US Coal-Combustion-related PM2.5 Emissions")
dev.off()

#########################################################################################
# Plot 5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Grab Motor Vehicle sources
ids <- SCC$SCC[grep("Vehicle", SCC$EI.Sector)]

# Filter by IDs and Baltimore FIPS
mo.ve <- NEI %>% filter(fips == "24510") %>% filter(SCC %in% ids)

# Plot
png("plot5.png")
g <- ggplot(mo.ve, aes(x = factor(year), y = Emissions))
g  + stat_summary(fun.y = sum, geom = "bar") + labs(x = "Year", y = "Tons of PM 2.5 Emissions") + labs(title = "Total Baltimore Motor Vehicle PM2.5 Emissions")
dev.off()

#########################################################################################
# Plot 6
#Compare emissions from motor vehicle sources in Baltimore City with emissions
#from motor vehicle sources in Los Angeles County, California (fips == "06037").
#Which city has seen greater changes over time in motor vehicle emissions?

# Grab Motor Vehicle sources
ids <- SCC$SCC[grep("Vehicle", SCC$EI.Sector)]

# Filter by IDs and Baltimore/LA FIPS
mo.ve <- NEI %>% filter(fips %in% c("24510","06037")) %>% filter(SCC %in% ids)

# Some more summarizing as one can't really see the changes in a direct plot
mo.ve <- mo.ve %>% group_by(fips, year) %>% summarize(sum(Emissions))
colnames(mo.ve)[3] <- "Emissions" # Renamed column for easier handling
# Calculating the percentages (there must be a better way then this, but this work :-))
mo.ve$Emissions <- append(mo.ve$Emissions[1:4]/mo.ve$Emissions[1]*100,mo.ve$Emissions[5:8]/mo.ve$Emissions[5]*100)

# Plot
png("plot6.png", width = 600)
g <- ggplot(mo.ve, aes(x = factor(year), y = Emissions, fill = fips))
g <- g + geom_bar(stat = "identity", position="dodge") # Plot
g <- g + labs(x = "Year", y = "% of 1999 Emissions of each city") + labs(title = "Total Baltimore vs. LA Motor Vehicle PM2.5 Emissions") # Labels
g <- g + scale_fill_discrete(name="City", breaks=c("24510","06037"), labels=c("Baltimore", "Los Angeles")) # Reformatting Legend
g + geom_text(aes(y = Emissions+5, label = sprintf("%1.f%%",Emissions)), position = position_dodge(width = 1), size = 4) # Plotting labels
dev.off()