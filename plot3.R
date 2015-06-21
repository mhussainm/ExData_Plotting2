# The script reads the National Emissions Inventory dataset, obtains a subset, 
# and generates 4 'points and lines' plot as 'plot3.png' file as required to
# answer the following questions: 
#   (a) Of the four types of sources indicated by the type (point, nonpoint, 
#       onroad, nonroad) variable, which of these four sources have seen decreases 
#       in emissions from 1999–2008 for Baltimore City? [ANSWER: Non-Road, NonPoint, On-Road]
#   (b) Which have seen increases in emissions from 1999–2008? [ANSWER: Point]
# Answers are based on the generated plot.
#
#
# PRE-REQUISITES:
#   (1) Installed 'dplyr' package (if not installed: 'install.packages("dplyr")')
#   (2) Installed 'ggplot2' package (if not installed: 'install.packages("ggplot2")')
#   (3) This script expects the file 'summarySCC_PM25.rds' in the working directory.
#   (4) Sourcing this file and executing the function gen_plot3() without any parameters.
#
library(dplyr)
library(ggplot2)

gen_plot3 <- function() {
    # Reading the PM2.5 Emissions Data as a data.frame [6497651 x 6]
    neiData <- readRDS("summarySCC_PM25.rds");
    
    # Subset of only Baltimore City's PM2.5 observations for years 1999 and 2008
    bcNeiData <- filter(neiData, fips == "24510", year %in% c(1999, 2008)); 

    # Summarize to obtain total PM2.5 emissions for each year and type
    bcEmsTypeYr <- bcNeiData %>% group_by(type, year)  %>% summarise(totalemissions = sum(Emissions));
    
    # Launch PNG device to generate the PNG file with the required plot
    png(file = "plot3.png", bg = "white");
    
    pl <- ggplot(bcEmsTypeYr, aes(year, totalemissions), group=1) +
            geom_line() +
            geom_point() +
            facet_grid(. ~ type) + 
            coord_cartesian(xlim=c(1997, 2010)) +
            scale_x_continuous(breaks=c(1999,2008)) +
            xlab("Year") + 
            ylab("Total Emissions (tons)") +
            ggtitle("PM2.5 Emissions by Type of Source - Baltimore City, MD");
    
    print(pl);

    # closing the PNG device
    dev.off();
    
    print("Plot generated on file: plot3.png");
}