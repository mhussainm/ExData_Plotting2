# The script reads the National Emissions Inventory dataset, obtains a subset, 
# and generates a 'points and lines' plot as 'plot1.png' file as required to
# answer the question 'Have total emissions from PM2.5 decreased in the 
# United States from 1999 to 2008?' 
# ANSWER: "Yes" based on the generated plot.
#
# PRE-REQUISITES:
#   (1) Installed 'dplyr' package (if not installed: 'install.packages("dplyr")')
#   (2) This script expects the file 'summarySCC_PM25.rds' in the working directory.
#   (3) Sourcing this file and executing the function gen_plot3() without any parameters.
#
library(dplyr)

gen_plot1 <- function() {
    # Reading the PM2.5 Emissions Data as a data.frame [6497651 x 6]
    neiData <- readRDS("summarySCC_PM25.rds");
    
    # Summarize to obtain total PM2.5 emissions for each year
    totalEmsPerYear <- neiData %>% group_by(year)  %>% summarise(totalemissions = sum(Emissions));
    
    # Convert Total Emission from ton to kiloton to enhance readibility of plot
    totalEmsPerYearKT <- transform(totalEmsPerYear, 
                                totalemissions = totalemissions / 1000)
    
    # Launch PNG device to generate the PNG file with the required plot
    png(file = "plot1.png", bg = "white");
    
    # A 'points and line' plot with custom Y-axis and no X-axis
    plot(totalEmsPerYearKT, type = "b", xaxt = "n", ylim = c(3000, 8000), 
            main = "PM2.5 Emissions in US", xlab = "Year", 
            ylab = "Total PM2.5 Emissions (kilotons)", pch = 19);
    
    # Add custom X-axis
    axis(1, xaxp = c(1999, 2008, 3));
    
    # Add text to plotted points displaying the value of Total emission per year
    text(totalEmsPerYearKT$year, totalEmsPerYearKT$totalemissions, 
            labels=round(totalEmsPerYearKT$totalemissions), pos=3, col="blue");

    # closing the PNG device
    dev.off();
    
    print("Plot generated on file: plot1.png");
}