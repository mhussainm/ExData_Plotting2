# The script reads the National Emissions Inventory dataset, obtains a subset, 
# and generates a 'points and lines' plot as 'plot6.png' file as required to
# answer the question 'Compare emissions from motor vehicle sources in 
# Baltimore City with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle emissions?'
# ANSWER: PM2.5 emissions in Los Angeles, CA compared to Baltimore City, MD are
# very high. However compared to Baltimore, Los Angeles has seen a significant
# decrease in PM2.5 emissions between 2005 to 2008.
# Answer is are based on the generated plot.
#
#
# PRE-REQUISITES:
#   (1) Installed 'dplyr' package (if not installed: 'install.packages("dplyr")')
#   (2) Installed 'ggplot2' package (if not installed: 'install.packages("ggplot2")')
#   (3) This script expects the file 'summarySCC_PM25.rds' and 
#       'Source_Classification_Code.rds' in the working directory.
#   (4) Sourcing this file and executing the function gen_plot5() without any parameters.
#
library(dplyr)
library(ggplot2)

gen_plot6 <- function() {
    # Reading the PM2.5 Emissions Data as a data.frame [6497651 x 6]
    neiData <- readRDS("summarySCC_PM25.rds");
    
    # Reading the emissions' Source Classification Code (SCC) table as a 
    # data.frame [11717 x 15]
    sccData <- readRDS("Source_Classification_Code.rds");
    
    # Derive vector of SCCs where Emission Category includes Coal Combusition
    sccMotorList <- filter(sccData, EI.Sector %in% unique(
        grep("mobile", sccData$EI.Sector, ignore.case = TRUE, value = TRUE)))$SCC;

    # Subset of Motor Vehicle related sources in Baltimore City, MD and Los Angeles, CA
    subNeiData <- filter(neiData, fips %in% c("24510", "06037"), SCC %in% sccMotorList);
    
    # Summarize to obtain total PM2.5 emissions for each year
    totalEmsPerYear <- subNeiData %>% group_by(year, fips)  %>% summarise(totalemissions = sum(Emissions)/1000);

    # Launch PNG device to generate the PNG file with the required plot
    png(file = "plot6.png", bg = "white");
    
    pl <- ggplot(totalEmsPerYear, aes(x = year, y = totalemissions, group = fips, color = fips)) +
            geom_line() +
            geom_point() +
            coord_cartesian(xlim=c(1998, 2009)) +
            scale_x_continuous(breaks=c(1999, 2002, 2005, 2008)) +
            xlab("Year") + 
            ylab("Total PM2.5 Emissions (kilotons)") +
            ggtitle("PM2.5 Emissions from Motor Vehicle sources") + 
            scale_color_discrete(name = "US Cities", 
                labels = c("Los Angeles, CA", "Baltimore City, MD"));
    
    print(pl);

    # closing the PNG device
    dev.off();
    
    print("Plot generated on file: plot6.png");
}