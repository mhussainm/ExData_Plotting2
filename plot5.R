# The script reads the National Emissions Inventory dataset, obtains a subset, 
# and generates a 'points and lines' plot as 'plot5.png' file as required to
# answer the question 'How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?'
# ANSWER: Significant decrease in PM2.5 emission from 1999 to 2002, then almost 
# none to slight increase from 2002 to 2005 followed by a steady increase from
# 2005 to 2008. 
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

gen_plot5 <- function() {
    # Reading the PM2.5 Emissions Data as a data.frame [6497651 x 6]
    neiData <- readRDS("summarySCC_PM25.rds");
    
    # Reading the emissions' Source Classification Code (SCC) table as a 
    # data.frame [11717 x 15]
    sccData <- readRDS("Source_Classification_Code.rds");
    
    # Derive vector of SCCs where Emission Category includes Coal Combusition
    sccMotorList <- filter(sccData, EI.Sector %in% unique(
        grep("mobile", sccData$EI.Sector, ignore.case = TRUE, value = TRUE)))$SCC;

    # Subset of Motor Vehicle related sources in Baltimore City, MD
    subNeiData <- filter(neiData, fips == "24510", SCC %in% sccMotorList);
    
    # Summarize to obtain total PM2.5 emissions for each year
    totalEmsPerYear <- subNeiData %>% group_by(year)  %>% summarise(totalemissions = sum(Emissions)/1000);

    # Launch PNG device to generate the PNG file with the required plot
    png(file = "plot5.png", bg = "white");
    
    pl <- ggplot(totalEmsPerYear, aes(year, totalemissions), group=1) +
            geom_line() +
            geom_point() +
            coord_cartesian(xlim=c(1998, 2009)) +
            scale_x_continuous(breaks=c(1999, 2002, 2005, 2008)) +
            xlab("Year") + 
            ylab("Total PM2.5 Emissions (kilotons)") +
            ggtitle("PM2.5 Emissions from Motor Vehicle sources in Baltimore City, MD");
    
    print(pl);

    # closing the PNG device
    dev.off();
    
    print("Plot generated on file: plot5.png");
}