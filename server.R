#
# This application is using data from Statistics Canada
# to show the population growth and distribution by province in Canada
# in each census year.

library(shiny)

#Get data
dfile = "populationCanada.csv"
if (!file.exists(dfile)) {
#    download.file(fileURL ,dfile,method="auto") 
x <- read.csv(url("http://www.statcan.gc.ca/pub/11-516-x/sectiona/A2_14-eng.csv"))

#Tidy data
dp<-x[,c(3,5,6,8,9,10,11,12,14,16,18,20,21)]
names(dp)<-c("Canada","Newfoundland","Prince Edward Island","Nova Scotia","New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan","Alberta","British Columbia","Yukon","Northwest Territories")
dpfinal<-dp[c(7,9:13),]
dpfinal$Year<-c(1976,1971,1966,1961,1956,1951)
dpfinal[,1:13] <- lapply(dpfinal[,1:13],function(x){as.numeric(gsub(",", "", x))})
#write.csv(dpfinal, file = "populationCanada.csv")
} else {
    dpfinal<-read.csv(dfile)
}

Y<-c("1951","1956","1961","1966","1971","1976")
dpfinal<-dpfinal[order(dpfinal$Year),]
#Dataset with geo sequence from west to east
dpfinalGeo<-dpfinal[,c(1,11,10,9,8,7,6,5,3,4,2,14)]
names(dpfinalGeo)<-c("Canada","BC","AB","SK","MB","ON","QC","NB","PEI","NS","NL","Year")

#Create area data.frame
dpArea<-data.frame(Canada=c(9984670),
                   BC=c(944735),
                   AB=c(661848),
                   SK=c(651900),
                   MB=c(649950),
                   ON=c(1076395),
                   QC=c(1542056),
                   NB=c(72908),
                   PEI=c(5660),
                   NS=c(52942),
                   NL=c(405212),
                   YK=c(35874),
                   NWT=c(3384828)
)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$growthPlot <- renderPlot({
        
        fitmodel <- lm(dpfinal[,input$region]/1000 ~ dpfinal$Year, data = dpfinal)
        
        plot(dpfinal$Year,dpfinal[,input$region]/1000,main=input$region,  xlab = "Year",
             ylab = "Population(x1000)", bty = "n", pch = 16)        
        
        abline(fitmodel, col = "red", lwd = 2)
        
    })
    
    output$barPlotDensity <- renderPlot({
        
        # Render a barplot with average line cross the country
        y<-unlist(dpfinalGeo[dpfinalGeo$Year==input$year,2:11]/dpArea[,2:11])

        barplot(y,ylim=c(min(0,y),max(y)),axes=FALSE)
        #Get population/km2 average by province
        abline(h=mean(y),col="red")
        
        barplot(y,
                main=paste(c("Year of", input$year), collapse = " ") ,
                ylab="Density(/km2)",
                xlab="Province",col = "blue",add=T)
        
        
    })    
    
    output$barPlotPopulation <- renderPlot({
        
        # Render a barplot with average line cross the country
        y<-unlist(dpfinalGeo[dpfinalGeo$Year==input$year,2:11])/1000

        
        barplot(y,ylim=c(min(0,y),max(y)),axes=FALSE)
        #Get population average by province
        abline(h=mean(y),col="red")
        
        barplot(y,
                main=paste(c("Year of", input$year), collapse = " ") ,
                ylab="Population(x1000)",
                xlab="Province",col = "Orange",add=T)

    })    
    
    
})
