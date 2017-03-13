#
# This application is using data from Statistics Canada
# to show the population growth and distribution by province in Canada
# in each census year.

library(shiny)

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
#    write.csv(dpfinal, file = "populationCanada.csv")
} else {
dpfinal<-read.csv(dfile)
}
dpfinal<-dpfinal[order(dpfinal$Year),]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Population of Canada"),
    
    # Sidebar with a slider input for year 
    sidebarLayout(
        sidebarPanel(
            h4("Growth Plot"),

            selectInput("region", "Region:", 
                        choices=colnames(dpfinal[,1:13])),
            hr(),
            helpText("By Province, from 1951 to 1976."),

            hr(),
            h4("Density and Population Plots"),
            
            sliderInput("year",
                        "Year of census:",
                        min = 1951,
                        max = 1976,
                        step = 5,
                        round=TRUE,
                        sep = "",
                        value = 1951),   
            
            hr(),
            p(strong(em("Documentation:",a("Population of Canada(1951-1971)",target="blank",href="README.html")))),
            p(strong(em("Github Repository:",a("Developing Data Products Week4 Project",target="blank",href="https://github.com/jc-coursera/DevelopingDataProducts"))))
        ),
        
        # Show a barplot of population and average by province
        mainPanel(
            mainPanel(
                tabsetPanel(
                    tabPanel("Growth Plot", plotOutput("growthPlot")),
                    tabPanel("Desity and Population Plots", plotOutput("barPlotDensity"),plotOutput("barPlotPopulation"))
                )
            )            

        )
    )
))
