library(googlesheets)
dieathlon <- gs_title("dieathlon2.xlsx")
# List worksheets
gs_ws_ls(dieathlon)

########    CARDS      ########

CardBasic <- gs_read(ss=dieathlon, ws = "CardBasic")
CardBasic$Wind[is.na(CardBasic$Wind)]<-""
#colnames(CardBasic)[5]<-"AthleteID" # TEMP SOLUTION until internet works again
CardInfo <- gs_read(ss=dieathlon, ws = "CardInfo")
Cards <- left_join(CardBasic[CardBasic$uCardID<=CardsToInclude,],CardInfo)
Cards$CardName <- as.factor(paste(Cards$Name,"-",Cards$Part))

Cards$UF <- with(Cards,U+F)
Cards$UFD = with(Cards,U+F+D)

CardMaxMove <- max(Cards$UFD)

########    ATHLETES      ########

AthleteBasic <- gs_read(ss=dieathlon, ws = "AthleteBasic")
AthleteInfo <- gs_read(ss=dieathlon, ws = "AthleteInfo")
xaxis = c("Initiative", "Energy", "Up","Flat","Down","Mood")
xaxis <- factor(xaxis, levels = c("Initiative", "Energy", "Up","Flat","Down","Mood"))

a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 1,
  ticklen = 10,
  tickwidth = 2,
  tickcolor = toRGB("blue"),
  range = c(0,10)
)

#######################

library(shiny)
library(shinydashboard)
library(plotly)

athletes <- 12
abxCards <- CardBasic[CardBasic$CardType %in% c("A","B","X"),c(-8)]
abCards <- abxCards[abxCards$CardType %in% c("A","B"),c(-1,-2)]
xCards <- abxCards[abxCards$CardType=="X",c(-5,-6)]

all <- cbind(rep(abCards[abCards$CardType=="A",]$Initiative,12),rep(1:athletes,each=7))
colnames(all) <- c("Initiative","AthleteID")
dotplotdata <- rbind(xCards[,c("Initiative","AthleteID")],all)

#dpd <- dotplotdata[order(dotplotdata$AthleteID),]
dpd <- dotplotdata[!substring(dotplotdata$Initiative,1,3) %in% c("max","min","RED"),]
dpd$Initiative <- as.integer(dpd$Initiative)
dpd$AthleteID <- as.integer(dpd$AthleteID)
wName <- left_join(dpd,AthleteBasic[,c("AthleteID","Surname")],by="AthleteID")
wName$Surname <- as.factor(wName$Surname)

library(lattice)

####################################### SERVER #######################################

server <- function(input, output) {
  # A & B Cards
  # CardBasic
  output$initiative <- renderText(paste0(abCards[abCards$uCardID==input$slider,]$Initiative))
  output$name <- renderText(paste0(abCards[abCards$uCardID==input$slider,]$Name))
  
  output$athlete <- renderText(paste0(AthleteBasic[AthleteBasic$AthleteID==abCards[abCards$uCardID==input$slider,]$AthleteID, ]$Surname ))
  output$cardtype <- renderText(paste0(abCards[abCards$uCardID==input$slider,]$CardType))
  output$cardid <- renderText(paste0(input$slider))
  output$text <- renderText(paste0(abCards[abCards$uCardID==input$slider,]$Text))
  
  # CardInfo - TOP
  output$tUp <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$U ))
  output$tFlat <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$F ))
  output$tDown <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$D ))
  output$tLost <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Lost ))
  output$tEnergy <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Energy,CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Lost))
  
  # CardInfo - BOTTOM
  output$bUp <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$U ))
  output$bFlat <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$F ))
  output$bDown <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$D ))
  output$bLost <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$Lost ))
  output$bEnergy <- renderText(paste0( CardInfo[CardInfo$uCardID==input$slider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$Energy ))

  # X Cards
  output$xinitiative <- renderText(paste0(xCards[xCards$AthleteID==input$athSlide & xCards$CardID==input$xslider,]$Initiative))
  output$xname <- renderText(paste0(xCards[xCards$AthleteID==input$athSlide & xCards$CardID==input$xslider,]$Name))
 
  output$xathlete <- renderText(paste0(AthleteBasic[AthleteBasic$AthleteID==input$athSlide, ]$Surname ))
  #output$xcardtype <- renderText(paste0(xCards[xCards$AthleteID==input$athSlide & xCards$CardID==input$xslider,]$CardType))
  output$xcardid <- renderText(paste0(input$xslider))
  #output$xtext <- renderText(paste0(xCards[xCards$AthleteID==input$athSlide & xCards$CardID==input$xslider,]$Text))
  
  # CardInfo - TOP
  #output$xtUp <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$U ))
  #output$xtFlat <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$F ))
  #output$xtDown <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$D ))
  #output$xtLost <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Lost ))
  #output$xtEnergy <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Energy,CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Top" & CardInfo$Version == 1,]$Lost))
  
  # CardInfo - BOTTOM
  #output$xbUp <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$U ))
  #output$xbFlat <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$F ))
  #output$xbDown <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$D ))
  #output$xbLost <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$Lost ))
  #output$xbEnergy <- renderText(paste0( CardInfo[CardInfo$CardID==input$xslider & CardInfo$Part=="Bottom" & CardInfo$Version == 1,]$Energy ))
  output$xwind <- renderText(paste0(xCards[xCards$AthleteID==input$athSlide & xCards$CardID==input$xslider,]$Wind))
  
  # AthleteInfo
  output$aName <- renderText(paste0(AthleteBasic[AthleteBasic$AthleteID==input$athSlide,]$Surname))
  output$Stats <- renderTable( AthleteInfo[AthleteInfo$AthleteID==input$athSlide,-1], striped=TRUE, hover=TRUE, bordered=TRUE)
  
  #marker = list(color = c('rgba(204,204,204,1)', 'rgba(222,45,38,0.8)',
  #                        'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
  #                        'rgba(204,204,204,1)')))
  
  output$p <- renderPlotly({ plot_ly(
    x = xaxis,
    y = as.numeric(AthleteBasic[input$athSlide,4:9]),
    name = "Skills Intention",
    type = "bar",
    marker = list(color = c('red', 'blue',
                            'black', 'green',
                            'yellow','white'),line = list(color = 'rgb(8,48,107)', width = 1.5)
    )) %>%
    layout(
        yaxis = a
    )
  })
  
  output$plot <- renderPlot({ 
    bwplot(Initiative ~ Surname,  data = wName,
           xlab = "Athlete", ylab = "Initiative",pch=19,col="red")
  })
  
  output$flag <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('./images',
                                       
                                      paste(AthleteBasic[input$athSlide,]$Country, '.png', sep='')))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
}

####################################### HEADER #######################################

header <- dashboardHeader(
  title = "dieAT"
)

####################################### SIDEBAR ######################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("A&B Cards", tabName = "cards", icon = icon("id-card")),
    menuItem("Athletes", tabName = "athletes", icon = icon("id-badge")),
    menuItem("Athlete Comparison", tabName = "compare", icon = icon("users"))
  )
)

######################################## BODY ########################################

body <- dashboardBody(
  #tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  #),
  tags$style(".small-box.bg-teal { background-color: #FFFFFF !important; color: #000000 !important; }"),
  tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
  tabItems(
    tabItem("cards",
            fluidRow(
            column(width = 3,
                   box(
                     width=NULL,
                
                   sliderInput("slider", "Select Card ID:", 1, 35, 1)  ),
                   box( title="Why Cool",width=NULL,
                        uiOutput("text")   
                   )
            ),
            column(width = 9,
            # valueBoxes
            fluidRow(
              valueBox( uiOutput("initiative"), "", color = "red" ),
              valueBox( uiOutput("name"), "", color = "teal"),
              valueBox( "", "", color = "teal" )
            ),
            hr(),
            fluidRow(
              valueBox( uiOutput("tUp"), "", color = "black" )
            ),
            fluidRow(
              valueBox( uiOutput("tFlat"), "", color = "green" ),
              valueBox( "", uiOutput("tLost"), color = "teal" ),
              valueBox( uiOutput("tEnergy"), "", color = "blue" )
            ),
            fluidRow(
              valueBox( uiOutput("tDown"), "", color = "yellow" )
            ),
            hr(),
            fluidRow(
              valueBox( uiOutput("bUp"), "", color = "black" )
            ),
            fluidRow(
              valueBox( uiOutput("bFlat"), "", color = "green" ),
              valueBox( "", uiOutput("bLost"), color = "teal" ),
              valueBox( uiOutput("bEnergy"), "", color = "blue" )
            ),
            fluidRow(
              valueBox( uiOutput("bDown"), "", color = "yellow" )
            ),
            hr(),
            fluidRow(
              valueBox( "", uiOutput("cardid"), color = "teal" ),
              valueBox( uiOutput("cardtype"), "", color = "teal" ),
              valueBox( "", uiOutput("athlete"), color = "teal" )
            )))
    ),
    tabItem("athletes",
            fluidRow(
              column(width = 6,
                         box(
                           title = "Athlete", width = NULL, solidHeader = TRUE, status = "primary",collapsible=TRUE,
                           fluidRow(
                            column(width=6,sliderInput("athSlide", "Select Athlete ID:", 1, 12, 1)),
                            #column(width=3,plotOutput("flag",height="10px")),
                            column(width=6,h2(uiOutput("aName")))
                            ),
                            fluidRow(
                                plotlyOutput("p",width="80%",height="200px")
                            )
              ), box(
                title = "Shooting & Technique", width = NULL, solidHeader = TRUE, status = "primary",
                tableOutput("Stats"))
              ),
              column(width = 6,
                     box(
                       title = "X Cards", width = NULL, solidHeader = TRUE, status = "primary",
                       sliderInput("xslider", "Select X Card No:", 1, 7, 1),
                       
                       # valueBoxes
                       fluidRow(
                         valueBox( uiOutput("xinitiative"), "", color = "red" ),
                         valueBox( uiOutput("xname"), "", color = "teal"),
                         valueBox( "xPLA", "", color = "teal")
                       ),
                       hr(),
                       fluidRow(
                         valueBox( "xtUp", "", color = "black" )
                       ),
                       fluidRow(
                         valueBox( "xtFlat", "", color = "green" ),
                         valueBox( "", "Ansigt", color = "teal" ),
                         valueBox( "xtEnergy", "", color = "blue" )
                       ),
                       fluidRow(
                         valueBox( "xtDown", "", color = "yellow" )
                       ),
                       hr(),
                       fluidRow(
                         valueBox( "xbUp", "", color = "black" )
                       ),
                       fluidRow(
                         valueBox( "xbFlat", "", color = "green" ),
                         valueBox( "",  plotOutput("flag",height="10px"), color = "teal" ),
                         valueBox( "xbEnergy", "", color = "blue" )
                       ),
                       fluidRow(
                         valueBox( "xbDown", "", color = "yellow" )
                       ),
                       hr(),
                       fluidRow(
                         valueBox( "", uiOutput("xcardid"), color = "teal" ),
                         valueBox( "X", "", color = "teal" ),
                         valueBox( "", uiOutput("xwind"), color = "teal" )
                       )
                     )
                     
                    
              )
              
              
            )),
    tabItem("compare",
            plotOutput("plot",height="800px")
            )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
  #,options = list(height = 1080)
)