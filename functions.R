timePlotly<-
  function (x)
  {
    library(ggplot2)
    library(dplyr) 
    library(plotly)
    x$Tweet_PostDate <- as.Date( x$postedTime)
    x$Tweet_Count <- 1
    by_time <- group_by(x, Tweet_PostDate)
    time.count <- summarise(by_time,Tweet_Count = sum(Tweet_Count))

    xtitle <- list(
      title = "Tweet Posted Date"
    )
    ytitle <- list(
      title = "Daily Tweet Count"
    )
    
    xmas <- filter(time.count, Tweet_PostDate == "2015-12-25")
    snow <- filter(time.count, Tweet_PostDate == "2016-01-22")
    sat <- filter(time.count, as.character(Tweet_PostDate) %in% c("2015-12-12","2015-12-19","2016-01-09","2016-01-16"))
    
      p <- plot_ly(time.count, x = Tweet_PostDate, y = Tweet_Count, name = "Daily Tweet Count")
      p %>% add_trace(y = fitted(loess(time.count$Tweet_Count ~ as.numeric(time.count$Tweet_PostDate))), 
                      x = time.count$Tweet_PostDate, name = "Fitted Average") %>%
                       layout(title = "Daily Tweet Count", autosize = F, width = 800, height = 600, legend = list(x = .5, y = .9),
                              xaxis = xtitle, yaxis = ytitle, annotations = list(
                                list(
                                  x = xmas$Tweet_PostDate,
                                  y = xmas$Tweet_Count,
                                  text = "Xmas",
                                  showarrow = T,
                                  ax = -5,
                                  ay = 50
                                ),
                                list(
                                  x = snow$Tweet_PostDate,
                                  y = snow$Tweet_Count,
                                  text = "Snow Wknd",
                                  showarrow = T,
                                  ax = 0,
                                  ay = -50
                                ),
                                list(
                                  x = as.Date(sat$Tweet_PostDate[1]),
                                  y = sat$Tweet_Count[1],
                                  text = "Saturday",
                                  showarrow = T,
                                  ax = 8,
                                  ay = -75
                                ),
                                list(
                                  x = as.Date(sat$Tweet_PostDate[2]),
                                  y = sat$Tweet_Count[2],
                                  text = "Saturday",
                                  showarrow = T,
                                  ax = 15,
                                  ay = -50
                                ),
                                list(
                                  x = as.Date(sat$Tweet_PostDate[4]),
                                  y = sat$Tweet_Count[4],
                                  text = "Saturday",
                                  showarrow = T,
                                  ax = 0,
                                  ay = -50
                                )
                              ))
  }


weekPlotly<-
  function (x)
  {
    library(ggplot2)
    library(dplyr)
    library(RColorBrewer)
    library(plotly)

    x$Tweet_PostDate <- as.Date(x$postedTime)
    x$Tweet_DOW <- as.factor(weekdays(x$Tweet_PostDate))
    x$Tweet_Week <- format(x$Tweet_PostDate+3, "%U")
    x$Tweet_Count <- 1
    
    by_time <-group_by(x,Tweet_DOW,Tweet_Week)
    time.count <- summarise(by_time,Tweet_Count = sum(Tweet_Count))
    
    time.count$Tweet_Week <- factor(time.count$Tweet_Week, 
                                    levels= c("48","49","50","51","52",
                                              "00","01","02","03","04",
                                              "05","06","07","08","09"))
    
    time.count <- time.count[order(time.count$Tweet_Week), ]
    time.count$Tweet_DOW <- factor(time.count$Tweet_DOW, levels= c("Sunday","Saturday","Friday",
                                                                   "Thursday","Wednesday","Tuesday","Monday"))
    time.count <- time.count[order(time.count$Tweet_DOW), ]
    xtitle <- list(
      title = "Tweet Post Day of the Week"
    )
    ytitle <- list(
      title = "Daily Tweet Count"
    )
    
    plot_ly(time.count, y = Tweet_Count, color = Tweet_DOW, type = "box", name = "Tweet Count") %>%
      layout(title = "Day-of-the-Week Tweet Count", autosize = F, width = 800, height = 600, xaxis = xtitle, yaxis = ytitle)
  }

hourPlotly<-
  function (x)
  {
    library(ggplot2)
    library(dplyr) 
    library(plotly)
    x$hour <- as.numeric(substr(x$postedTime,12,13))
    x$Tweet_PostDate <- as.Date(x$postedTime)
    x$Tweet_DOW <- as.factor(weekdays(x$Tweet_PostDate))
    x$Tweet_Count <- 1
    
    time.count <- x %>% group_by(hour,Tweet_DOW) %>% summarise(Tweet_Count = sum(Tweet_Count)/13)
    
    time.count$Tweet_DOW <- factor(time.count$Tweet_DOW, levels= c("Sunday","Saturday","Friday",
                                                                   "Thursday","Wednesday","Tuesday","Monday"))
    time.count <- time.count[order(time.count$Tweet_DOW), ]
    xtitle <- list(
      title = "Tweet Post Hour of the Day (24 Hour Time)"
    )
    ytitle <- list(
      title = "Daily Tweet Count"
    )
    
    p <- plot_ly(time.count, x = hour, y = Tweet_Count, color = Tweet_DOW, name = "Hourly Tweet Count",
                 mode = "lines")
    p %>%  layout(autosize = F, width = 800, height = 600, legend = list(x = 0.2, y = .8), 
                  title = "Hourly Tweet Count", xaxis = xtitle, yaxis = ytitle)
  }

# used from Pablo Barbera's GitHub

getCommonHashtags <- function(text, n=20){
  hashtags <- regmatches(text, gregexpr("#(\\d|\\w)+",text))
  hashtags <- unlist(hashtags)
  tab <- table(hashtags)
  return(head(sort(tab, dec=TRUE), n=n))
}

getCommonHandles <- function(text, n=20){
  handles <- regmatches(text, gregexpr('@([0-9_A-Za-z]+)',text, perl=TRUE))
  handles <- unlist(handles)
  tab <- table(handles)
  return(head(sort(tab, dec=TRUE), n=n))
}

heatmapPlot <-
  function (x, zoom)
  {
  library(ggplot2)
  library(ggmap)
  library(RColorBrewer)
  library(ggrepel)  
    
    if (zoom == "Charlotte") {
        CltMap <- qmap("charlotte", zoom = 11)}
    else if (zoom == "Uptown") {
        CltMap <- qmap("charlotte", zoom = 13)
    }
    else CltMap <- qmap("charlotte", zoom = 10)
    
  xx <- x[!is.na(x$point_long),]
  
  cltbreweries <- data.frame(c("Birdsong","Unknown","Triple C", "Craft", "Salud","Sycamore","Beer Growler",
                               "NoDa (Old)","NoDa (New)","Legion","Wooden Robot","Olde Meck","Heist","Sugar Creek","Growlers PH"),
                    c(35.230652,35.220506,35.201092,35.220019,35.247799,35.208722,35.214912,
                      35.240348,35.251570,35.218387,35.216677,35.187449,35.245468,35.185429,35.246781),
                    c(-80.826536,-80.857539,-80.869570,-80.856875,-80.804048,-80.862762,-80.853231,
                      -80.814849,-80.812472,-80.812963,-80.856644,-80.881815,-80.809440,-80.881015,-80.805798))
  
  colnames(cltbreweries) <- c("Name","Lat","Long") 
  
  
  CltMap <- CltMap + 
    stat_density2d(data = xx,
      aes(x = point_long, y = point_lat, fill=..level.., alpha = ..level..),
      size = 0.5, bins = 16, geom = "polygon"
    ) + theme(legend.position="none")
  CltMap <- CltMap + scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.5), guide = FALSE)
  CltMap <- CltMap +
    geom_point(aes(x = point_long, y = point_lat),
               size = 0.5,
               data = xx)
  if (zoom == "Uptown"){
  CltMap + 
    geom_point(aes(x = Long, y = Lat),
               shape = 21,
               fill  = "white",
               colour = "black",
               size = 3,
               data = cltbreweries) + geom_text(aes(label = "Brewery")) +
    geom_text_repel(data = cltbreweries, aes(x = Long, y = Lat, label = Name), 
              size = 3, fontface = "bold")
  }
  else 
  CltMap
  }

barChartTotal <- function(dataset){
  library(RColorBrewer)
  barplot(dataset$count, main="Beer-Related Tweets by Geo-Type",
          xlab="Geo-location Type",
          names.arg = dataset$geo.type)
}

plotlyBarChart <- function(dataset){
library(plotly)
  x <- list(
    title = "Geo-Type"
  )
  y <- list(
    title = "Tweet Count"
  )

p <- plot_ly(
  x = dataset$geo.type,
  y = dataset$count,
  name = "Geo-Type Count",
  type = "bar",
  color = dataset$geo.type) 
p %>% layout(title = "Count by Geo-Type Tweet", autosize = F, width = 400, height = 300, xaxis = x, yaxis = y, 
             legend = list(x = 0.75, y = 1))
}


pieChart <- function(dataset){
  library(RColorBrewer)
  pielabels <- round(100*dataset$count/sum(dataset$count), 1)
  pielabels <- paste(pielabels, "%", sep="")
  pielabels <- paste(dataset$geo.type,"\n",pielabels)
  pie(dataset$count, labels = pielabels, 
    main = "Percent of Beer Tweets by Geo-Type", col = brewer.pal(6,"Set1"))
}

convertBreweries <- function(dataset){
  count <- dataset[order(dataset)]
  topplaces <- as.data.frame(count)
  topplaces$type <- "CLT Breweries"
  topplaces[c("@wickedweedbeer","@foundersbrewing","@foothillsbeer"),2] <- "Non-CLT Breweries"
  topplaces[c("@ncbeertemple","@saludnoda","@craftgrowler","@goodbottleco"),2] <-"Bar/Growler Shops"
  return(topplaces)
  }

barChartHandle <- function(dataset, font_size = 12){
  library(ggplot2)
  library(plotly)
  theme_set(theme_gray(base_size = font_size))
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE
  )
  
  m = list(
    l = 200,
    r = 0,
    b = 50,
    t = 50,
    pad = 0
  )

  p <- ggplot(dataset, aes(x = reorder(rownames(dataset),count), y = count,  fill=type)) + 
    geom_bar(stat = "identity") + 
    labs(x = " ",
         y = " ") + 
    theme(axis.ticks = element_blank()) + 
    coord_flip() +
    geom_text(aes(label=count), vjust=0, size = 3) +
    theme(legend.position="bottom") + 
    theme(legend.title=element_blank())
  ggplotly(p) %>% layout(title = "Number of Twitter Mentions by Brewery or Bar", autosize = F, width = 800, height = 600, legend = list(x = .5, y = .5),
                         xaxis = ax, yaxis = ax, margin = m)
  
  
  }

untappd.list <- function(tweets){
  
  clean <- gsub('Drinking an','Drinking a',tweets)
  
  pos <- regexpr('Drinking a', clean)
  
  clean2 <- substr(clean,pos,length(clean))
  
  sub <- gsub('.*\\ba\\b(.*) by @(.*) @(.*) https.*','\\1|\\2|\\3|\\4',clean2)
  
  temp <- strsplit(sub,'\\|')
  
  max_length <- max(unlist(lapply(temp,length)))
  temp_data <- lapply(temp,function(x) {ans <- rep(NA,length=max_length);
  ans[1:length(x)]<- x;
  return(ans)})
  data <- do.call(rbind,temp_data)
  data <- as.data.frame(data)
  colnames(data) <- c("Beer","Brewer","Bar")
  
  col <- grepl("Drinking a ", tweets)
  col2 <- grepl("by @", tweets)
  data$Brewer <- gsub(" at","",data$Brewer)
  data$Brewer <- gsub("@","",data$Brewer)
  for (i in colnames(data)){
    data[,i] <- gsub("&amp;","&", data[,i])
  }
  
  col <- which(col + col2 == 2)
  data <- data[col,]
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  for (i in colnames(data)){
    data[,i] <- trim(data[,i])
  }
  return(data)
}


shiny_barchart <- function(dataset) { 
  
  require(shiny)  
  library(RColorBrewer)
  shinyApp(
    ui = fluidPage(
      # Give the page a title
      titlePanel("Top Beers Referenced by Brewery"),
      fluidRow(style = "padding-bottom: 20px;",
               column(4, selectizeInput('xcol', 'Select a Brewery', dataset$Brewer, 
                                        options = list(placeholder = "nodabrewing")))
      ),
      fluidRow(
        plotOutput('bars', height = "400px")  
      )
    ),
    
    server = function(input, output, session) {
      
      # Combine the selected variables into a new data frame
        output$bars <- renderPlot(height = 400, width = 600, {
          par(mar=c(5,20,4,2))
          # Render a barplot
          if(!is.null(input$xcol)){
          barplot(rev(head(dataset$Count[dataset$Brewer == input$xcol],8)), 
                  names.arg = rev(head(dataset$Beer[dataset$Brewer == input$xcol],8)),
                  main=input$xcol,
                  ylab=" ",
                  xlab="Number of Untappd Tweets",
                  col = brewer.pal(8,"Dark2"),
                  horiz = TRUE,
                  las = 2
                  )
          }
      })
    },
    
    options = list(height = 500)
  )
}


