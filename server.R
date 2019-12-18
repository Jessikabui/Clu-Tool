# VErsión 7
# S-T Clustering visualization
# Made by: Jessika Buitrago inspired by https://github.com/fitrahmunir/Web-Clustering-ST-DBSCAN

# libraries
library(ggplot2)
library(DT)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(rpivotTable)
library(tidyverse)
library(scales)
library(Cairo)
library(leaflet.extras)
library(dplyr)


options(shiny.maxRequestSize=2000*1024^2) # file size limit 

# functions
source("stdbscan.R")

# server.R
shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  
  selectedData <- reactive({
 
      req(input$file1)
      tryCatch(
        {
          klm= data <- read.csv(input$file1$datapath, sep=";")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    latitude <- data[grep("lat|LAT", names(data))]
    longitude <- data[grep("long|LONG", names(data))]
    data <- data[,!names(data) %in% c("latitude","longitude"),drop=FALSE]
    colnames(latitude) <-"latitude"
    colnames(longitude) <-"longitude"
    data <- data.frame(data,latitude,longitude)
    
    assign("data", data, envir = .GlobalEnv)
    
    
   })


  cluster <- reactive({
    stdbscan(selectedData(), input$eps1, input$eps2, input$minPts, input$distance)
  })
  
  output$plot <- renderLeaflet({
        selectedData()
        res <- stdbscan(data, input$eps1,input$eps2, input$minPts, input$distance) # apply function #

        # getting numeric columns
        nums <- unlist(lapply(data, is.numeric))
        data_uno <- data[ , nums]
        # In case there's no numeric columns
        None <- (seq(10,10,length.out=dim(data)[1]))
        dataValues <- data.frame(None,  data_uno[ , !(names(data_uno) %in% c("longitude", "latitude", "date")), drop=FALSE])
        assign("dataValues", dataValues, envir = .GlobalEnv)


        # data without numeric columns
        dataNew_1<- data.frame(longitude=data$longitude, latitude=data$latitude, clusters=res$cluster, date=data$date, date2=as.Date(data$date,origin="30/12/1899",tryFormats = c("%d-%m-%Y","%d/%m/%Y")))
        idlocal <- rownames(dataNew_1)
        id <- data[grep("cod|id", names(data))]
        if (dim(id)[2]==0){
          id <- data.frame(idlocal)
        }
        colnames(id) <- "id"
        id <- id[,1] 
        #create day of week
        week_day <- weekdays(as.Date(dataNew_1$date2))
        
        # data with numeric columns
        dataNew_2 <- data.frame(idlocal,id, dataNew_1, dataValues)
        assign("dataNew_2", dataNew_2, envir = .GlobalEnv)

        # complete data with clusters
        dataNew_5 <- data.frame(clusters=res$cluster, id,date2=as.Date(data$date, origin="30/12/1899", tryFormats = c("%d-%m-%Y","%d/%m/%Y")), data,week_day,None)
        assign("dataNew_5", dataNew_5, envir = .GlobalEnv)

        # data without noise 
        dataNew_4 <- dataNew_5[dataNew_5$clusters!=0,]
        assign("dataNew_4", dataNew_4, envir = .GlobalEnv)
    
 
    ## time cluster
    time_class314<- unique(res$cluster)
    assign("time_class314", time_class314, envir = .GlobalEnv)

    ## time cluster color
    class314_col<-rainbow(length(time_class314))
    assign("class314_col", class314_col, envir = .GlobalEnv)

    ##Cluster
    b<-0
    for (i in 1: length(unique(dataNew_2$clusters))){
      a<-nrow(data[dataNew_2$clusters==i,])
      if(a >= 30){
        b<- b+1
      }
    }
    assign("cb", b, envir = .GlobalEnv)

    ## Total Cluster
    max<-length(time_class314)
    max<- max-1
    assign("total", max, envir = .GlobalEnv)
    ## Noise info
    noise <- length(data$latitude[dataNew_5$clusters==0])
    assign("noise", noise, envir = .GlobalEnv)
    
    outTable_ll <- reactive({
      input$eps1
      input$eps2
      input$minPts
      input$distance
      req(input$file1,
          klm = dataNew_2 <- dataNew_2[]
      )
      return(dataNew_2)
    })

    output$table_ll <- renderDataTable(outTable_ll())
    
    # update slider reactive - size value reactive
      max = round(dim(dataNew_5)[1]/100,0)
      maxt = round((max(dataNew_5$date)-min(dataNew_5$date))/3,0)
      min_time = min(dataNew_5$date2)
      max_time = max(dataNew_5$date2)
      
      updateSliderInput(session, "eps2", max=maxt)
      updateSliderInput(session, "minPts", max=max)
      updateSelectInput(session, "size", choices = names(dataValues))
      updateSliderInput(session, "range", min=min_time,max=max_time, value=range(min_time,max_time))
      
      
      
      ##### map #####
      leaflet(outTable_ll()) %>% addTiles(group = "OpenStreetMap") %>%
        addFullscreenControl(pseudoFullscreen = TRUE)%>%
        fitBounds(~min(longitude), ~min(latitude),
                ~max(longitude), ~max(latitude)) #%>%
      
      })
  

 observe({
    outTable_ll <- reactive({
      input$eps1
      input$eps2
      input$minPts
      input$distance
      req(input$file1,
          klm = dataNew_5 <- dataNew_5[]
      )
      return(dataNew_5)
    })

    output$table_ll <- renderDataTable(outTable_ll())
   
 
    
      sliderData1 <- reactive({
      outTable_ll()[outTable_ll()$date2 >= input$range[1] & outTable_ll()$date2 <= input$range[2],]
      })
     output$sliderData1 <- renderDataTable(sliderData1())
    
   

    sizeBy <- input$size
    radius <- sliderData1()[[sizeBy]]
    
    
    colorData <- sliderData1()$clusters
    gro <- sprintf("%.0f", colorData)
    date_pop <-  sliderData1()$date2
    pal <- colorFactor("magma", colorData)
    id_n <- sliderData1()$idlocal
    id_nn <- sliderData1()$id
    
  leafletProxy("plot", data =sliderData1()) %>%
    clearShapes() %>%
            addCircles(~longitude,
                       ~latitude,
                        color=~pal(colorData),
                        label=paste("Date:", date_pop,"Cluster:",gro,
                                    "Row:",id_n,"ID:",id_nn),
                        radius=radius,
                        group = gro)%>%
            addLayersControl(overlayGroups = gro,
                             options = layersControlOptions(collapsed = FALSE))

  
})

  
  outTotal <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
           klm = total <- total[]
           )
    return(total)
  })

  outNoise <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
           klm = noise <- noise[]
           )
    return(noise)
  })

  outBigCluster <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
           klm = big <- cb[]
           )
    return(big)
  })

  outSmallCluster <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
           klm = small <- total - cb[]
           )
    return(small)
  })

  output$total <- renderValueBox({
    valueBox(
      outTotal(), "Total Cluster", icon = icon("info-circle"),
      color = "light-blue"
    )
  })

  output$noise <- renderValueBox({
    valueBox(
      outNoise(), "Total Noise", icon = icon("info-circle"),
      color = "red"
    )
  })

  output$bigCluster <- renderValueBox({
    valueBox(
      outBigCluster(), "Big Cluster", icon = icon("info-circle"),
      color = "light-blue"
    )
  })

  output$smallCluster <- renderValueBox({
    valueBox(
      outSmallCluster(), "Small Cluster", icon = icon("info-circle"),
      color = "light-blue"
    )
  })
  ################################################################################
  ############### Tab 4 - Table with clusters
  outTable <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
           klm = dataNew_5 <- dataNew_5[, !(names(dataNew_5) %in% c("date"))]
           )
    return(dataNew_5)
  })

  output$table <- renderDataTable(outTable(),options = list(scrollX = TRUE))

  ## Download button
  output$downloadData <- downloadHandler(
        filename = function() {
              paste(input$file1, "", sep = "")
        },
        content = function(file) {
              write.csv2(outTable(), file, row.names = FALSE, fileEncoding = "latin1")
        }
  )

  ################################################################################
  ################ Tab 2 - Plotting clusters histogram
  output$hist <- renderPlot({
    
    min_date <- setNames(aggregate(date2 ~ clusters, data = outTable(), min),c("cluster","Start_date"))
    max_date <- setNames(aggregate(date2 ~ clusters, data = outTable(), max),c("cluster","End_date"))
    sum_date <- setNames(aggregate(date2 ~ clusters, data = outTable(), length),c("cluster","Count"))
    date <- cbind(min_date,max_date,sum_date)
    date1 <- date[-1,-c(3,5)]
    date1$cluster <- as.factor(date1$cluster)
    date1$Count <- as.factor(date1$Count)
    
    g.gantt <- gather(date1, "state", "date", 2:3) %>% 
      mutate(date = as.Date(date, "%Y.%m.%d"))
    assign("g.gantt", g.gantt, envir = .GlobalEnv)
    
    ggplot(g.gantt, aes(date, cluster,color=cluster, group=cluster)) +
      geom_line(size = 10) +theme_classic()+
      labs(x="Date", y="Clusters", title="Timeline and frequency of Clusters")+
      scale_x_date(date_breaks="6 months", labels=date_format("%b - %y"))+
      geom_text(data = g.gantt, aes(x = date, y = cluster, 
                                    label =Count, 
                                    color = cluster, 
                                    hjust = 1, vjust = 1)) +
      theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))
  })
  

  
  #### boxes total and noise 2
  output$total2 <- renderValueBox({
    valueBox(
      outTotal(), "Total Cluster", icon = icon("info-circle"),
      color = "light-blue"
    )
  })
  output$noise2 <- renderValueBox({
    valueBox(
      outNoise(), "Total Noise", icon = icon("info-circle"),
      color = "red"
    )
  })
################################################################################
################ Tab 2 - Plotting with brushed points - without noise
  
  
  outTable_o <- reactive({
    input$eps1
    input$eps2
    input$minPts
    input$distance
    req(input$file1,
        klm = dataNew_4 <- dataNew_4[, !(names(dataNew_4) %in% c("id_local","date"))]
    )
    
    return(dataNew_4)
  })
  output$table_o <- renderDataTable(outTable_o())
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$cluplot <- renderPlot({
    ggplot(outTable_o(), aes(x = longitude, 
                             y = latitude, 
                             color=factor(id),
                             shape=factor(clusters),
                             group=interaction(factor(clusters),factor(id)))) +
      ggtitle("Plot of clusters and ID") +
      geom_point( size=3) +
      scale_shape_manual(values=1:max(outTable_o()$clusters))+
      theme(legend.key=element_blank(), 
            legend.key.size=unit(1,"point"),
            legend.direction = "vertical", 
            legend.box = "horizontal" )+
      guides(colour=guide_legend(nrow=30),fill=guide_legend(title="ID"))
  })
   
  foo <- reactive({
      user_brush <- input$plot_brush
      brushedPoints(outTable_o(), user_brush, xvar = "longitude", yvar = "latitude")
    })

    output$info<-DT::renderDataTable({DT::datatable(foo())})
    # plot zoom
  
    output$plot3 <- renderPlot({
      ggplot(outTable_o(), aes(x = longitude,
                               y = latitude,
                               color=factor(id),
                               shape=factor(clusters),
                               group=interaction(factor(clusters),factor(id)))) +
        ggtitle("Zoom of points") +
        geom_point( size=3) + 
        theme(legend.position = "none")+
        scale_shape_manual(values=1:max(outTable_o()$clusters))+
        coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    })
    # # When a double-click happens, check if there's a brush on the plot.
    # # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges2$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges2$x <- NULL
        ranges2$y <- NULL
      }
    })
    
##########################################################################################
################ Tab 2 - Plotting boxplot by num columns
    
    observe({
      var.opts<-colnames((outTable_o()[,unlist(lapply(outTable_o(), is.numeric))])  )
      updateSelectInput(session, "metric", choices = var.opts)
    })
    
    output$boxplot <- renderPlot({
      fillit <- input$metric
      fill_ <- outTable_o()[[fillit]]
      ggplot(outTable_o(),aes(x=as.factor(outTable_o()$clusters),
                              y=fill_,fill=as.factor(outTable_o()$clusters))) +
        geom_boxplot()+
        xlab("Clusters") + ylab("Var selected")+
        theme(panel.background = element_blank(),legend.position = "none")
    })
##########################################################################################
################ Tab 3 - pivot table
    
    output$OverallPivot <- renderRpivotTable({
      rpivotTable(data = outTable())
    })
    
 })
##########################################################################################



