library(shiny)
library(fitdistrplus)     #distribution fitting
library(MASS)
library(stats)
library(xts)              #to convert dataframe to xts format for dygraphs use
library(dygraphs)         #time series plot
library(zoo)              #time series plot
library(ggplot2)          #box plot, histogram, scatter plot
#library(xlsx)            #to read .xlsx file (but does not load huge xl file)
library(gdata)            #slow but works with big data also.
#library(openxlsx)        #very fast but error in processing mean etc.
library(lubridate)        #to check is.date
library(reshape2)         #to melt the corelation matrix
library(d3heatmap)        #for heatmap
library(metricsgraphics) #for scatterplot
library(portfolio)          #treemap
library(gridBase)
library(googleVis)

options(shiny.maxRequestSize=30*1024^2) #shiny allows 5 mb upload only. Inc to 30 MB
options(java.parameters = "-Xmx1000m")  #allocate more sapce to read the file. Jave heap error.
shinyServer(function(input, output, session) {

dataUpload = reactive({
    
   infile = input$file
   if (is.null(infile))
     return(NULL)
   infile_read = read.csv(infile$datapath, 1)
   
   numericcol <- c()
   charactercol <- c()
   #dtcol <- c()
   #to update the Column names in ui.R based on excel input
   for (clnames in colnames(infile_read)){
     if (is.numeric(infile_read[,clnames])){
       numericcol <- c(numericcol, clnames)
     }
     else {
       charactercol <- c(charactercol, clnames)
     }
     
   }
   numericcol <- c(numericcol, "None")
   charactercol <- c(charactercol, "None")
   
   updateSelectInput(session , "Xvar", choices = numericcol)
   updateSelectInput(session , "Tvar", choices = numericcol)
   updateSelectInput(session , "Yvar", choices = charactercol)
   updateSelectInput(session , "Zvar", choices = charactercol)
   return(infile_read)
 })


#Show the first 100 selected data in Data tabbn,d
output$tabl <- renderDataTable(
dataUpload(), options=list(pageLength=10)
)

#printing outliers in the Outliers tab

predicted <- reactive ({
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  d <- dataUpload()[,input$Xvar]
  d <- na.omit(d)
  len <- length(d)
  date_type <- seq.Date(from = as.Date(input$dtfrm),
                        by = input$txinp,
                        length.out = len)
  
  df <- cbind.data.frame(firstvar = date_type, secondvar = d)
  #df$firstvar <- as.Date(df$firstvar,format="%Y-%m-%d")
  #df$firstvar <- strptime(df$firstvar, format="%Y-%m")
  qxts <- xts(df[,-1], order.by=df[,1])
  #print(qxts)
  #if (input$txinp == "week"){
  #  freobs <- 356.25/7
  #  st_yr <- as.numeric(format(df$firstvar[1], "%Y"))
    #print(st_yr)
    #st_mn <- as.numeric(format(df$firstvar[1], "%m"))
  #  st_wk <- as.numeric(format(df$firstvar[1], "%W"))
    #st_wk <- as.numeric(format(as.Date(df$firstvar[1]), "%W-%w"))
  #  print(st_wk)

    #dfts <- xts(df$secondvar, seq.Date(from=as.Date(df$firstvar[1]), 
    #                                    by='week', length.out=len, frequency = 52 ))
   
   # dfts <- ts(df$secondvar, frequency = freobs, start = c(st_yr, st_wk))
  #  print(dfts)
  #}
  if (input$txinp == "month") {
    freobs <- 12
    ym <- as.yearmon(df$firstvar[1], format = "%Y-%m-%d")
    #st_yr <- as.numeric(format(df$firstvar[1], "%Y"))
    #st_mn <- as.numeric(format(df$firstvar[1], "%m"))
    #ed_yr <-  as.numeric(format(df$firstvar[len], "%Y"))
    #ed_mn <- as.numeric(format(df$firstvar[len], "%m"))
    dfts <- ts(df$secondvar, frequency = freobs, start = c(ym))
  }
  else if (input$txinp == "quarter"){
    freobs <- 4
    yq <- as.yearqtr(df$firstvar[1], format = "%Y-%m-%d")
    dfts <- ts(df$secondvar, frequency = freobs, start = c(yq))
    #dfts <- zooreg(df$secondvar, start = yq, frequency = freobs)
  }
  
  hw <- HoltWinters(dfts)

  p <- predict(hw, n.ahead = input$unpd, 
          prediction.interval = TRUE,
          level = as.numeric(input$interval))

  merge(actual= as.xts(dfts), predicted= as.xts(p))
  #all.xtx <- xts(all, as.Date(index(all)))
  
  })


output$dygraph <- renderDygraph({
  #if no file selected return NULL
  if (is.null(input$file))
    return(NULL)
  #df <- data.frame(firstvar = dataUpload()[,input$Xvar], secondvar = dataUpload()[,input$Yvar])
  #dfplot <- ts(df$secondvar, frequency = input$freq, start= df$firstvar[1])
  if (input$Xvar == "None" | input$Zvar == "None") {
    return(NULL)
  }
  else {
    dygraph(
      predicted(), main = paste(
        "Forecasting for ", input$Xvar, " over ",
        input$unpd, " ", input$txinp, sep = ""
      )
    ) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("x", label = input$txinp) %>%
      dyAxis("y", label = input$Xvar) %>%
      dySeries(label = paste("Actual ",input$txinp, sep = ""), color = "blue") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Predicted", color = "green")
    #dyOptions()
  }
})


#output$corelplot1 <- renderPlot({
#   #if no file selected return NULL
#   if(is.null(input$file))
#     return(NULL)
#   if (input$Zvar == "None"){
#     return(NULL)  #need group to perform corelation
#   }
#   else {
#     # calculate the mean for each variable using the group (Zvar)
#     cols = sapply(dataUpload(), is.numeric)
#     cols = names(cols)[cols]
#     a <- aggregate(dataUpload()[,c(cols)], list(dataUpload()[, input$Zvar]), 
#                    function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
#   }
#   #print(a)
#   #convert list a to matrix o
#   myData <- t(do.call(rbind,a))
#   # give row names to matrix using the Zvar (group)
#   #rownames(myData) <- names(a$Group.1)
#   #remove columns with all NA values. This will happen if all categorical col.
#   myData <- myData[,colSums(is.na(myData)) == 0]
#   myData <- myData[,-1]
#   #myData <- mtcars[, c(1,3,4,5,6,7)]
#   #print(myData)
#   
#   #create corelation matrix
#   cormat <- round(cor(myData),2)
#   #print(cormat)
#   
#   # Reorder the correlation matrix
#   dd <- as.dist((1-cormat)/2)
#   hc <- hclust(dd)
#   cormat <-cormat[hc$order, hc$order]
#   
#   #get upper_tri
#   cormat[lower.tri(cormat)]<- NA
#   
#   # Melt the correlation matrix
#   melted_cormat <- melt(cormat)
#   #print(melted_cormat)
#   melted_cormat <- na.omit(melted_cormat)
#   
#   # Create a ggheatmap
#   ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
#     geom_tile(color = "white")+
#     scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                          midpoint = 0, limit = c(-1,1), name="Pearson Correlation") +
#     theme_minimal()+ # minimal theme
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                      size = 12, hjust = 1))+
#     coord_fixed()+
#     geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
#     theme(
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.border = element_blank(),
#       panel.background = element_blank(),
#       axis.ticks = element_blank(),
#       legend.justification = c(1, 0),
#       legend.position = c(0.6, 0.7),
#       legend.direction = "horizontal")+
#     guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                                  title.position = "top", title.hjust = 0.5))
# })


# output$coreltext <- renderTable({
#   #if no file selected return NULL
#   if (is.null(input$file))
#     return(NULL)
#   if (input$Zvar == "None") {
#     return(NULL)  #need group to perform corelation
#   }
#   else {
#     # calculate the mean for each variable using the group (Zvar)
#     cols = sapply(dataUpload(), is.numeric)
#     cols = names(cols)[cols]
#     a <-
#       aggregate(dataUpload()[,c(cols)], list(dataUpload()[, input$Zvar]),
#                 function(x)
#                   c(mean = round(
#                     mean(x[is.numeric(x)],na.action = na.pass,na.rm = TRUE),2
#                   )))
#   }
# })


output$corelplot2 <- renderD3heatmap({
  
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    cols = sapply(dataUpload(), is.numeric)
    cols = names(cols)[cols]
    # calculate the mean for each variable using the group (Zvar)
    a <- aggregate(dataUpload()[,c(cols)], list(dataUpload()[, input$Zvar]), 
                   function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
  } 
  #print(a)
  
  #convert list a to matrix o
  myData <- as.data.frame(t(do.call(rbind,a)))
  #print(names(a$Group.1))
  
  # give row names to matrix using the Zvar (group)
  row.names(myData) <- a$Group.1
  #print(myData)
  
  #remove columns with all NA values. This will happen if all categorical col.
  #myData <- myData[,colSums(is.na(myData)) == 0]
  #print(myData)
  
  #remove the first column
  myData <- myData[,-1]
  
  #myData <- as.numeric(myData)
  #print(myData)
  
  "plot the chart"
  d3heatmap(myData, scale = "column", colors = "Greens",cexRow = .9,cexCol = .8, 
            show_grid = TRUE)
  })


output$barplot1 <- renderPlot({
  
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None" | input$Yvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    # calculate the mean for each variable using the group (Zvar)
    a <- aggregate(dataUpload()[,input$Yvar], list(dataUpload()[, input$Zvar]), 
                   function(x) c(length=length(x)))
  } 
  #convert list a to matrix o
  myData <- data.frame(a)
  #print(a)
  
  # give row names to matrix using the Zvar (group)
  row.names(myData) <- a$Group.1
  
  #remove columns with all NA values. This will happen if all categorical col.
  myData <- myData[,colSums(is.na(myData)) == 0]
  #print(myData)
  
  # melt the data frame for plotting
  data.m <- melt(myData, id.vars='Group.1')
  #print(data.m)
  
  #plot bar chart
  ggplot(data.m, aes(x=Group.1, y= value, fill = Group.1)) +
    geom_bar(stat="identity")+
    geom_text(aes(label= value), vjust=1.6, color="white", size=3.5)+
    ggtitle(paste("Count ", input$Yvar, " by ", input$Zvar, Sep = ""))+
    xlab(input$Zvar) + 
    ylab("count")+
    guides(fill=guide_legend(title=NULL))
}) 


output$barplot2 <- renderPlot({
  
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None" | input$Xvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    # calculate the mean for each variable using the group (Zvar)
    a <- aggregate(dataUpload()[,input$Xvar], list(dataUpload()[, input$Zvar]), 
                   function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
  } 
  #convert list a to matrix o
  myData <- data.frame(a)
  #print(a)
  
  # give row names to matrix using the Zvar (group)
  row.names(myData) <- a$Group.1
  
  #remove columns with all NA values. This will happen if all categorical col.
  myData <- myData[,colSums(is.na(myData)) == 0]
  #print(myData)
  
  # melt the data frame for plotting
  data.m <- melt(myData, id.vars='Group.1')
  #print(data.m)
  
  #plot bar chart
  ggplot(data.m, aes(x=Group.1, y=value, fill = Group.1)) +
    geom_bar(stat="identity")+
    geom_text(aes(label= value), vjust=1.6, color="white", size=3.5)+
    ggtitle(paste("Average ", input$Xvar, " by ", input$Zvar, Sep = ""))+
    xlab(input$Zvar) + 
    ylab(paste("Average of ", input$Xvar, sep = "")) +
    guides(fill=guide_legend(title=NULL))
})


output$scatplot <- renderMetricsgraphics({
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    # calculate the mean for each variable using the group (Zvar)
    a <- aggregate(dataUpload()[,c(input$Xvar, input$Tvar)], list(dataUpload()[, input$Zvar]), 
                   function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
  }
  #print(a)
  
  #convert list a to matrix myData
  #myData <- t(do.call(rbind,a))
  #print(myData)
  
  #convert list to data frame
  myData <- as.data.frame(a)
  #print(myData)
  
  # give row names to matrix/dataframe using the Zvar (group)
  row.names(myData) <- a$Group.1
  #print(myData)
  
  #remove columns with all NA values. This will happen if all categorical col.
  #myData <- myData[,colSums(is.na(myData)) == 0]
  
  #remove the first column
  #myData <- myData[,-1]
  
  #set the colnames of dataframe
  xvar = input$Xvar
  print(xvar)
  yvar = input$Tvar
  print(yvar)
  colnames(myData) <- c(input$Zvar,"xvar", "yvar")
  print(str(myData))
  print(myData[2])
  print(myData[3])

  "plot the chart"
  myData %>%
  mjs_plot(x=xvar, y=yvar) %>%
    mjs_point(size_accessor=xvar, color_accessor=input$Zvar,color_type = "category",
              size_range = c(5,10)) %>%
    mjs_labs(x=paste("Average ",input$Xvar, " per ", input$Zvar, sep="")
             , y=paste("Average ",input$Tvar, " per ", input$Zvar, sep=""))
  })


output$scattext <- renderTable({
  
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    # calculate the mean for each variable using the group (Zvar)
    a <- aggregate(dataUpload()[,c(input$Xvar, input$Tvar)], list(dataUpload()[, input$Zvar]), 
                   function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
  }
  colnames(a) <- c(input$Zvar,input$Xvar, input$Tvar)
  print(a)
})

#fourth tab main panel 
output$fourth  <- renderTable({
  #if no file selected return NULL
  if(is.null(input$file))
    return(NULL)
  if (input$Zvar == "None"){
    return(NULL)  #need group to perform corelation
  }
  else {
    # calculate the mean for each variable using the category (Yvar)
    a <- aggregate(dataUpload()[,input$Xvar], list(dataUpload()[, input$Yvar]), 
                   function(x) c(mean=round(mean(x[is.numeric(x)],na.action=na.pass,na.rm=TRUE),2)))
  }
  a$remarks <- "above target"
  names(a) <- c(input$Yvar, "Mean", "Remarks")
  for (i in 1 : nrow(a)){
    if (a[i,2] <= input$target){
      a[i,3] = "below target"
    }
  }
  return(a)
})

})