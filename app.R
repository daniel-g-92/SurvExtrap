library(shiny)
library(flexsurv)
library(htmltools)
library(ggplot2)
library(survminer)

#setwd("//gobo/USER40/u/u1474467/Documents/survival widget") # local
# surv extrap live file

#define UI for app that finds a survival curve and plots it alongside the hazard function.

# 
uitest <- fluidPage(
titlePanel("SurvExtrap"),
sidebarLayout(
  sidebarPanel(
    helpText("Please select the parametric model and the survival points you would like the model to fit to."),

# checkbox input to select curve type 
radioButtons("curve_type", h3("Parametric Model"),
              choices=list("Exponential"=1, "Weibull"=2, "Gompertz" = 3, "Log-logistic"=4 
#                           ,"Hazard Spline"=5
), selected = 1) , 

# numeric input 
      numericInput("time_1", 
              h3(HTML(paste0("First time, t",tags$sub("1")))), 
              value = 12, min=0.0001), 

      numericInput("S_1", 
             h3(HTML(paste0("Proportion alive at t",tags$sub("1")))), 
             value = 0.5, min=0.0001, max=0.9999),   




conditionalPanel(
  condition="input.curve_type != 1", 
  numericInput("time_2", 
               h3(HTML(paste0("Second time, t",tags$sub("2")))), 
               value = 36, min=0.0001),   
  
  numericInput("S_2", 
               h3(HTML(paste0("Proportion alive at t",tags$sub("2")))), 
               value = 0.25, min=0.0001, max=0.9999)
  
),

fileInput('file1', 'Optional: Upload data as csv file with columns (time, event)',
          accept=c('text/csv',
                   'text/comma-separated-values,text/plain',
                   '.csv'))

#conditionalPanel(
#  condition="input.curve_type == 5", 
#  numericInput("time_3", 
#               h3("Third time, t3" ), 
#               value = 36, min=0.0001),   
#  
#  numericInput("S_3", 
#               h3("Proportion alive at t3"), 
#               value = 0.25, min=0.0001, max=0.9999)
#  
#)

),

mainPanel(
 h3("Confirming Input"),
  textOutput("curve") ,
  textOutput("confirminput"),
  h3("Parameters"),
  textOutput("param1"),
  textOutput("param2"),
 h3("Plot"),
 plotOutput("survivalcurve", height="600px")
 
 
  ), # placing location of output


),
hr(),
print(HTML(paste0("SurvExtrap uses parameterisations as described in the ", tags$a(href="https://www.jstatsoft.org/article/view/v070i08","flexsurv"), " package.", tags$br()))),

#print(HTML(paste0(  "SurvExtrap was created by Dan Gallacher. Email: ", tags$u("d.gallacher@warwick.ac.uk"), tags$br() ))), 

#print(HTML(paste0(  "If you have found SurvExtrap useful and would like to thank Dan or to contribute towards it running costs, click ", tags$a(href="https://www.ko-fi.com/dgallacher","here"),".")))

)

#source("./appfile/function_all.R") # local
source("function_all.R")

server <- function(input, output,session) {
  
 

  output$curve <- renderText({ 
    
    if (input$curve_type==1){paste("You have selected an Exponential model.")} else
    if (input$curve_type==2){paste("You have selected a Weibull model.")} else
    if (input$curve_type==3){paste("You have selected a Gompertz model.")} else
    if (input$curve_type==4){paste("You have selected a Log-logistic model.")} 
  #  if  (input$curve_type==5){paste("You have selected a Spline PH model.")}
    })
  

  output$confirminput <- renderText({ 
    
    if (input$curve_type==1){paste("You are fitting to S(",input$time_1,") = ",input$S_1,".",sep = "")} else
      if (input$curve_type!=1 & input$curve_type!=5){paste("You are fitting to S(",input$time_1,") = ",input$S_1," and S(",input$time_2,") = ",input$S_2,".",sep = "")} 
      #  if (input$curve_type==5){paste("You are fitting to S(",input$time_1,") = ",input$S_1,", S(",input$time_2,") = ",input$S_2, " and S(",input$time_3,") = ",input$S_3,      ".",sep = "")}
    
  })  
  

  output$param1 <- renderText({
    
    if (input$curve_type==1){paste("Rate (lambda) = ", par1(), sep = "")} else
      if (input$curve_type==2){paste("Shape (a) = ", par1(), sep = "")} else
        if (input$curve_type==3){paste("Shape (a) = ", par1(),sep = "")} else
          if (input$curve_type==4){paste("Shape (a) = ", par1(),sep = "")}
  })
    
  output$param2 <- renderText({
    
    if (input$curve_type==1){paste("_")} else
      if (input$curve_type==2){paste("Scale (mu) = ",par2(), sep = "")} else
        if (input$curve_type==3){paste("Rate (b) = ",par2(),sep = "")} else
          if (input$curve_type==4){paste("Scale (b) = ",par2(),sep = "")}
  })
  
maxt <-  reactive({
  as.numeric(2*max(as.numeric(input$time_1),as.numeric(input$time_2)))
})

timevec <- reactive({
  seq(0.01,maxt(),0.1)
})

survvec <- reactive({
  if (input$curve_type==1){exp(-par1()*timevec())} else
        if (input$curve_type==2){ exp(-(timevec()/as.numeric(par2()))^as.numeric(par1()))} else
          if (input$curve_type==3){ exp(-(as.numeric(par2())/as.numeric(par1()))*(exp(timevec()*as.numeric(par1()))-1))} else
            if (input$curve_type==4){1/(1+(timevec()/as.numeric(par2()))^as.numeric(par1()))} 
})

data1 <- reactive({ data.frame(timevec(),survvec())
})

data2 <- reactive({
  if (input$curve_type==1){
    d2 <- data.frame(input$time_1,input$S_1)
  } else {
    d2 <- data.frame(c(input$time_1, input$time_2),c(input$S_1, input$S_2))
  }
  colnames(d2) <- c("time","prop")
  d2
})

par1 <- reactive({
  if (input$curve_type==1){ expsolver(input$time_1,input$S_1)$rate} else
    if (input$curve_type==2){ wei2(input$time_1,input$time_2,input$S_1, input$S_2)$shape} else
      if (input$curve_type==3){ gomp2(input$time_1,input$time_2,input$S_1, input$S_2)$shape} else
        if (input$curve_type==4){ loglog2(input$time_1,input$time_2,input$S_1, input$S_2)$shape}
})

par2 <- reactive({
    if (input$curve_type==2){wei2(input$time_1,input$time_2,input$S_1, input$S_2)$scale} else
      if (input$curve_type==3){gomp2(input$time_1,input$time_2,input$S_1, input$S_2)$rate} else
        if (input$curve_type==4){loglog2(input$time_1,input$time_2,input$S_1, input$S_2)$scale}
})

   


myData <- reactive({
  req(input$file1)
  inFile <- input$file1
  data <- read.csv(inFile$datapath)
  colnames(data) <- c("time","event")
  return(data)
})

survfit1 <- reactive({
  req(input$file1)
  survfit(Surv(time,event)~1,data=myData()) })


output$survivalcurve <- renderPlot({
  
  
  if (is.null(input$file1)){
    
    ggplot(data=data1(),aes(x=timevec(), y=survvec()), yaxis="i") + 
      theme_set(theme_survminer(base_size=16)) +
      geom_line(colour='mediumblue', size =1.4) + 
      scale_x_continuous(name="Time",expand = c(0,0)) + 
      scale_y_continuous(name="Proportion Event Free",expand = c(0,0), limits=c(0,1)) +
      geom_point(data=data2(), aes(x=time, y=prop), size=5, color="black")
    
  }else{
    
    ggsurvplot(survfit1() ,data=myData() ,color="red", axes.offset=F, xlim = c(0,maxt()), font.main=16, font.x=16,font.y=16,
               font.legend=16, font.tickslab=16, legend.labs="Observed",conf.int=F , ylim=c(0,1))$plot + 
      theme_set(theme_survminer(base_size=16)) +
      geom_line(data=data1(),aes(x=timevec(), y=survvec()), colour='mediumblue', size=1.4) + 
      scale_x_continuous(name="Time",expand = c(0,0)) + 
      scale_y_continuous(name="Proportion Event Free",expand = c(0,0)) +
      geom_point(data=data2(), aes(x=time, y=prop), size=5, color="black")
    
  }    
  
} )
    
  }


shinyApp(ui = uitest, server = server)


