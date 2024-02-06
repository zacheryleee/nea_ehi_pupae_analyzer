#### V3 mock up, V3 improvement to add perentile and clean up of geom_line with colours and legend. Not Live yet
### The code for the Web Application for Pupae Analysis for the separation of the bimodal peaks through compuational modelling



#Set the directory
#setwd("/Users/Zachery/Documents/NEA Internship 2022/Data Analytics/EHI_Data_Analyzer(V3)/app")



#For installing of the necessary packages needed for the app to work offline
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("rsconnect")
# install.packages('ISLR')
# install.packages('mixtools')
# install.packages("stats4")
# install.packages('devtools')
# install.packages("cutoff")
# install.packages('bbmle')
# install.packages("usethis")
# install.packages("dpylr")



#For running R Shiny 
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(rsconnect)



# For the modelling (run it twice due to some issue with dependencies among the packages)
library('ISLR')
library('mixtools')
library(stats4)
library('devtools')
library(cutoff)
library('bbmle')
library("usethis")
library("dplyr")



##### UserInterface
about_page <- tabPanel(
  title = "About", 
  titlePanel("About"),
  br(),
  "The purpose of the web application is to accurately split a bimodal distribution to 2 normal distributions with a confidence interval of 99.9% and Type1 error of 0.1% through computational modelling.",
  br(),
  br(),
  "Solely for use at Environmental Health Institute (EHI), NEA @ AMK Techplace II",
  br(),
  "Created by Zachery Lee Wei Quan with R Shiny",
  br(),
  "2022 June, Version III",
  br(),
  "All rights reserved"
)



main_page <- tabPanel(
  title = "Pupae Size Anaylsis",
  titlePanel("Analysis"),
  sidebarLayout(
    sidebarPanel(
      title ="Inputs",
      fileInput("csv_input", "Select Group File(s) to Import", accept = ".csv", multiple=T),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title= "Plot",
          plotOutput("plot_1"),
          br(), 
          br(),
          tableOutput("text_1")
        ),
        tabPanel(
          title = "Data File",
          fluidRow(
            column(width =12, tableOutput("combined_summary_table"))
          )
        )
      )
    )
  )
)



ui <- navbarPage(
  title = "EHI Pupae Data Analyser",
  theme = shinytheme('simplex'),
  main_page,
  about_page
) 



##### Functions 
create_combined_table <- function(data_input){
  data_input <- unlist(data_input, use.names=F)
  data_input <- data_input[-which(is.na(data_input))]
  length(data_input) <- prod(dim(matrix(data_input, ncol=12)))
  ddd <- matrix(data_input, ncol=12 )
  colnames(ddd) <- c(paste("#",seq(1:12),sep=""))
  return(ddd)
} 



plot_mix_comps <- function(x, mu, sigma,lam){
  lam * dnorm(x, mu, sigma)
}



draw_plot_1 <- function(data_input){
  data_input <- unlist(data_input, use.names=F)
  data_input <- data_input[-which(is.na(data_input))]
  
  set.seed(1234)
  mixmdl = normalmixEM(data_input)
  mixmodel <- em(data_input, "normal", "normal")
  confint(mixmodel,level=0.99)
  cut_off <- cutoff(mixmodel, distr =1, level=0.999, type1 = 0.001)
  
  
  data.frame(x= mixmdl$x) %>%
    ggplot() +
    geom_histogram(aes(x, ..density..), binwidth = 0.01, colour = "black", 
                   fill = "grey") + scale_x_continuous(breaks = seq(0.80, 1.45, by=0.025))  + coord_cartesian(xlim = c(0.80, 1.45))+ 
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                  aes(color = "Male"), lwd = 1, lty=1, show.legend=T, inherit.aes =F) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                  aes(color ="Female"), lwd = 1, lty=1, show.legend=T, inherit.aes=F) + 
    scale_colour_manual("Gender", values = c("pink3", "blue"), labels = c("Female", "Male")) +
    ylab("Density") + xlab("Length of Pupae (mm)") + 
    geom_vline(xintercept = cut_off[1], lty=2, lwd=1.0, col="darkblue") + 
    annotate(x=cut_off[1], y= +Inf, label = paste("Estimate Cutoff = ",round(cut_off[1],digit=3),"mm",sep=""), vjust =3, geom = "label", colour = "red", size =4.5, fill = "white" )+
    annotate(x=cut_off[1], y= +Inf, label = paste("Male Percentile = ",signif(pnorm((cut_off[1] - mixmdl$mu[1])/mixmdl$sigma[1])*100, digit=2),"%",sep=""), vjust =4.5, geom = "label", colour = "darkblue", size =4, fill ="white")+
    theme(axis.text.x =element_text(size=6.5), axis.title=element_text(size=10,face="bold.italic")) +
    theme(legend.title = element_text(face="bold.italic", size =10), legend.position = c(.95,.75))
}

write_text_1 <- function(data_input){
  data_input <- unlist(data_input, use.names=F)
  data_input <- data_input[-which(is.na(data_input))]
  
  set.seed(1234)
  mixmdl = normalmixEM(data_input)
  mixmodel <- em(data_input, "normal", "normal")
  confint(mixmodel,level=0.99)
  cut_off <- cutoff(mixmodel, distr =1, level=0.999, type1 = 0.001)
  
  testing <-as.data.frame(mixmodel$param)
  testing[5,] <- pnorm((cut_off[1] - mixmdl$mu[1])/mixmdl$sigma[1])*100
  testing <- t(testing)
  colnames(testing) <- c("Male Pupae Mean (mm)", "Male Pupae Standard Deviation (mm)","Female Pupae Mean (mm)", "Female Pupae Standard Deviation (mm)", "Male Pupae Percentile Cutoff (%)")
  
  return(testing)
} 



##### Server list 
server <- function(input, output){
  options(shiny.maxRequest = 20*1024^2)
  
  data_input <- reactive ({
    req(input$csv_input)
    rbindlist(lapply(input$csv_input$datapath, fread), use.names =F, fill= F)
  })
  
  # for the statistic table 
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table (data_input())
  })
  output$combined_summary_table <- renderTable(combined_summary_table(), bordered =T, width = "100%")
  
  #for the plot table
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input())
  })
  output$plot_1 <- renderPlot(plot_1(), width = "auto", height ="auto")
  
  # for the text 
  text_1 <- eventReactive(input$run_button,{
    write_text_1(data_input())
  })
  output$text_1 <- renderTable(text_1())
}



shinyApp(ui = ui, server = server)



#For the app to work offline
#runApp("/Users/Zachery/Documents/NEA Internship 2022/Data Analytics/EHI_Data_Analyzer(V3)/app", display.mode = "showcase")






# This code is written by Zachery Lee Wei Quan. Do contact me at 98361720 should there be any problem. 
