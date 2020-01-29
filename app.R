library(ggplot2)
library(dplyr)
library(readr)
library(GGally)
library(reshape2)
library(textclean)
library(shiny)
library(RColorBrewer)
library(colourpicker)
library(DT)
library(markdown)
library(foreign)
library(rio)
library(rio)
library(shiny)
library(tools)
library(foreign)
library(readxl)
library(datasets)
library(ggvis)



SEAM_ORDER<- c("C1","C2","D1U","D1L","D2U","D2L","D3","E1","E2","F1","F2","F3")


# Define UI
ui <- fluidPage(pageWithSidebar(
  
  headerPanel("Model validation quality plot"),
  
  sidebarPanel(width=2,
               
               fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               
               checkboxInput('header', 'Header', TRUE),
               
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
               
               uiOutput("slider"),
               
               selectInput('x', 'X',""),
               selectInput('y', 'Y',"",selected=""),
               downloadButton('downloadPlot', 'PNG'),
               downloadButton('downloadPdf', 'PDF')
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)
)







server <- function(input, output, session){
  
  # added "session" because updateSelectInput requires it
  
  dataset <- reactive({
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    ###############################update input in UI #################################
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    
    updateSelectInput(session, inputId = 'x', label = 'X',
                      choices = 'SEAM', selected = names(df))
    updateSelectInput(session, inputId = 'y', label = 'Y',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  
  
  
  ################################
  
  plotInput <- reactive({
    
    df <-dataset()
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + 
      geom_boxplot(fill = "blue", colour = "black", alpha = 0.7, outlier.colour = "red", 
                   outlier.shape = NA, outlier.size = 1,)+
      geom_point(aes_string(color=input$y))+
      scale_x_discrete(limits =c(SEAM_ORDER)) + 
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=90, hjust=1),      ##size x axis labels and turn 90 degrees
            plot.background = element_rect(fill = "gray80"),   ##plot background to gray
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    
    if (input$y=='THK'|| input$y == 'FC'|| input$y == 'CVR'|| input$y == 'CVR') {
      p <- p + scale_color_gradient(low="red",high="green")
    }
    
    else {
      p<- p + scale_color_gradient(low="green",high="red")
      
    }
    
  })
  
  #####################################
  
  output$plot <- renderPlot({
    
    
    print(plotInput())
    
    
  },width = 1550,height=700)   
  
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {name <- basename(file_path_sans_ext(input$y))
    paste0(name,'_APB' ,'.png')
    },
    content = function(file) {
      ggsave(file,plot=plotInput(),device="png",width = 300, height = 210, units = "mm",dpi = 600)
    }
  )
  
  output$downloadPdf <- downloadHandler(
    filename = function() {name <- basename(file_path_sans_ext(input$y))
    paste0(name, '.pdf')  
    },
    content = function(file) {
      ggsave(file,plot=plotInput(),device='pdf',path=NULL,width = 297, height = 210, units = "mm",dpi = 600)
    }
  )
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)
