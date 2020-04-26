library(haven)
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(vctrs)
library(rsconnect)
library(shiny)
library(haven)
library(readxl)

rsconnect::setAccountInfo(name='kieraobrien',
                          token='D36A6A60ACFFD2CA1CB5F3ED10FE5C4C',
                          secret='Hx28vDzuEizm14tABGzGmdr/B7WFerO7MLHYzILd')

# Note: each of these datasets comes with a guide for interpretation.
# It is on the CCES website. I used it to help select useful questions.
# https://cces.gov.harvard.edu/, under each year. 

# creating shiny app

ui <- fluidPage(
  plotOutput(outputId = "figure1")
)
server <- function(input, output) {
  input = NULL 
  output$figure1 <- renderPlot({figure1
  })
}
shinyApp(ui = ui, server = server)
