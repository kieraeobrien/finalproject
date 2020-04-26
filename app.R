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

# reading in Stata file of CCES 2018 data

cces2018 <- read_excel("cces2018.xlsx")

View(cces2018)

# creating ggplot and assigning as object

figure1 <- ggplot(cces2018, 
                  aes(x = cces2018$CO2)) + 
  geom_bar(mapping = NULL, fill = "mediumseagreen", 
           color = "mediumseagreen") + 
  labs(title = "Should the EPA be given the power to regulate CO2 emissions?", 
       x = "Support                                                      Oppose", 
       y = "Count", 
       caption = "Based on data from the Cooperative Congressional Election Study, 2018") + 
  scale_x_discrete(labels=c("1" = "Support", "2" = "Oppose")) + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating shiny app

ui <- fluidPage(
  plotOutput(outputId = "figure1")
)
server <- function(input, output) {
  output$figure1 <- renderPlot({figure1
    
  })
}
shinyApp(ui = ui, server = server)
