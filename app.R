library(tidyverse)
library(ggplot2)
library(vctrs)
library(rsconnect)
library(shiny)
library(readxl)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

rsconnect::setAccountInfo(name='kieraobrien',
                          token='D36A6A60ACFFD2CA1CB5F3ED10FE5C4C',
                          secret='Hx28vDzuEizm14tABGzGmdr/B7WFerO7MLHYzILd')

# Note: each of these datasets comes with a guide for interpretation.
# It is on the CCES website. I used it to help select useful questions.
# https://cces.gov.harvard.edu/, under each year. 

# reading in excel file of CCES 2018 data

cces2018 <- read_excel("cces2018.xlsx")

# removing NA values and viewing 

cces2018 <- na.omit(cces2018)

# creating ggplot comparing favorability of different climate plans

action <- cces2018 %>%
  select(CO2, MPG, Reneww, JobsVEnvir, Paris) %>%
  mutate(category = case_when(
    (cces2018$CO2) == 1 ~ "EPA Regulation of CO2",
    (cces2018$MPG) == 1 ~ "MPG Standards",
    (cces2018$Reneww) == 1 ~ "Renewable Portfolios",
    (cces2018$JobsVEnvir) == 1 ~ "Environment > Jobs",
    (cces2018$Paris) == 1 ~ "Paris Agreement"))

# plotting density and filling by cased categories
# assigning themes and correcting labels

figure2 <- cces2018 %>%  
  ggplot(aes(action$category, fill = action$category)) +
  geom_density(alpha = .4, colour = "black", size = 0.73) +
  labs(fill = "Climate Action Supported", title = "Support for Different Climate Actions at the Federal Level", x = "Climate Action Supported", y = "Support Relative to Other Actions", caption = "Data from the Cooperative Congressional Election Study (2018)") + 
  scale_x_discrete(labels = NULL) +
  scale_fill_brewer(palette = 3, direction = -1, na.value = "grey50") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating shiny app

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(tags$b("Climate Policy and Public Opinion")),

  titlePanel("Climate Policy and Public Opinion"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("About", 
               titlePanel("About"),
               h3("Background"),
               p("Text on why I did this"),
               p("Text on where my data comes from"),
               h3("About Me"),
               p("My name is Kiera O'Brien and I'm a senior at Harvard College. I study Government on the Technology Science track with a secondary in History. 
             You can reach me at kiera_obrien@college.harvard.edu.")),
 
       tabPanel("Findings",
           titlePanel("Findings"),
           mainPanel(
             plotOutput(outputId = "figure2"),
             br(), br(),
             p("Description of graph"),
             br(), br(),
             
  )),
  
  tabPanel("Analysis",
    titlePanel("The Data"),
    mainPanel(
      h3("CCES"),
      p("Description of data'")
  
  
)))))
  
  

server <- function(input, output) {
  output$figure2 <- renderPlot({figure2
    
  })
}
shinyApp(ui = ui, server = server)