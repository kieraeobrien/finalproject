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

# creating ggplot for EPA CO2 regulation

figure1 <- ggplot(cces2018, 
                  aes(x = cces2018$CO2)) + 
  geom_bar(mapping = NULL, stat = "count", fill = "Indianred", 
           color = "Indianred") + 
  labs(title = "EPA Regulation of CO2?", 
       x = "Support                  Oppose", 
       y = "Count", 
       caption = "Based on data from the Cooperative Congressional Election Study, 2018") + 
  scale_x_discrete(labels=c("1" = "Support", "2" = "Oppose")) + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating ggplot comparing favorability of different climate plans

action <- cces2018 %>%
  select(CO2, MPG, Reneww, JobsVEnvir, Paris) %>%
  mutate(category = case_when(
    (cces2018$CO2) == 1 ~ "EPA Regulation of CO2",
    (cces2018$MPG) == 1 ~ "MPG Standards",
    (cces2018$Reneww) == 2 ~ "Renewable Portfolios",
    (cces2018$JobsVEnvir) == 1 ~ "Environment > Jobs",
    (cces2018$Paris) == 2 ~ "Paris Agreement"))

# plotting density and filling by cased categories
# assigning themes and correcting labels

figure2 <- cces2018 %>%  
  ggplot(aes(action$category, fill = action$category)) +
  geom_density(alpha = .4, colour = "black", size = 0.73) +
  labs(fill = "Climate Action Supported", title = "Support for Federal Climate Actions", x = "Climate Action Supported", y = "Support Relative to Other Actions", caption = "Data from the Cooperative Congressional Election Study (2018)") + 
  scale_x_discrete(labels = NULL) +
  scale_fill_brewer(palette = 3, direction = -1, na.value = "grey50") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating shiny app

ui <- fluidPage(
  theme = shinytheme("cosmo"),

  titlePanel("Climate Policy and Public Opinion"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("About", 
               titlePanel("About"),
               h3("Background"),
               p("Climate change has emerged as one of the most pressing political issues of our time. However, federal-level action lags behind popular demand for change."),
               p("By examining public opinion data from the Cooperative Congressional Election Study, I analyze several common misconceptions regarding American politics and the climate challenge."),
               p("Rather than trying to identify who or what is to blame for the lag in federal policy-making, I'm interested in points of common ground or belief, as that is where progress is more likely to be made."),
               h3("About Me"),
               p("My name is Kiera O'Brien and I'm a senior at Harvard College. I studied Government on the Technology Science track with a secondary in History. I was born and raised in Ketchikna, Alaska, and am passionate about responsible climate policy-making and conservative politics. 
                 You can reach me via email at kiera_obrien@college.harvard.edu.")),
 
       tabPanel("Findings",
           titlePanel("Findings"),
           mainPanel(
             plotOutput(outputId = "figure2"),
             br(), br(),
             h3("Corresponding Questions"),
             h4("Environment Versus Jobs"),
             p("The above graph compares the relative support for the five environmental questions asked in the 2018 CCES."),
             p("The first variable, 'Environment > Jobs', corresponds with affirmative answers to the question: 'Do you support or oppose strengthening the Environmental Protection
Agency enforcement of the Clean Air Act and Clean Water Act even if it costs US jobs'"),
             h4("EPA Regulation of CO2"),
             p("The second varible, 'EPA Regulation of CO2' corresponds with affirmative answers to the question: 'Do you support or oppose giving the Environmental Protection Agency
power to regulate Carbon Dioxide emissions?'"),
            p("This question is very important broadly due to the fact that regulation of CO2 emissions is a prerequisite to any federal-level emissions-reductions plan, 
including cap-and-trade or a price on carbon."),
            h4("Fuel Efficiency Standards"),
            p("The third variable, 'MPG Standards', corresponds with negative responses to the question: 'Do you support or oppose lowering the required fuel efficiency for the average automobile from 35 mpg to 25 mpg?'"),
            h4("Paris Agreement"),
            p("The fourth variable, 'Paris Agreement', corresponds with negative responses to the question: 'Do you support or oppose withdraw the United States from the Paris
Climate Agreement?'"),
            h4("Renewable Portfolios"), 
            p("The fifth variable, 'Paris Agreement', corresponds with affirmative answers to the question: 'Do you support or oppose requiring that each state use a minimum
amount of renewable fuels (wind, solar, and hydroelectric) in the generation of electricity even if electricity prices increase?'"),
            h4("No federal action"),
            p("NA responses represent the relative proportion of respondents that did not choose the environmentally favorable answer to any of these five questions."),
            br(), br(),
             
  )),
  
  tabPanel("Partisanship",
           titlePanel("Republican Views on the Most Broadly-Favored Federal Measure"),
           mainPanel(plotOutput(outputId = "figure1"),
                     br(), br(),
            h3("Takeaways"),
                     p("Selecting only Republican respondants, there is still a strong majority that favors the most broadly-favored climate measure, allowing the EPA the power to regulate CO2 emissions.")
           )),
  
  tabPanel("Data",
    titlePanel("The Cooperative Congressional Election Study (CCES)"),
    mainPanel(
      p("The CCES is a 50,000+ person stratified sample survey administered by YouGov.")
  
  
)))))
  
  

server <- function(input, output) {
  output$figure2 <- renderPlot({figure2
    
  })
  output$figure1 <- renderPlot({figure1})
}
shinyApp(ui = ui, server = server)