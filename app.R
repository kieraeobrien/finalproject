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

# creating Republican only variables

rsonly <- cces2018 %>%
  mutate(category = case_when(
    (cces2018$pid7) == 5 ~ "Lean Republican",
    (cces2018$pid7) == 6 ~ "Not Very Strong Republican",
    (cces2018$pid7) == 7 ~ "Strong Republican",
  ))

rsonly <- na.omit(rsonly)

# breaking down by age group 

yrsonly <- rsonly %>%
  mutate(age = (2020 - rsonly$birthyr)) 
  
yrsonly <- yrsonly %>%
  filter(age <= 30)

oldrsonly <- rsonly %>%
  mutate(age = (2020 - rsonly$birthyr)) 

oldrsonly <- oldrsonly %>%
  filter(age > 30)

# creating ggplot for EPA CO2 regulation

figure1 <- ggplot(oldrsonly, 
                  aes(x = oldrsonly$CO2)) + 
  geom_bar(mapping = NULL, stat = "count", fill = "lightsteelblue", 
           color = "lightsteelblue") + 
  labs(title = "Older Republicans: Should the EPA Regulate CO2?", 
       x = "Support                  Oppose", 
       y = "Count", 
       caption = "Based on data from the Cooperative Congressional Election Study, 2018") + 
  scale_x_discrete(labels=c("1" = "Support", "2" = "Oppose")) + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating ggplot for young Republicans only 

figure3 <- ggplot(yrsonly, aes(x = yrsonly$CO2)) + 
  geom_bar(mapping = NULL, stat = "count", fill = "lightsteelblue", 
           color = "lightsteelblue") + 
  labs(title = "Young Republicans: Should the EPA Regulate CO2?", 
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
  geom_bar(alpha = .4, colour = "black", size = 0.73) +
  labs(fill = "Climate Action Supported", title = "Support for Federal Climate Actions", x = "Climate Action Policy Supported", y = "Support for Action (Count)", caption = "Data from the Cooperative Congressional Election Study (2018)") + 
  scale_x_discrete(labels = NULL) +
  scale_fill_brewer(palette = 3, direction = -1, na.value = "grey50") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))

# creating all policy ggplot for just young Rs

yraction <- yrsonly %>%
  select(CO2, MPG, Reneww, JobsVEnvir, Paris) %>%
  mutate(category = case_when(
    (yrsonly$CO2) == 1 ~ "EPA Regulation of CO2",
    (yrsonly$MPG) == 1 ~ "MPG Standards",
    (yrsonly$Reneww) == 1 ~ "Renewable Portfolios",
    (yrsonly$JobsVEnvir) == 1 ~ "Environment > Jobs",
    (yrsonly$Paris) == 1 ~ "Paris Agreement"))

figure4 <- yrsonly %>%  
  ggplot(aes(yraction$category, fill = yraction$category)) +
  geom_bar(alpha = .4, colour = "black", size = 0.73) +
  labs(fill = "Climate Action Supported", title = "Young Republican Climate Action Support", x = "Climate Action Policy Supported", y = "Young Republicans Supporting (Count)", caption = "Data from the Cooperative Congressional Election Study (2018)") + 
  scale_x_discrete(labels = NULL) +
  scale_fill_brewer(palette = 3, direction = -1, na.value = "grey50") + 
  theme_classic() + 
  theme(plot.title = element_text(face = "bold", 
                                  size = 17))


# creating shiny app

ui <- fluidPage(
  theme = shinytheme("cosmo"),

  titlePanel("Climate Policy & Public Opinion"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("About", 
               titlePanel("About"),
               h3("Background"),
               p("Climate change has emerged as one of the most pressing political issues of our time. However, federal-level action lags behind popular demand for change."),
               p("By examining public opinion data from the Cooperative Congressional Election Study, I analyze a common misconception regarding American politics and the climate challenge: that Republicans are opposed to action of any sort."),
               p("Rather than trying to identify who or what is to blame for the lag in federal policy-making, I'm interested in points of common ground or belief, as that is where progress is more likely to be made."),
               h3("About Me"),
               p("My name is Kiera O'Brien, and I'm a senior at Harvard College. I studied Government on the Technology Science track with a secondary in History. I was born and raised in Ketchikan, Alaska and am passionate about responsible climate policy-making and conservative politics. 
                 In the fall, I'll be starting my graduate studies in Columbia University's Department of Earth & Environmental Sciences.
                 You can reach me via email at:", a("kiera_obrien@college.harvard.edu.", href = "mailto: kiera_obrien@college.harvard.edu"))),
 
       tabPanel("Findings",
           titlePanel("Findings"),
           mainPanel(
             plotOutput(outputId = "figure2"),
             br(), 
             h3("Summary"),
             p("The chart above compares levels of support for Federal level climate actions, as found in the 2018 Cooperative Congressional Election Study. Five policy actions were asked about, and respondents that did not answer affirmatively to any of the questions are shown as 'NA.'"),
             p("The corresponding question language is shown and clarified below. Clearly, EPA regulation of CO2 is far and away the most popular policy choice. A bar graph was chosen to clarify popularity relative to other actions polled."),
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
           h3("The Anticipated Response"),
           p("When asked whether or not the EPA should be allowed to regulate carbon emissions, the first graph shows the expected Republican position. However, this graph includes only Republican respondents over the age of 30, as climate change has emerged as a generational wedge issue."),
           br(),
           mainPanel(plotOutput(outputId = "figure1"),
                     br(),
          h3("The Next Generation"),
          p("Republicans under 30 are significantly more open to federal-level climate action. This graph shows the majority of Republicans under the age of 30 would like the EPA to regulate carbon emissions. This is a major divergence from the party line."),
          br(),
          plotOutput(outputId = "figure3"),  
          br(),
          h3("Takeaways"),
          p("While the older generations of Republicans hold oppositional views to the most popular federal climate policies, the next generation of Republicans do not take the same stance. In fact, the Young Republican subsection of the electorate holds fairly similar positions as the entire electorate when it comes to climate policy at the federal level."),
          p("As shown here, Republicans under 30 are relatively less favorable on the environment versus jobs question and renewable portfolio requirements, but are not openly hostile toward EPA regulation of CO2 in the same way their older counterparts are."), 
          br(),
           plotOutput(outputId = "figure4")
           )),
  
  tabPanel("Data",
    titlePanel("The Cooperative Congressional Election Study (CCES)"),
    mainPanel(
      p("The CCES is a 50,000+ person national stratified sample survey administered by YouGov. Half of the content is 'common content,' consisting of questions asked to all respondents. The data used in my project comes from the 2018 common content questionnaire, looking at full-sample answers to questions on environmental policymaking as well as how partisanship impacts these opinions."),
        p("The data and codebook can be accessed", a("here.", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/ZSBZ7K"))
  
  
)))))
  
  

server <- function(input, output) {
  output$figure2 <- renderPlot({figure2})
  output$figure1 <- renderPlot({figure1})
  output$figure3 <- renderPlot({figure3})
  output$figure4 <- renderPlot({figure4})
  
  }
shinyApp(ui = ui, server = server)