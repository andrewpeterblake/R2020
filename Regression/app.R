#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(readxl)     # Not core tidyverse
library(lubridate)  # Not core tidyverse
library(quantmod)   # For as.yearqtr

h <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/"
k <- "survey-of-professional-forecasters/historical-data/"
f <- "spfmicrodata.xlsx"

# download.file(paste0(h,k,f), destfile=f, mode="wb")

micro <- read_excel(f, na="#N/A", sheet="UNEMP", col_types="numeric") %>% 
  mutate(Date=as.Date(as.yearqtr(paste(YEAR, QUARTER, sep=" Q")))) %>% 
  select(Date, UNRATE=UNEMP1, UNEMP0=UNEMP2, UNEMP1=UNEMP3, UNEMP2=UNEMP4, UNEMP3=UNEMP5, UNEMP4=UNEMP6, ID) %>%
  mutate(UNRATE=lead(UNRATE,1))

# Plots 

mm2 <- micro %>% 
  pivot_longer(cols = starts_with("UNE"), names_to="Var", values_to="Val") %>% 
  mutate(IDVin = paste0("ID",ID,"_Vin:",Date)) %>% 
  group_by(ID, Date) %>% 
  mutate(FP = seq.Date(Date[1], by="quarter", length.out=5)) %>%
  ungroup() %>% 
  filter(!is.na(Val)) %>%
  group_by(ID) %>% 
  mutate(Earliest = min(Date)) %>% 
  ungroup() %>% 
  mutate(Started = if_else(year(Earliest) > 2009, 
                           "3. Only forecasted since 2005", 
                           if_else(year(Earliest) < 2000, 
                                   "1. Forecasted since last century", "2. Started 2000-2008")))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Forecaster IDs"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("minID",
                     "Min ID number:",
                     min = min(mm2$ID),
                     max = max(mm2$ID),
                     value = 1), 
         sliderInput("maxID",
                     "Max ID number:",
                     min = min(mm2$ID),
                     max = max(mm2$ID),
                     value = max(mm2$ID)),
         radioButtons("era", "Which era forecaster?",
                      c("Any"          = "0",
                        "Last century" = "1",
                        "Millenial"    = "2",
                        "Gen Z"        = "3"))
      ),
      
      # Show the plot
      mainPanel(
         plotOutput("uPlot")
      )
   )
)

# Define server logic required to draw graph
server <- function(input, output) {
   
   output$uPlot <- renderPlot({
     
     imaxID <- input$maxID
     iminID <- input$minID
     if (iminID > imaxID) iminID <- imaxID
     if (imaxID < iminID) imaxID <- iminID
     
     if (!input$era == "0") {
       era <- unique(mm2$Started)
       era <- era[substr(era, 1, 1) == input$era] 
       mm2 <- mm2 %>% 
         filter(Started == era) 
       }
     else 
     {
       era <- "All eras"
     }

     mm2 %>% 
       filter(ID < (imaxID+1) & ID > (iminID-1)) %>% 
       # filter(Started == era) %>% 
       ggplot() + 
       geom_line(aes(x=FP, y=Val, group=IDVin, colour=as.factor(ID))) +
       theme_minimal() +
       theme(legend.position = "none") +
       labs(title=paste("Unemployment forecasts by ID =", iminID, "to", imaxID),
            subtitle = era, 
            x="", y="Rate (percentage points)")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
