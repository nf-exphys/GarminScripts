library(shiny)
myfile <- file.path("C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Fall 2020 classes\\R Files\\GarminData\\PostRunShiny\\data\\Post run data.csv")
df <- read.csv(file = myfile, 
               header = T)
df$date <- as.Date(anytime::anydate(df$date), origin = "1970-01-01") #might not need this line

fastfed <- list("fed", "fasted")
poststrength <- list("yes", "no")
HRacc <- list("Accurate the whole time", "Accuracy was intermittent", 
              "Disconnected at end","Not accurate","No HRM", "Other")
daylist <- list("easy", "workout", "race", "long")

workoutlist <- list("none", "intervals <60 sec.", "intervals 1-3 min", "intervals 3-10", 
                    "tempo", "progression", "long run")

ui <- function(request){
  fluidPage( #Creates user interface
  titlePanel("Nick's Post Run Data Collection Form"),
  
  fluidRow(
    column(6, offset = 1,
          wellPanel(
    #Quick questions
    dateInput("date", "What's the date of this run?", autoclose = T),
    selectInput("fedstat", "Were you fasted or fed?", fastfed),
    selectInput("strength", "Did you do core after your run?", poststrength),
    selectInput("hraccuracy", "How accurate was the HR data?", HRacc, multiple = T),
          )
     ),
    column(6, offset = 1, 
           wellPanel(
    title = "Type of Run, Type of Workout, & sRPE",
    selectInput("daytype", "What kind of run did you do today?", daylist),
    selectInput("workouttype", "What kind of workout did you do today?", workoutlist),
    numericInput("srpevalue", "Session RPE (sRPE)", value = 0, min = 0, max = 2000, step = 10),
         )
     ),
    actionButton("submit", label = "Submit"), #creates a Submit button
    dataTableOutput("TheData"), #outputs data in a table
    bookmarkButton() #adds a bookmark button
   )
 )}

fields <- list("date", "fedstat", "strength", "hraccuracy", "daytype", "workouttype", "srpevalue")

server <- function(input, output, session) {
  observeEvent(input$submit, {
    newdata <- data.frame(date=input$date, food = input$fedstat, strength = input$strength, 
                          hracc = input$hraccuracy, day = input$daytype, 
                          workout = input$workouttype, sRPE = input$srpevalue)
    df <- rbind(df, newdata)
    ReactiveDf(df) # set reactiveVal's value.
    write.csv(df, "Post run data.csv", row.names = F)
  })
  #Create a reactive dataset to allow for easy updating
  ReactiveDf <- reactiveVal(df)
  
  #Create the table of all the data, not really needed but a nice way to make sure data is working
  output$TheData <- renderDataTable({
    ReactiveDf()
  })
  
}

shinyApp(ui, server, enableBookmarking = "url") #bookmarking still doesn't work
library(rsconnect)
setAccountInfo(name='nfexphys', 
                          token='C720CCC9EFBF61756F35DDA5CC483AB2', 
                          secret='U2rBHtFyTrdFEPdA3OpSLjcQIYxNOxSwjhSZ9x7W')
deployApp('C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Fall 2020 classes\\R Files\\GarminData\\PostRunShiny\\',
                     forceUpdate = T)
#rsconnect::deployApp(appDir = 'C:\\Users\\Nick.Foreman\\Desktop\\Nick Mobile Folders\\School\\Fall 2020 classes\\R Files\\GarminData\\PostRunShiny\\', 
#                     appFiles = c('.\\data\\Post run data.csv'), forceUpdate = T, lint = T)

rsconnect::showLogs(appPath = "./PostRunShiny/")
