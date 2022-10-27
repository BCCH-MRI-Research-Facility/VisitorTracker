#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
    options(shiny.launch.browser=FALSE)

library(shinyjs)
library(shinyalert)    
library(dplyr)
library(lubridate)
source('./database.R')


# Define UI for application that collects name and contact information
ui <- fluidPage(theme = "bootstrap.css",
    shinyjs::useShinyjs(),
    useShinyalert(),
    div(id = "visitorlog",
        fluidRow(
            column(10, br(), br(), h1("Welcome to the BCCH MRI Research Facility")),
            column(2, img(src = "BCCH_Logo_RGB.jpg", width = 150))
            ),
        br(),
        h2("Please complete our online check-in and check-out process."),
        p("Due to the ongoing COVID-19 pandemic, we are required to collect
          personal information to support contact 
          tracing in the event we have a known case of COVID-19.  We will 
          retain your first name, surname, telephone number, and email for 30
          days. These details will be used to record who visited the BCCH
          MRI Research Facility. This information will be used only if there
          is a known case of COVID-19 at the BCCH MRI Research Facility. 
          If no case has been reported in the 30 days since your visit, 
          your name and contact information will be deleted from our database." ),
        h3("Please help us keep everyone safe."),
        shinyjs::hidden(
            div(id = "loginoutbuttons",
                fluidRow(
                    column(3,
                           actionButton("LogIn", "Check In"),
                           actionButton("LogOut", "Check Out")
                           )
                    )
                )
            ),
    
    shinyjs::hidden(
        div(id = "logininfo",
            br(),
            textInput(inputId = "Name", 
                      label = "Name (Last, First)", 
                      placeholder = "Last name, First name", 
                      width = "100%"),
            textInput(inputId = "Phone",
                      label = "Telephone Number (###-###-####)",
                      placeholder = "###-###-####",
                      width = "100%"),
            textInput(inputId = "Email",
                      label = "Email",
                      placeholder = "Email",
                      width = "100%"),
            selectInput(inputId = "Reason4Visit",
                        label = "Reason for Visit",
                        choices = c("",
                                    "Participating in Research Study",
                                    "MRI Appointment from my Doctor",
                                    "Accompanying Person/Parent/Guardian",
                                    "Member of Research Team",
                                    "Facility Staff",
                                    "Other"),
                        selected = ""),
            actionButton("SubmitEntry", "Submit")
            )
        ),
    
    shinyjs::hidden(
        div(id = "logoutinfo",
            br(),
            textInput(inputId = "NameExit", 
                      label = "Name (Last, First)", 
                      placeholder = "Last name, First name", 
                      width = "100%"),
            #selectizeInput(inputId = 'NameExit', 
            #               label = 'Please select your name', 
             #              choices = c("Select Name" = "", levels(visitors$Name))),
            actionButton("SubmitExit", "Submit")
            )
        )
    )
)
    
#    conditionalPanel(
#        condition = "input.LogIn == true && input.SubmitEntry != true", 
#        hidden(textInput(inputId = "EntryTime",
#                         label = "EntryTime",
#                         value = now())),
#        hidden(textInput(inputId = "ExitTime",
#                         label = "ExitTime",
#                         value = NA)),
#        actionButton("SubmitEntry", "Submit")
#        ),
    # Log-out info
# 
#)

# Define server logic required 
server <- function(input, output, session) {
    require(dplyr)
    #shinyjs::hide(id = "logininfo", anim = TRUE)
    shinyjs::show(id = "loginoutbuttons", anim = TRUE)
    
    # Let's prevent people from submitting an empty item
    observe({
        shinyjs::toggleState("SubmitEntry", !is.null(input$Name) && input$Name != "")
        shinyjs::toggleState("SubmitEntry", !is.null(input$Email) && input$Email != "")
        shinyjs::toggleState("SubmitEntry", !is.null(input$Phone) && input$Phone != "")
        shinyjs::toggleState("SubmitEntry", !is.null(input$Reason4Visit) && input$Reason4Visit != "")
        shinyjs::toggleState("SubmitExit", !is.null(input$NameExit) && input$NameExit != "")
    })
    shinyjs::onclick("LogIn",
                     shinyjs::hide(id = "loginoutbuttons", anim = TRUE))
    shinyjs::onclick("LogOut",
                     shinyjs::hide(id = "loginoutbuttons", anim = TRUE))
    shinyjs::onclick("LogIn",
                     shinyjs::show(id = "logininfo", anim = TRUE))
    shinyjs::onclick("LogOut",
                     shinyjs::show(id = "logoutinfo", anim = TRUE))
    # Submit Values to database and restart app
    observeEvent(input$LogIn, {
        shinyjs::reset("visitorlog")
        removeClass("LogOut", "btn-primary")
        addClass("LogIn", "btn-primary")
        hide(id = "logoutinfo", anim = TRUE)
    })
    observeEvent(input$SubmitEntry,{
        require(dplyr)
        require(lubridate)
        
        inputs <- as.data.frame(cbind("Name" = input$Name, 
                        "Phone" = input$Phone, 
                        "Email" = input$Email,
                        "Reason4Visit" = input$Reason4Visit,
                        "EntryDate" = as.character(today()),
                        "EntryTime" = as.character(now()),
                        "ExitTime" = NA))
        saveData(inputs)
        
        shinyalert::shinyalert("Thank-you!", 
        "Please be sure to use hand sanitizer 
          and complete the COVID screening.", 
        "Someone will be with you shortly.", type = "info")
        Sys.sleep(1)
        hide(id = "logininfo", anim = TRUE)
        shinyjs::reset("visitorlog")
    })
    
    # get previous logins and add logout date/time
    observeEvent(input$LogOut, {
        require(dplyr)
        require(lubridate)
        
        removeClass("LogIn", "btn-primary")
        addClass("LogOut", "btn-primary")
        hide(id = "logininfo", anim = TRUE)
        
        
        #ExitName.choice <- reactive({
        #    visitors$Name
        #    })
        
        #observe({
        #    updateSelectizeInput(session, "NameExit", choices = ExitName.choice())
        #    })
       })
    
    
    # Now save the logout time
    observeEvent(input$SubmitExit, {
        visitors <- as.data.frame(loadData())
        
        if (input$NameExit %in% visitors$Name){
            Sys.sleep(1)
            shinyalert::shinyalert("Thank-you for visiting!", 
                                   "Please be sure to use hand sanitizer 
          on your way out!.", type = "info"
            )
        } else {
            shinyalert::shinyalert("We do not seem to have you on record.", 
                                   "Please check your spelling and try again.", 
                                   type = "error")
        }
        Sys.sleep(1)
        hide(id = "logoutinfo", anim = TRUE)
        shinyjs::reset("visitorlog")
        visitors <- loadData()
    })
    
    # Now delete entries more than 30 days old (Bad practice, I know.)
    timeStop <- "23:55:00"
    trick <- reactiveValues()
    trick$catch <- FALSE
    
    observe({
        invalidateLater(200, session)
        if (strftime(Sys.time(),  format="%H:%M:%S") > timeStop) {
            trick$toFire <- "after"
        } else if (strftime(Sys.time(),  format="%H:%M:%S") < timeStop){
            trick$toFire <- "before"
        } else if(strftime(Sys.time(),  format="%H:%M:%S") == timeStop & trick$catch == FALSE){
            trick$toFire <- "exact"  
        }
    })
    
    observe({
        invalidateLater(200, session)
        if (trick$toFire == "exact") {
            trick$toFire <- "after"
            trick$catch <- TRUE
            Sys.sleep(1)
            #your code or functions
            shiny.auto.delete(table)
            trick$catch <- FALSE
        } 
    }) 
    
    session$allowReconnect("force")
}


# Run the application 
shinyApp(ui = ui, server = server)
