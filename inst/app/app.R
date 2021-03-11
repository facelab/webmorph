## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(DT)
  library(dplyr)
  library(webmorphR)
})

## functions ----

debug_msg <- function(txt) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) message(txt)
}

## tabs ----

finder_tab <- tabItem(
  tabName = "finder_tab",
  fileInput("fileUpload", "Upload File", multiple = TRUE,  width = "100%"),
  uiOutput("finder")
)

project_tab <- tabItem(
  tabName = "project_tab",
  dataTableOutput("project")
)

delin_tab <- tabItem(
  tabName = "delin_tab",
  uiOutput("delin")
)

average_tab <- tabItem(
  tabName = "average_tab",
  uiOutput("average")
)

trans_tab <- tabItem(
  tabName = "trans_tab",
  uiOutput("transform")
)

three_tab <- tabItem(
  tabName = "three_tab",
  uiOutput("threeD")
)

## UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WebMorphR",
                  dropdownMenuOutput("queue")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Projects", tabName = "project_tab"),
      menuItem("Finder", tabName = "finder_tab"),
      menuItem("Delineate", tabName = "delin_tab"),
      menuItem("Average", tabName = "average_tab"),
      menuItem("Transform", tabName = "trans_tab"),
      menuItem("3D", tabName = "three_tab")
    ),
    textInput("email", NULL, placeholder = "email"),
    passwordInput("password", NULL, placeholder = "password"),
    textOutput("login_status"),
    actionButton("login", "Login"),
    actionButton("logout", "Logout")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      project_tab,
      finder_tab,
      delin_tab,
      average_tab,
      trans_tab,
      three_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  # hide elements
  c("logout", "login_status") %>%
    lapply(hide)

  v <- reactiveValues()

  # login ----
  observeEvent(input$login, { debug_msg("login")
    email <- ifelse(input$email == "",
                    Sys.getenv("WEBMORPH_EMAIL"),
                    input$email)

    password <- ifelse(input$password == "",
                       Sys.getenv("WEBMORPH_PASSWORD"),
                       input$password)

    if (email == "" || password == "") return()

    tryCatch({
      login(email, password)
      show("logout")
      show("login_status")
      hide("login")
      hide("email")
      hide("password")

      output$login_status <- renderText({
        paste("Logged in as", email)
      })

      plist <- projListGet(TRUE)
      output$project <- renderDT(plist)

    }, error = function(e) {
      alert(e$message)
    })
  })

  # logout ----
  observeEvent(input$logout, { debug_msg("logout")
    tryCatch({
      logout()
      output$login_status <- renderText("")
      hide("logout")
      hide("login_status")
      show("login")
      show("email")
      show("password")
    }, error = function(e) {
      alert(e$message)
    })
  })

  # fileUpload ----
  observeEvent(input$fileUpload, { debug_msg("fileUpload")
    path <- input$fileUpload$datapath
  }, ignoreNULL = TRUE)



} # end server()

shinyApp(ui, server)
