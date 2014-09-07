library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Childcare and Infant care centres in Singapore"),
        sidebarPanel(
                h4("Please input your postal code here"),
                h5("Singapore"),
                textInput("POnum",label="", value = "238823"),
                submitButton("Submit")
                ),
        mainPanel(
                #verbatimTextOutput("oEstate"),
                h4("You belong to the Estate in"),
                h4(textOutput("oEstate")),
                h4("The number of child care centres in your area is", textOutput("oCCcentres")),
                h4("The number of infant care centres in your area is", textOutput("oICcentres")),
                plotOutput('CCstackplot', width = "auto", height = 600)
                )
))