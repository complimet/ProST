library(shiny)
library(shinydashboard)

sidebar_menu <- sidebarMenu(
  div(
    align = "center",
    br(),
    tags$figure(
      tags$img(
        style = "filter:invert(100%)",
        src = "ProSt_logo_exp.svg", # PLACEHOLDER FROM FLATICON -- original logo to be made
        width = 200,
        alt = ""
      ),
    ),
    h3("ProST v0.91"),
    br()
  ),
  menuItem("Getting started", tabName = "start", icon = icon("gauge")),
  menuItem("Download sample data", tabName = "samples", icon = icon("download")),
  menuItem("Analyze", tabName = "analyze", icon = icon("magnifying-glass")),
  menuItem("Troubleshoot", tabName = "troubleshoot", icon = icon("screwdriver-wrench")),
  menuItem("Parameter Help", tabName = "param", icon = icon("gear")),
  menuItem("Authors and citing", tabName = "cite", icon = icon("pencil")),
  menuItem("Return to CompLiMet", icon = icon("house"), href = "https://complimet.ca")
)
