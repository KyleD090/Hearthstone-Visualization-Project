

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Hearthstone Project"),
  
  dashboardSidebar(selectInput("select1", label = h3("Select Class"), 
                                      choices = list("Druid" = "DRUID", "Mage" = "MAGE", "Shaman" = "SHAMAN", "Rogue" = "ROGUE", "Paladin" = "PALADIN", "Warlock" = "WARLOCK", "Warrior" = "WARRIOR", "Priest" = "PRIEST", "Hunter" = "HUNTER"),
                                      selected = "DRUID"), sidebarMenu(
   
  )),
  
  dashboardBody(  
    fluidRow(
    box(plotOutput("plot1")),
    box(plotOutput("plot2")),
    box(plotOutput("plot3")),
    box(plotOutput("plot4"))
   )
   ) 
    
   
    
  )
  



server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    data <- read.csv("cards.csv")
    cards1 <- filter(data, playerClass %in% c("MAGE", "DRUID", "HUNTER", "PALADIN", "PRIEST", "ROGUE", "SHAMAN", "WARLOCK", "WARRIOR"))
    cards1A <- filter(cards1, rarity %in% c("FREE", "COMMON", "RARE", "EPIC", "LEGENDARY"))
    cardsUnique1 <- filter(cards1A, playerClass == input$select1)
    cols <- c("FREE" = "blue4", "COMMON" = "blue3", "RARE" = "dodgerblue3", "EPIC" = "steelblue2", "LEGENDARY" = "skyblue1")
    ggplot(cardsUnique1, aes( playerClass, fill = rarity )) + geom_bar() + labs(title = "Stacked Rarity") +  scale_fill_manual(values = cols)
  })
  
  output$plot2 <- renderPlot({
    data <- read.csv("cards.csv")
    cards2 <- filter(data, playerClass %in% c("MAGE", "DRUID", "HUNTER", "PALADIN", "PRIEST", "ROGUE", "SHAMAN", "WARLOCK", "WARRIOR"))
    cards2A <- filter(cards2, type %in% c("SPELL" , "MINION"))
    cardsUnique2 <- filter(cards2A, playerClass == input$select1)
    ggplot(cardsUnique2, aes(x = playerClass, y = "", fill = type )) + geom_bar(stat = "identity") + coord_polar("y", start=0) + labs(title = "Spell to Minion Ratio") + scale_fill_manual(values = c("red", "purple"))
  })
  
  output$plot3 <- renderPlot({
    data <- read.csv("cards.csv")
    cards3 <- filter(data, playerClass %in% c("MAGE", "DRUID", "HUNTER", "PALADIN", "PRIEST", "ROGUE", "SHAMAN", "WARLOCK", "WARRIOR"))
    cards3A <- filter(cards3, rarity %in% c("FREE", "COMMON", "RARE", "EPIC", "LEGENDARY"))
    cardsUnique3 <- filter(cards3A, playerClass == input$select1)
    cols <- c("FREE" = "blue4", "COMMON" = "blue3", "RARE" = "dodgerblue3", "EPIC" = "steelblue2", "LEGENDARY" = "skyblue1")
    ggplot(cardsUnique3, aes(x=rarity, y=cost, color = rarity)) +  geom_boxplot() + labs(title = "Mana Cost compared to Rarity" ) + scale_color_manual(values = cols)
  })
  
  
  output$plot4 <- renderPlot({
    data <- read.csv("cards.csv")
    cards4 <- filter(data, playerClass == c("MAGE", "DRUID", "HUNTER", "PALADIN", "PRIEST", "ROGUE", "SHAMAN", "WARLOCK", "WARRIOR"))
    cards4A <- filter(cards4, health <= 10)
    cardsUnique4 <- filter(cards4A, playerClass == input$select1)
    ggplot(cardsUnique4, aes(health, fill = "red")) +  geom_bar() + labs(title = "Distribution of Minion Health") 
  })
  
}

shinyApp(ui, server)



