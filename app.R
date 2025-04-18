source("global.R")
source("ui/ui_main.R")
source("server/server_main.R")


shinyApp(ui = ui, server = server)