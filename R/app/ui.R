# ui function for shiny app
ui <- fluidPage(
  wizardUI(
    id = "wizard",
    pages = list(page0, page1, page2, page3, page4, page5),
    doneButton = actionButton("done", "Find design!")
  )
)
