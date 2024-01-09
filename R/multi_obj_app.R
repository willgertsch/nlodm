multi_obj_app = function() {

  models = c(
    'Logistic',
    'Weibull',
    'Log-logistic',
    'Hill',
    'Probit'
  )

  objectives = c(
    'D',
    'A',
    'bmd',
    'c',
    'c_e'
  )

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        "Options",
        shiny::selectInput(
          inputId = 'type_selector',
          label = 'Type',
          choice = c('pareto', 'Weighted sum')
        ),
        shiny::numericInput(
          inputId = 'bound_input',
          label = "Dose upper bound",
          value = 30,
          min = 1
        ),
        shiny::numericInput(
          inputId = 'pts_input',
          label = 'Max dose groups',
          min = 1,
          value = 3,
          step = 1
        ),
        shiny::numericInput(
          inputId = 'swarm_input',
          label = 'Swarm size',
          min = 10,
          value = 100,
          step = 1
        ),
        shiny::numericInput(
          inputId = 'generations_input',
          label = 'Number of generations',
          min = 1,
          value = 500,
          step = 1
        )
      ),
      shiny::mainPanel(
        shiny::selectInput(
          inputId = 'model_selector',
          label = 'Model',
          choices = models
        ),
        shiny::selectInput(
          inputId = 'obj_selector',
          label = 'Objective',
          choices = objectives
        ),
        shiny::textInput(
          inputId = 'theta_input',
          label = 'Theta'
        ),
        shiny::textInput(
          inputId = 'param_input',
          label = 'Additional parameters'
        ),
        shiny::actionButton(
          inputId = 'add_obj_button',
          label = 'Add objective'
        ),
        shiny::actionButton(
          inputId = 'remove_obj_button',
          label = 'Remove last'
        ),
        shiny::tableOutput(
          outputId = 'obj_table'
        ),
        shiny::actionButton(
          inputId = 'run_button',
          label = 'Run'
        )
      )
    )
  )

  server <- function(input, output, session) {

    # reactive data
    values = shiny::reactiveValues()
    values$obj_table = data.frame(
      model = c('Log-logistic'),
      objective = c('D'),
      theta = c('0.02461,-2.390, 1'),
      param = c('')
    )

    # design criterion table
    output$obj_table = shiny::renderTable({
      values$obj_table
    })

    # add entry to the table
    shiny::observeEvent(input$add_obj_button, {

      values$obj_table = rbind(
        values$obj_table,
        data.frame(
          model = c(input$model_selector),
          objective = c(input$obj_selector),
          theta = c(input$theta_input),
          param = c(input$param_input)
        )
      )
    }
    )

    # remove last row of the table
    shiny::observeEvent(input$remove_obj_button, {
      values$obj_table = head(values$obj_table, -1)
    })

    # run algorithm
    shiny::observeEvent(input$run_button, {

      # need to have at least 2 objectives
      if (nrow(values$obj_table) < 2)
        return()

      # load results from table
      model_names = values$obj_table$model
      obj_names = values$obj_table$objective
      raw_theta = values$obj_table$theta
      raw_param = values$obj_table$param

      grad_funs = sapply(model_names, grad_selector)
      obj_funs = sapply(obj_names, get_obj)
      thetas = lapply(raw_theta, process_theta)
      params = lapply(raw_param, process_theta)


      # call multi-objective function
      result = multi_obj(
        grad_funs = grad_funs,
        obj_funs = obj_funs,
        thetas = thetas,
        params = params,
        type = input$type_selector,
        bound = input$bound_input,
        pts = input$pts_input,
        swarm = input$swarm_input,
        maxiter = input$generations_input,
        verbose = F
      )

    })

  }

  # start app
  shiny::shinyApp(ui, server)
}
