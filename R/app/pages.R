# individual pages in the wizard
page0 = tagList(
  titlePanel("Optimal experimental design for dose response."),
  'This app will help you to find an optimal design for your dose response experiment.'
)

page1 <- tagList(
  titlePanel("What is the goal of your experiment?"),
  "Before finding an optimal design, we need to clarify the purpose of the experiment.
  Select one or more of the options below.",
  checkboxInput('obj_checkbox_D', 'Estimating the dose response model (D-optimality).', value = TRUE),
  checkboxInput('obj_checkbox_BMD', 'Estimating the benchmark dose', value = FALSE)
  # what if the user checks none of the boxes?


)
page2 <- tagList(
  titlePanel('What model will you use to analyze the data?'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'model_selector',
        label = NULL,
        # choices = c('4 parameter log-logistic'),
        # selected = '4 parameter log-logistic'
        choices = c('Log-logistic', 'Logistic', 'Weibull'),
        selected = 'Log-logistic'
      ),
    ),
    mainPanel(
      "The optimal design will be based on the model you plan to use to analyze the data.
      If you are unsure, choose log-logistic. If even you you choose another model at the analysis stage,
      there is some evidence that the design will be similar if the number of parameters is the same.",
      uiOutput('model_formula_display_page2')
    )
  )


)
page3 <- tagList(
  titlePanel('Provide prior information'),
  "Finding the optimal design requires prior information about the dose-response relationship.
  This can come from a prior study. If there is no prior result, then select a curve that is
  scientifically reasonable.",
  uiOutput('model_formula_display_page3'),
  uiOutput('example_theta'),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        'theta1',
        'Theta 1',
        value = 0.32,
        step = 0.1
      ),
      numericInput(
        'theta2',
        'Theta 2',
        -12.1,
        step = 0.1
      ),
      numericInput(
        'theta3',
        'Theta 3',
        4.1,
        step = 0.1
      ),
      numericInput(
        'theta4',
        'Theta 4',
        0.913343,
        step = 0.1
      ),
      numericInput(
        'dose_limit',
        'Maximum dose',
        50,
        min = 0.01,
        step = 0.5
      )
    ),
    mainPanel(
      plotOutput('dose_response_plot_page3')
    )
  ),


)
page4 = tagList(
  titlePanel('Set algorithm options'),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        'design_pts',
        'Number of dose groups',
        value = 3,
        min = 1,
        max = 100
      ),
      numericInput(
        'swarm',
        'Swarm size',
        100,
        min = 1,
        max = 10000
      ),
      numericInput(
        'maxIter',
        'Maximum iterations',
        500,
        max = 10000,
        min = 1
      )
    ),
    mainPanel(
      'The options on this page control options for the algorithm that finds designs.',
      tags$ul(
        tags$li("Dose levels: this option controls the maximum number of dose groups in the design.
                Usually, this should match the number of parameters in the model."),
        tags$li('Swarm size: controls the number of entities used by the algorithm to
                find the design. Increasing this number will diversify the seach and
                make design more likely to be found'),
        tags$li("Maximum iterations: controls the number of iterations for algorithm.")
      )
    )
  ),


)
page5 = tagList(
  titlePanel('Verify and find design'),
  sidebarLayout(
    sidebarPanel(
      htmlOutput('verify_choices')
    ),
    mainPanel(
      plotOutput('dose_response_plot_page4')
    )
  )
)
