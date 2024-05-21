# individual pages in the wizard
page1 <- tagList(
  titlePanel("Design criteria"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput('obj_checkbox_D', 'D', value = TRUE),
      checkboxInput('obj_checkbox_A', 'A', value = FALSE),
      checkboxInput('obj_checkbox_BMD', 'BMD', value = FALSE)
    ),
    mainPanel(
      'Before finding the optimal design, one or more design criteria must be selected.
      These are chosen based on the goals of the experiment and different criteria will
      lead to different designs.',
      tags$ul(
        tags$li('D (recommended): minimizes the confidence region for all parameter estimates.'),
        tags$li('A: minimizes the sum of the variances of the parameter estimates.'),
        tags$li('BMD: minimizes the variance of the estimated benchmark dose. ')
      )
    )
  )


)
page2 <- tagList(
  titlePanel('Select model'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'model_selector',
        label = NULL,
        # choices = c('4 parameter log-logistic'),
        # selected = '4 parameter log-logistic'
        choices = c('Log-logistic', 'Logistic', 'Weibull', 'Hill'),
        selected = 'Log-logistic'
      ),
    ),
    mainPanel(
      'Optimal design is a model-based methodology and assumes the data will be
      analyzed using a specific model. If the analysis model is unknown, it may be
      beneficial to find designs under the assumption of several different models.',
      uiOutput('model_formula_display_page2')
    )
  )


)
page3 <- tagList(
  titlePanel('Choose model parameters'),
  "Finding optimal designs for nonlinear dose-response models requires a prior guess of the true parameters.",
  uiOutput('model_formula_display_page3'),
  uiOutput('example_theta'),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        'theta1',
        'Theta 1',
        value = 0.084950,
        step = 0.1
      ),
      numericInput(
        'theta2',
        'Theta 2',
        -11.093067,
        step = 0.1
      ),
      numericInput(
        'theta3',
        'Theta 3',
        12.157339,
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
        'Dose limit',
        5,
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
  titlePanel('Algorithm options'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'method',
        'Multi-objective method',
        choices = c('compound', 'pareto'),
        selected = 'pareto'
      ),
      numericInput(
        'design_pts',
        'Dose levels (sample size for exact designs)',
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
      ),
      checkboxInput(
        'exact',
        'Exact design',
        value = F
      )
    ),
    mainPanel(
      'The options on this page control options for the algorithm that finds designs.',
      tags$ul(
        tags$li('Multi-objective method: if the Pareto option is selectd, multi-objective
                optimization will be used to find designs that are optimal trade-offs between objectives.
                (Compound designs are the traditional weighted sum approach and are
                currently not supported.)'),
        tags$li("Dose levels: this option controls the maximum number of dose groups in the design.
                Usually, this should match the number of parameters in the model."),
        tags$li('Swarm size: controls the number of entities used by the algorithm to
                find the design. Increasing this number will diversify the seach and
                make design more likely to be found'),
        tags$li("Maximum iterations: controls the number of iterations for algorithm."),
        tags$li("Exact design: By default, the design found will be in terms of
                the proportion of the total sample size at each dose group. If a
                specific total sample size is required, check this box and update
                the dose levels option to be the total sample size.")
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
