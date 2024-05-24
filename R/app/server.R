# code for server
server <- function(input, output, session) {
  wizardServer("wizard", 6)

  # dose response plot
  dose_response_plot = reactive({
    theta = c(input$theta1, input$theta2, input$theta3, input$theta4)
    plot_response(input$model_selector, theta, input$dose_limit, log_dose=F)
  })

  # reactive objects for result
  results = reactiveValues(pareto_data = NULL, eq_plot = NULL, design = NULL)

  # find design button
  observeEvent(input$done, {

    shinybusy::show_modal_spinner(
      spin = "double-bounce",
      color = "#112446",
      text = "Loading..."
    )

    #browser()
    selected_obj = c(input$obj_checkbox_D, input$obj_checkbox_BMD)
    obj_names = c('D', 'c')[selected_obj]
    grad = grad_selector(input$model_selector)
    grad_funs = list(grad, grad)[selected_obj]
    obj_funs = list(obj.D, obj.c)[selected_obj]
    theta = c(input$theta1, input$theta2, input$theta3, input$theta4)
    thetas = list(theta, theta)[selected_obj]
    bmd_grad = get_bmd_grad(input$model_selector, 'extra')
    params = list(c(), bmd_grad(0.1, theta))[selected_obj]
    dr_funs = list(f.loglogistic3.bmds, f.loglogistic3.bmds)[selected_obj]


    # switch between single and multi-objective
    if (sum(selected_obj) == 1) {

      result = nlodm(
        model = NULL,
        grad_fun = grad_funs[[1]],
        obj = obj_names[1],
        theta = theta,
        prior_weights = c(1),
        bound = input$dose_limit,
        pts = input$design_pts,
        algorithm = 'DE',
        swarm = input$swarm,
        iter = input$maxIter,
        seed = NULL,
        c = params[[1]],
        exact = F,
        exact_digits = 4,
        binary_response = T,
        dr_fun = dr_funs[[1]]
      )

      # save results to reactive data structure
      results$eq_plot = result$plot
      results$design = result$design

      shinybusy::remove_modal_spinner()

      showModal(modalDialog(
        title = 'Results',
        "A design consists of x values (dosages) and w values (weights).
        The weights are the proportion of the total sample size that should be
        assigned to that dose group. Also displayed is the best objective value
        found and the algorithm used. The plot provides a graphic check of the design's
        optimality.",
        plotOutput("eq_plot"),
        verbatimTextOutput('single_obj_out'),
        modalButton("Done"),
        footer = NULL))
    }
    else if (sum(selected_obj) == 2) {
      # call main function
      shinybusy::update_modal_spinner("Computing Pareto front...")

      result = multi_obj(
        grad_funs,
        obj_funs,
        thetas,
        params,
        type = 'pareto',
        bound = input$dose_limit,
        pts = input$design_pts,
        swarm = input$swarm,
        maxiter = input$maxIter,
        verbose = T,
        exact = F,
        binary_responses = rep(T, sum(selected_obj)),
        dr_funs = dr_funs
      )

      # process results
      results$pareto_data = extract_front(result, F)

      shinybusy::update_modal_spinner('Finding optimal single objective designs...')

      # find single objective designs
      Dopt = nlodm(
        model = NULL,
        grad_fun = grad_funs[[1]],
        obj = obj_names[1],
        theta = theta,
        prior_weights = c(1),
        bound = input$dose_limit,
        pts = input$design_pts,
        algorithm = 'DE',
        swarm = input$swarm,
        iter = input$maxIter,
        seed = NULL,
        c = params[[1]],
        exact = F,
        exact_digits = 4,
        binary_response = T,
        dr_fun = dr_funs[[1]]
      )

      Copt = nlodm(
        model = NULL,
        grad_fun = grad_funs[[2]],
        obj = obj_names[2],
        theta = theta,
        prior_weights = c(1),
        bound = input$dose_limit,
        pts = input$design_pts,
        algorithm = 'DE',
        swarm = input$swarm,
        iter = input$maxIter,
        seed = NULL,
        c = params[[2]],
        exact = F,
        exact_digits = 4,
        binary_response = T,
        dr_fun = dr_funs[[2]]
      )

      # D-efficency
      results$pareto_data$obj1 = (exp(-results$pareto_data$obj1)/exp(as.numeric(Dopt$design$obj_value)))^(1/length(theta))
      results$pareto_data$obj2 = (exp(-results$pareto_data$obj2)/exp(as.numeric(Copt$design$obj_value)))


      shinybusy::remove_modal_spinner()


      showModal(modalDialog(
        title = 'Results',
        "The Pareto front contains the designs that optimally balance efficiency
        for both objects. For these designs, it is impossible to improve on one
        objective without sacrificing performance on the other. The plot and the
        table can be used to choose the designs which best meet your requirements.",
        plotOutput("results_plot"),
        tableOutput("results_table"),
        modalButton("Done"),
        footer = NULL))
    }
    else if (sum(selected_obj) == 0) {
      showModal(modalDialog(
        title = 'Results',
        "Please select at least 1 objective function.",

        modalButton("Done"),
        footer = NULL))
    }


  })

  # equivalence theorem plot in modal
  output$eq_plot = renderPlot({
    results$eq_plot
  })

  # single objective design output in modal
  output$single_obj_out = renderPrint(
    results$design
  )

  # Render the data table in modal
  output$results_table <- renderTable({
    results$pareto_data
  })

  # Render the plot in modal
  output$results_plot <- renderPlot({
    selected_obj = c(input$obj_checkbox_D, input$obj_checkbox_BMD)
    if (!is.null(results$pareto_data) &
        sum(selected_obj) == 2) {
      plot_pareto2d(results$pareto_data, c("D efficiency", "BMD efficiency"))
    }
  })

  output$model_formula_display_page2 = renderUI({
    p(withMathJax(model_display(input$model_selector)))
  })

  output$model_formula_display_page3 = renderUI({
    p(withMathJax(model_display(input$model_selector)))
  })

  output$example_theta = renderUI({
    p(withMathJax(display_example_param(input$model_selector)))
  })

  output$dose_response_plot_page3 = renderPlot({
    dose_response_plot()
  })

  output$dose_response_plot_page4 = renderPlot({
    dose_response_plot()
  })

  output$verify_choices = renderUI({
    HTML(paste(
      'D objective:',
      input$obj_checkbox_D,
      '<br>BMD objective:',
      input$obj_checkbox_BMD,
      '<br>Model:',
      input$model_selector,
      '<br>theta 1:',
      input$theta1,
      '<br>theta 2:',
      input$theta2,
      '<br>theta 3:',
      input$theta3,
      '<br>theta 4:',
      input$theta4,
      '<br>Maximum dosage:',
      input$dose_limit,
      '<br>Number of dose groups:',
      input$design_pts,
      '<br>Swarm size:',
      input$swarm,
      '<br>Maximum iterations:',
      input$maxIter
    ))
  })
}
