if (require(pryr)) {
  mem_used <- pryr::mem_used
} else {
  mem_used <- function() {
    warning("pryr package is not installed.")
    return(0)
  }
}


server <- function(input, output, session) {

  # Get variables passed through the environment
  if(!exists('roots', envir = parent.env(environment()), inherits = FALSE)) {
    roots <- c(Data="C:/")
  }
  if(!exists('debug', envir = parent.env(environment()), inherits = FALSE)) {
    debug <- FALSE
  }
  if(!exists('review', envir = parent.env(environment()), inherits = FALSE)) {
    review <- FALSE
  }

  # Not reactive values


  # Reactive values for flowbunch
  mem <- reactiveValues(
    my_fb = NULL,
    my_fb_ref = NULL,
    my_fb_ref_adj = NULL
  )

  # Reactive values to store user preferences
  prefs <- reactiveValues(
    ui_plot_height = 400
  )

  state <- reactiveValues(
    debugging = FALSE
  )

  # User does have control over cleanup
  cleanup <- reactiveVal(1)
  review_plots <- reactiveVal(1)


  # ========== DEBUG

  # display debugging UI in debug mode
  output$debug_ui <- renderUI({
    req(debug)
    tags$div(
      class="form-group shiny-input-container",
      actionButton(
        "debug_button",
        "Debug NOW"
      ),
      actionButton(
        "debug_flag",
        "Debugging is Off"
      ),
      actionButton(
        "debug_mem_button",
        "Mem use: click!"
      ))
  })

  # activate debugging in some functions
  observeEvent(input$debug_flag, {
    state$debugging <- !state$debugging
    dbg_msg <- paste0("Debugging is ", ifelse(state$debugging, "On", "Off"))
    updateActionButton(session, "debug_flag", dbg_msg)
  })
  debugging <- function() {
    if (debug) isolate(state$debugging) else FALSE
  }

  # interrupt the code to view values using isolate()
  observeEvent(input$debug_button, {
    browser()
  })

  # report memory use
  observeEvent(input$debug_mem_button, {
    mem_use <- sprintf("Mem use %.0f MB", mem_used()/1024^2)
    updateActionButton(session, "debug_mem_button", mem_use)
    message(mem_use)
  })


  # ========== CREATE

  shinyDirChoose(
    input, 'create_fcs_dir', roots = roots)
  shinyDirChoose(
    input, 'create_proj_dir', roots = roots)

  # create_create_button <- eventReactive(input$create_create_button, {})
  # TODO: validate are not output
  observeEvent(input$create_create_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!'),
      need(input$create_fcs_dir, 'Check directory of FCS!')
    )
    proj_name <- input$create_proj_name
    proj_dir <- parseDirPath(roots = roots, input$create_proj_dir)
    fcs_dir <- parseDirPath(roots = roots, input$create_fcs_dir)
    cytometer <- input$create_cytometer
    # check FCS files present
    ok <- length(dir(path = fcs_dir, "\\.fcs$", ignore.case = TRUE)) > 0
    if (!ok) {
      showNotification("No FCS in selected dir?", type = "error")
      req(ok)
    }
    showNotification("Scan started", type = "message")
    withCallingHandlers({
      shinyjs::html("create_log", "\n")
      my_fb <- fb_initiate(proj_name, proj_dir, fcs_dir, cytometer = cytometer)
    },
    message = function(m) {
      shinyjs::html(id = "create_log", html = m$message, add = TRUE)
    })
    showNotification("Scan finished", type = "message")
    # TODO: update UI for default processing parameters
    mem$my_fb <- my_fb
  })

  output$create_set <- renderText({
    req(mem$my_fb)
    fb_info(mem$my_fb)
  })

  output$create_pheno_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@pheno)
      mem$my_fb@pheno
    }
  )

  output$create_panel_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@panel)
      mem$my_fb@panel
    }
  )

  # ========== SETUP

  # during the setup, the user fill batch_id, anchor and reference columns of
  # the pheno slot.

  # TODO: test button for regular expressions

  # Finalize the setup and store pheno and panel as files that the user will
  # edit and check
  observeEvent(input$setup_setup_button, {
    batch_pattern <- input$setup_batch_pattern
    ref_sample_pattern <- input$setup_ref_sample_pattern
    validate(
      need(batch_pattern, 'Batch pattern!'),
      need(ref_sample_pattern, 'Sample pattern!')
    )
    req(batch_pattern, ref_sample_pattern)
    my_fb <- mem$my_fb
    req(my_fb)

    withCallingHandlers({
      shinyjs::html("setup_log", "\n")
      my_fb <- fb_bn_init_batch(
        my_fb,
        batch_pattern,
        ref_sample_pattern)
      fb_write(my_fb)
      message("Done!")
    },
    message = function(m) {
      shinyjs::html(id = "setup_log", html = m$message, add = TRUE)
    })
    showNotification("Pheno and panel files written to disk", type = "message")
    showNotification("Edit and verify pheno and panel files", type = "warning")
    mem$my_fb <- my_fb
  })

  # This opens an explorer to ease the editing of pheno and panel files
  observeEvent(input$setup_explorer, {
    my_fb <- mem$my_fb
    req(my_fb)
    fb_path <- fb_file_path(my_fb)
    if (.Platform[['OS.type']] == "windows") {
      shell.exec(fb_path)
      showNotification("A file explorer is opened", type = "message")
    } else {
      message(fb_path)
      showModal(modalDialog(
        title = "Open project dir",
        "It is not possible to open a file explorer on this OS.",
        "Copy and paste the following path in your file manager. ", fb_path
      ))
    }
  })

  # Once edited, pheno and panel are loaded back
  observeEvent(input$setup_reload_button, {
    my_fb <- mem$my_fb
    req(my_fb)
    # <<< DEBUGGING
    if (debug) {
      f_backup <- fb_file_name(my_fb, "../%s-pheno.xlsx")
      if (file.exists(f_backup))
        file.copy(f_backup, overwrite = TRUE,
                  fb_file_name(my_fb, "%s-pheno.xlsx"))
      f_backup <- fb_file_name(my_fb, "../%s-panel.xlsx")
      if (file.exists(f_backup))
        file.copy(f_backup, overwrite = TRUE,
                  fb_file_name(my_fb, "%s-panel.xlsx"))
    }
    # DEBUGGING >>>
    my_fb <- fb_reload(my_fb)
    mem$my_fb <- my_fb
    # Erase any ref and transformation
    mem$my_fb_ref_adj <- mem$my_fb_ref <- NULL
    showNotification("Pheno and panel files reloaded from disk", type = "message")
    # Check batch information in pheno is correct
    errors <- fb_bn_check_pheno(my_fb)
    errors <- c(errors, fb_bn_check_panel(my_fb))
    # output errors
    msg <- paste0(tags$br(), "===== ", date(), tags$br(), tags$br())
    if (length(errors)) {
      msg <- paste0(msg, errors, collapse = "<br/>")
      showNotification("Pheno file is incorrect. Check Log.", type = "error")
    } else {
      msg <- paste0(msg, "Pheno is correct.")
      showNotification("Pheno file is correct.", type = "message")
    }
    shinyjs::html(id = "setup_log", html = msg, add = TRUE)
  })

  output$setup_set <- renderText({
    req(mem$my_fb)
    tags$pre(fb_info(mem$my_fb))
  })

  output$setup_pheno_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@pheno)
      pheno <- mem$my_fb@pheno
      pheno[, colnames(pheno) %in%
              c("sample_id", "batch_id", "sample_is_ref", "batch_is_ref")]
    }
  )

  output$setup_panel_table <- renderDataTable(
    options = list(pageLength = 20),
    {
      req(mem$my_fb, mem$my_fb@panel)
      mem$my_fb@panel
    }
  )

  # # Check batch information in pheno is correct
  # observeEvent(input$setup_reload_button, {
  #   my_fb <- mem$my_fb
  #   req(my_fb)
  #
  #   errors <- fb_bn_check_pheno(my_fb)
  #   # output errors
  #   msg <- paste0(tags$br(), "===== ", date(), tags$br(), tags$br())
  #   if (length(errors)) {
  #     msg <- paste0(msg, errors, collapse = "<br/>")
  #     showNotification("Pheno file is incorrect. Check Log.", type = "error")
  #   } else {
  #     msg <- paste0(msg, "Pheno is correct.")
  #     showNotification("Pheno file is correct.", type = "message")
  #   }
  #   shinyjs::html(id = "setup_log", html = msg, add = TRUE)
  # })


  # ========== TUNE

  debug_mem <- function(msg = "") {
    # return(NULL)
    # warning(sprintf("Mem used: %.2f", mem_used()/1024^2))
    rep1 = sprintf(
      "  %s  output: %.2f kB,  mem: %.2f kB, mem used: %.2f MB\n", msg,
      (object.size(output))/1024,
      (object.size(mem))/1024,
      (mem_used())/1024^2)
    rep2 = paste0(sapply(reactiveValuesToList(mem), function(x)
      sprintf("    %.2f kB", object.size(x)/1024)), collapse = "")
    paste0(rep1, rep2, "\n")
    # cat(rep1, rep2, sep = "", file = stderr())
  }
  # When the sample button is pushed,
  # Then sample cells
  observeEvent(input$tune_sample_button, {
    warning("Update UI sample")
    if (debugging()) browser()
    # load data to assess density plot
    tune_load_ncells <- as.integer(input$tune_load_ncells)
    req(tune_load_ncells)
    err <- tune_load_ncells < 1000
    if (err) {
      showNotification("Not enough cells.", type = "error")
      req(err)  #, cancelOutput = TRUE)
    }
    my_fb <- mem$my_fb
    req(my_fb)

    # set my_fb_ref at 1st sampling or simply update expressions
    my_fb_ref <- mem$my_fb_ref
    if (is.null(my_fb_ref)) {
      # extract the bunch of reference FCS
      my_fb_ref <- fb_bn_subset_references(my_fb)
    }

    # load data to assess density plot
    showNotification("Sampling started", type = "message")
    my_fb_ref <- fb_load_cells(
      my_fb_ref, n_cells = tune_load_ncells
    )
    showNotification("Modeling started", type = "message")
    my_fb_ref <- fb_bn_model_batch(my_fb_ref)
    showNotification("Normalizing started", type = "message")
    my_fb_ref_adj <- fb_bn_correct_batch(my_fb_ref)
    showNotification("Data ready", type = "message")

    # update memory
    mem$my_fb_ref <- my_fb_ref
    mem$my_fb_ref_adj <- my_fb_ref_adj

    # Update the UI channels
    # should be done previously
    # TODO: trigger on my_fb instead of my_fb_ref
    warning("Update UI channel")
    ok <- !is.na((my_fb_ref@panel$batchnorm_method))
    channels <- my_fb_ref@panel$antigen[ok]
    names(channels) <- channels
    # TODO: use row_no instead of name in case of duplicated marker name
    channel <- input$tune_channel
    # if (isFALSE(channel)) channel <- channels[1]
    channel <- channels[1]
    updateSelectizeInput(
      session, "tune_channel",
      choices = channels,
      selected = channel)
    # will be updated when tune_channel changes
    updateSelectizeInput(
      session, "revtran_channel",
      choices = channels)
      # selected = input$revtran_channel)
    updateSelectizeInput(
      session, "revcoef_channel",
      choices = channels)
      # selected = input$revcoef_channel)
    updateSelectizeInput(
      session, "revbipl_channel_x",
      choices = channels)
      # selected = input$revbipl_channel_x)
    updateSelectizeInput(
      session, "revbipl_channel_y",
      choices = channels)
      # selected = input$revbipl_channel_y)

    warning("Update UI batch")
    batches <- c(0, my_fb_ref@pheno$file_no)
    names(batches) <- c("all_batches", my_fb_ref@pheno$batch_id)
    updateSelectizeInput(
      session, "revtran_batch",
      choices = batches)
      # selected = input$revtran_batch)
    updateSelectizeInput(
      session, "revbipl_batch",
      choices = batches)
      # selected = input$revbipl_batch)
  })

  observeEvent(input$tune_channel, {
    if (debugging()) browser()
    channel <- input$tune_channel
    req(channel)
    validate(need(nchar(as.character(channel)) > 2,
                  "Select a channel in the menu."))
    # my_fb_ref <- mem$my_fb_ref
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    req(idx)
    channel <- my_fb_ref@panel$fcs_colname[idx]

    warning("Update UI batch method")
    bnp <- fb_bn_split_params(
      my_fb_ref@panel$batchnorm_method[idx],
      my_fb_ref@panel$batchnorm_params[idx]
    )
    freezeReactiveValue(input, "tune_batch_method")
    updateSelectInput(
      session, "tune_batch_method",
      selected = bnp[["method"]])
    freezeReactiveValue(input, "tune_batch_params")
    updateTextInput(
      session, "tune_batch_params",
      value = bnp[["params"]])
    freezeReactiveValue(input, "tune_batch_zero")
    updateCheckboxInput(
      session, "tune_batch_zero",
      value = bnp[["exclude_zeroes"]])
    freezeReactiveValue(input, "tune_batch_transf")
    updateCheckboxInput(
      session, "tune_batch_transf",
      value = bnp[["transform"]])

    warning("Update UI transf")
    method <- my_fb_ref@panel$transf_method[idx]
    params <- my_fb_ref@panel$transf_params[idx]
    # freezeReactiveValue(input, "tune_transf_method")
    updateSelectInput(
      session, "tune_transf_method",
      selected = method
    )
    # freezeReactiveValue(input, "tune_transf_params")
    updateTextInput(
      session, "tune_transf_params",
      value = params
    )

    warning("Update UI rev")
    # freezeReactiveValue(input, "revcoef_channel")
    updateSelectizeInput(session, "revcoef_channel", selected = input$tune_channel)
    # freezeReactiveValue(input, "revtran_channel")
    updateSelectizeInput(session, "revtran_channel", selected = input$tune_channel)
    # freezeReactiveValue(input, "revbipl_channel_x")
    updateSelectizeInput(session, "revbipl_channel_x", selected = input$tune_channel)

    cleanup(runif(1))
  })

  # Set the UI for the plots
  output$tune_ui_plot_adj <- renderUI({
    height <- input$tune_plot_height
    req(height)
    plotOutput(
      "tune_main_plot_adj", width = "100%", height = height
    )
  })
  output$tune_ui_plot_raw <- renderUI({
    height <- input$tune_plot_height
    req(height)
    plotOutput(
      "tune_main_plot_raw", width = "100%", height = height
    )
  })

  # If method or parameters of batch modeling change,
  # Then update fb_ref
  observeEvent(c(
    cleanup(),
    input$tune_transf_method,
    input$tune_transf_params,
    input$tune_batch_method,
    input$tune_batch_params,
    input$tune_batch_zero,
    input$tune_batch_transf
    # input$tune_channel
  ), {
    # retrieve channel
    my_fb <- mem$my_fb
    req(my_fb)
    idx <- guess_match_channels(my_fb, input$tune_channel)
    req(idx)
    tune_channel <- my_fb@panel$fcs_colname[idx]
    req(tune_channel)
    # retrieve transformation
    transf_method <- input$tune_transf_method
    transf_params <- input$tune_transf_params
    req(transf_method, transf_params)
    message(transf_method, "-", transf_params)
    # retrieve batch and check
    batch_method <- input$tune_batch_method
    batch_params <- input$tune_batch_params
    bp <- as.numeric(strsplit(batch_params, ",\\s*")[[1]])
    # verify conversion is OK
    if (any(is.na(bp)))
      req(FALSE, cancelOutput = FALSE)
    # no percentile is OK for method "none"
    if (batch_method != "none" && length(bp) == 0)
      req(FALSE, cancelOutput = FALSE)
    # verify 4 numbers for quantiles method
    if (batch_method == "quantiles" && length(bp) < 4) {
      showNotification("Quantiles method required at least 4 percentiles.", type = "error")
      req(FALSE, cancelOutput = TRUE)
    }
    # verify quantiles are between 0 and 1
    err <- (min(bp) < 0) || (max(bp) > 1)
    if (err) {
      showNotification("Percentiles are values between 0 and 1.", type = "error")
      req(FALSE, cancelOutput = TRUE)
    }
    # verify a minimal amount at each end
    tune_load_ncells <- as.integer(isolate(input$tune_load_ncells))
    req(tune_load_ncells)
    err <- min(c(bp, 1-bp) * tune_load_ncells) < 10
    if (err) {
      showNotification("Too extreme percentiles.", type = "error")
      req(FALSE, cancelOutput = TRUE)
    }
    # append options to quantile
    if (input$tune_batch_zero)
      batch_params <- paste0(batch_params, ",exclude_zeroes")
    if (input$tune_batch_transf)
      batch_params <- paste0(batch_params, ",transform")
    req(batch_method, batch_params)
    message(batch_method, "-", batch_params)
    # retrieve FB and apply parameters
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    my_fb_ref <- fb_tr_set_params(
      my_fb_ref,
      transf_method = transf_method,
      transf_params = transf_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    my_fb_ref <- fb_bn_set_params(
      my_fb_ref,
      batchnorm_method = batch_method,
      batchnorm_params = batch_params,
      overwrite = TRUE,
      channels = tune_channel
    )
    # fit model
    my_fb_ref <- fb_bn_model_batch(
      my_fb_ref,
      channels = tune_channel
    )
    # correct batch effect
    my_fb_ref_adj <- fb_bn_correct_batch(
      my_fb_ref
    )
    # TODO: to speed up, only correct the current channel and push the result in
    # the exprs of my_fb_ref_adj
    # update mem
    mem$my_fb_ref <- my_fb_ref
    mem$my_fb_ref_adj <- my_fb_ref_adj
    rm(my_fb_ref, my_fb_ref_adj, my_fb)
    review_plots(runif(1))
  })

  output$tune_main_plot_adj <- renderPlot({
    if (debugging()) browser()
    warning("Plot Adj")
    my_fb_ref_adj <- mem$my_fb_ref_adj
    req(my_fb_ref_adj)
    channel <- isolate(input$tune_channel)
    req(channel)
    idx <- guess_match_channels(my_fb_ref_adj, channel)
    req(idx)
    channel <- isolate(my_fb_ref_adj@panel$fcs_colname[idx])
    req(channel)

    # plot corrected batch effect
    fb_plot_ridgelines(
      my_fb_ref_adj,
      channel,
      group_by = "batch_id",
      cof = 8,
      cut_lower_than = -5,
      title = "Normalized"
    )
  })

  output$tune_main_plot_raw <- renderPlot({
    if (debugging()) browser()
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    channel <- isolate(input$tune_channel)
    req(channel)
    idx <- guess_match_channels(my_fb_ref, channel)
    req(idx)
    channel <- isolate(my_fb_ref@panel$fcs_colname[idx])
    req(channel)

    # plot
    fb_plot_ridgelines(
      my_fb_ref,
      channel,
      group_by = "batch_id",
      cof = 8,
      cut_lower_than = -5,
      title = "Raw"
    )
  })


  # ========== REVIEW SCALING COEFF

  # Set the UI for the plots
  output$revcoef_ui_plots <- renderUI({
    height <- input$revcoef_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revcoef_main_plot", width = "100%", height = height
      )
    )
  })

  output$revcoef_main_plot <- renderPlot({
    warning("revcoef_main_plot")
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    my_fb_ref_adj <- mem$my_fb_ref_adj
    req(my_fb_ref_adj)
    channel <- input$revcoef_channel
    req(channel)
    linear_scale <- input$revcoef_scale
    req(linear_scale)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]
    fb_bn_plot_coef(my_fb_ref, my_fb_ref_adj, channel,
                    calc_mode = linear_scale)
  })


  # ========== REVIEW TRANSFORM

  # Set the UI for the plots
  output$revtran_ui_plots <- renderUI({
    height <- input$revtran_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revtran_main_plot", width = "100%", height = height
      )
    )
  })

  output$revtran_main_plot <- renderPlot({
    warning("revtran_main_plot")
    review_plots()
    channel <- input$revtran_channel
    req(channel)
    file_no <- as.integer(input$revtran_batch)
    req(file_no)
    jitter <- as.numeric(input$revtran_jitter)
    req(jitter)
    point <- input$revtran_point
    req(point)
    gg_ncol <- as.numeric(input$revtran_ncol)
    req(gg_ncol)

    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel)
    channel <- my_fb_ref@panel$fcs_colname[idx]
    channel_name <- my_fb_ref@panel$antigen[idx]

    if (!is.na(as.integer(point))) point <- as.integer(point)

    my_fb_ref_adj <- isolate(mem$my_fb_ref_adj)
    req(my_fb_ref_adj)
    df_raw <- fb_get_exprs(my_fb_ref, "data.frame", transformed = TRUE)
    df_adj <- fb_get_exprs(my_fb_ref_adj, "data.frame", transformed = TRUE)

    df <- cbind(df_raw[ , seq(ncol(df_raw)-2)], df_adj)
    colnames(df)[seq(2*(ncol(df_raw)-2))] <- paste0(
      colnames(df)[seq(2*(ncol(df_raw)-2))],
      rep(c("_raw", "_adj"), each = ncol(df_raw)-2))
    mapping <- factor(
      my_fb_ref@pheno$batch_id, levels = my_fb_ref@pheno$batch_id)
    names(mapping) <- my_fb_ref@pheno$file_no
    df$batch_id <- mapping[as.character(df$file_no)]

    if (debugging()) browser()
    if (0 %in% file_no) {
      file_no <- c(setdiff(file_no, 0), setdiff(df$file_no, file_no))
    }
    file_nos <- c(file_no)
    gg <- ggplot(subset(df, file_no %in% file_nos),
                 aes_(x = as.name(colnames(df)[idx]),
                      y = as.name(colnames(df)[idx+ncol(df_raw)-2]))) +
      geom_jitter(width = jitter, height = jitter,
                  pch = point, cex = 2, col = "#11222222") +
      geom_abline(slope = 1, intercept = 0, col = grey(.3)) +
      theme_minimal() + theme(aspect.ratio = 1) +
      # facet_wrap(~file_no, ncol = gg_ncol) +
      facet_wrap(~batch_id, ncol = gg_ncol) +
      labs(x = paste0(channel_name, " raw"),
           y = paste0(channel_name, " normd"))
    print(gg)
  })


  # ========== REVIEW BI-PARAMETRIC PLOT

  # Set the UI for the plots
  output$revbipl_ui_plots <- renderUI({
    height <- input$revbipl_plot_height
    req(height)
    tagList(
      # tags$legend("Transformation"),
      plotOutput(
        "revbipl_main_plot", width = "100%", height = height
      )
    )
  })

  output$revbipl_main_plot <- renderPlot({
    warning("revbipl_main_plot")
    if (debugging()) browser()
    review_plots()
    channel_x <- input$revbipl_channel_x
    req(channel_x)
    channel_y <- input$revbipl_channel_y
    req(channel_y)
    file_no <- as.integer(input$revbipl_batch)
    req(file_no)
    hexbin <- as.numeric(input$revbipl_hexbin)
    req(hexbin)
    aspect <- as.numeric(input$revbipl_aspect)
    req(aspect)
    gg_ncol <- as.numeric(input$revbipl_ncol)
    req(gg_ncol)

    my_fb_ref <- isolate(mem$my_fb_ref)
    req(my_fb_ref)
    idx <- guess_match_channels(my_fb_ref, channel_x)
    channel_x <- my_fb_ref@panel$fcs_colname[idx]
    channel_name_x <- my_fb_ref@panel$antigen[idx]
    idx <- guess_match_channels(my_fb_ref, channel_y)
    channel_y <- my_fb_ref@panel$fcs_colname[idx]
    channel_name_y <- my_fb_ref@panel$antigen[idx]

    my_fb_ref_adj <- isolate(mem$my_fb_ref_adj)
    req(my_fb_ref_adj)
    df_raw <- fb_get_exprs(my_fb_ref, "data.frame", transformed = TRUE)
    df_adj <- fb_get_exprs(my_fb_ref_adj, "data.frame", transformed = TRUE)

    df_all <- rbind(cbind(df_raw, normed = "raw"),
                    cbind(df_adj, normed = "normd"))
    df_all$normed <- factor(df_all$normed, levels = c("raw", "normd"))
    mapping <- factor(
      my_fb_ref@pheno$batch_id, levels = my_fb_ref@pheno$batch_id)
    names(mapping) <- my_fb_ref@pheno$file_no
    df_all$batch_id <- mapping[as.character(df_all$file_no)]

    if (debugging()) browser()
    if (0 %in% file_no) {
      file_no <- c(setdiff(file_no, 0), setdiff(df_all$file_no, file_no))
    }
    file_nos <- c(file_no)
    df_tmp <- subset(df_all, file_no %in% file_nos)
    df_tmp$facet <- sprintf("%s | %s", df_tmp$batch_id, df_tmp$normed)
    # ordered factor
    dd <- unique(df_tmp[, c("batch_id", "normed", "facet")])
    oo <- order(dd$batch_id, dd$normed)
    df_tmp$facet = factor(df_tmp$facet, levels = dd$facet[oo])

    gg_pal <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
    asinh_d <- function(x) asinh(x/10)
    asinh_i <- function(x) 10*sinh(x)
    gg <- ggplot(df_tmp, aes_(x = as.name(channel_x), y = as.name(channel_y))) +
      geom_hex(bins = hexbin) +
      scale_fill_gradientn(colours = gg_pal,
                           trans = scales::trans_new("asinh", asinh_d, asinh_i)) +
      # scale_fill_gradientn(colours = gg_pal, trans = "sqrt") +
      theme_minimal() + theme(aspect.ratio = aspect) +
      # facet_grid(batch_id ~ normed) +
      facet_wrap(~facet, ncol = 2*gg_ncol) +
      labs(x = channel_name_x, y = channel_name_y)
    print(gg)
  })


  # ========== PRE-VIEW NORMALISATION

  observeEvent(input$proc_preview_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!')
    )
    if (debugging()) browser()
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)

    # store before preview
    showNotification("Writing flowBunch ")
    fb_write(my_fb)
    # plot raw, ie before
    showNotification("Preview RAW started")
    pdf(fb_file_name(my_fb_ref, "-refs_raw.pdf"), width = 15,
        height = 2.5+0.20*nrow(my_fb_ref@pheno))
    fb_plot_ridgelines(my_fb_ref, title = "Raw")
    dev.off()
    showNotification("Preview RAW finished")

    # apply models
    # showNotification("Normalisation started")
    # my_fb_ref_mod <- fb_bn_model_batch(
    #   my_fb_ref
    # )
    # my_fb_ref_adj <- fb_bn_correct_batch(
    #   my_fb_ref_mod
    # )
    # showNotification("Normalisation finished")
    my_fb_ref_adj <- isolate(mem$my_fb_ref_adj)
    req(my_fb_ref_adj)

    # plot normed, ie after
    showNotification("Preview NORMD started")
    pdf(fb_file_name(my_fb_ref, "-refs_normed.pdf"), width = 15,
        height = 2.5+0.20*nrow(my_fb_ref_adj@pheno))
    fb_plot_ridgelines(my_fb_ref_adj, title = "Normd")
    dev.off()
    showNotification("Preview NORMD finished")

    # plot normed, ie after
    showNotification("Preview COEFF started")
    pdf(fb_file_name(my_fb_ref, "-coeff_diff_tr.pdf"), height = 9,
        width = 4 + length(unique(my_fb_ref@pheno$batch_id)))
    fb_bn_plot_coef(my_fb_ref, my_fb_ref_adj, calc_mode = "diff_tr")
    dev.off()
    pdf(fb_file_name(my_fb_ref, "-coeff_ratio_un.pdf"), height = 9,
        width = 4 + length(unique(my_fb_ref@pheno$batch_id)))
    fb_bn_plot_coef(my_fb_ref, my_fb_ref_adj, calc_mode = "ratio_un")
    dev.off()
    showNotification("Preview COEFF finished")
  })


  # ========== PROCESS

  observeEvent(input$proc_apply_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!'),
      need(input$create_fcs_dir, 'Check directory of FCS!')
    )
    if (debugging()) browser()
    my_fb_ref <- mem$my_fb_ref
    req(my_fb_ref)
    my_fb <- mem$my_fb
    req(my_fb)
    # copy transformations
    showNotification("Normalisation started")
    my_fb@procs <- my_fb_ref@procs
    my_fb@panel <- my_fb_ref@panel
    my_fb <- fb_freeze_file_no(my_fb) # ?, modeling
    # store before processing
    showNotification("Writing flowBunch ")
    fb_write(my_fb)
    # update prefix and suffix for writing out FCS files
    my_fb@output$fcs$prefix <- input$proc_file_prefix
    my_fb@output$fcs$suffix <- input$proc_file_suffix
    # apply models
    showNotification("File processing started")
    withCallingHandlers({
      shinyjs::html("proc_log", "\n")
      message(format(Sys.time(), "%a %b %d %Y %X TZ(%z)"), appendLF = TRUE)
      fb_bn_correct_fcs(my_fb, hook = showNotification)
      message(format(Sys.time(), "%a %b %d %Y %X TZ(%z)"), appendLF = TRUE)
    },
    message = function(m) {
      shinyjs::html(id = "proc_log", html = m$message, add = TRUE)
    })
    showNotification("Normalisation finished")
  })

  # This opens an explorer to ease the editing of pheno and panel files
  observeEvent(input$proc_explorer, {
    my_fb <- mem$my_fb
    req(my_fb)
    fb_path <- fb_file_path(my_fb)
    if (.Platform[['OS.type']] == "windows") {
      shell.exec(fb_path)
      showNotification("A file explorer is opened", type = "message")
    } else {
      message(fb_path)
      showModal(modalDialog(
        title = "Open project dir",
        "It is not possible to open a file explorer on this OS.",
        "Copy and paste the following path in your file manager. ", fb_path
      ))
    }
  })


  # ========== RE-VIEW NORMALISATION

  output$proc_review_ui <- renderUI({
    req(review)
    tagList(
      tags$fieldset(
        tags$legend("QC: review the normalization"),
      ),
      tags$div(
        class="form-group shiny-input-container",
        tags$label("To review all channels in all files in all batches, define the amount of cells per FCS file and click the 'Review' button"), tags$br(),
        splitLayout(
          textInput(
            "proc_review_ncells",
            NULL,
            5000
          ),
          actionButton(
            "proc_review_button",
            "Review", width = "100%"
          )
        )
      )
    )
  })

  observeEvent(input$proc_review_button, {
    validate(
      need(input$create_proj_name, 'Check project name!'),
      need(input$create_proj_dir, 'Check project directory!')
    )
    if (debugging()) browser()
    my_fb <- mem$my_fb
    req(my_fb)
    n_cells <- as.numeric(input$proc_review_ncells)
    req(n_cells, n_cells >= 2000, n_cells <= 20000)

    # plot raw, ie before
    showNotification("Review RAW started")
    # load data to assess density plot
    my_fb_raw <- fb_load_cells(
      my_fb, n_cells = n_cells
    )
    # plot raw, ie before normalization
    pdf(fb_file_name(my_fb_raw, "-raw.pdf"), width = 15,
        height = 2.5+0.20*nrow(my_fb_raw@pheno))
    fb_plot_ridgelines(my_fb_raw, title = "Raw")
    dev.off()
    showNotification("Review RAW finished")

    # check process succeeded
    fcs_dir <- fb_file_path(my_fb, my_fb@output$fcs$basen)
    if (!dir.exists(fcs_dir)) {
      showNotification("Normalized files not found")
      return()
    }

    # plot normed, ie after
    showNotification("Review NORMD started")
    my_fb_adj <- fb_open(
      project_name = my_fb@output$fcs$basen,
      project_dir = fb_file_path(my_fb)
    )
    # load data to assess density plot
    my_fb_adj <- fb_load_cells(
      my_fb_adj, n_cells = n_cells
    )
    # plot normed, ie after normalisation, in the same project
    pdf(fb_file_name(my_fb, "-normd.pdf"), width = 15,
        height = 2.5+0.20*nrow(my_fb_adj@pheno))
    fb_plot_ridgelines(my_fb_adj, title = "Normd")
    dev.off()
    showNotification("Review NORMD finished")
  })


  # ========== OPTIONS

  # Apply
  observeEvent(input$opt_apply_button, {
    opt_ui_plot_height <- as.integer(input$opt_ui_plot_height)
    validate(
      need(opt_ui_plot_height > 200, 'Integer > 200!')
    )
    opt_ui_plot_height <- max(200, as.integer(opt_ui_plot_height))
    prefs$ui_plot_height <- opt_ui_plot_height
  })

}
