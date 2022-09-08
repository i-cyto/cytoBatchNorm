ui <- dashboardPage(
  dashboardHeader(title = "cytoBatchNorm"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Create Bunch", tabName = "Create",
               icon = icon("import", lib = "glyphicon")),
      # menuItem("Load",  tabName = "Load", icon = icon("th")),
      menuItem("Setup Batch", tabName = "Setup",
               icon = icon("pencil", lib = "glyphicon")),
      menuItem("Tune Params", tabName = "Tune",
               icon = icon("wrench", lib = "glyphicon")),
      menuItem("Process", tabName = "Process",
               icon = icon("ok", lib = "glyphicon")),
      # menuItem("Options", tabName = "Options", icon = icon("dashboard")),
      # menuItem("Save", tabName = "Save", icon = icon("dashboard")),
      NULL
    ),
    uiOutput("debug_ui")
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "Create",
        box(
          title = "Create", width = 12,
          sidebarPanel(
            width = 4,
            textInput(
              "create_proj_name",
              "Set the name of the project",
              "cytobatchnorm"
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Select a directory to store the project"),
              tags$br(),
              shinyDirButton(
                "create_proj_dir",
                "Browse",
                "Select a directory to store the project"
              )),
            selectInput(
              "create_cytometer",
              "Select the cytometry technology",
              choices = c("mass", "flow", "spectral")
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Select the directory of all the FCS files"),
              tags$br(),
              shinyDirButton(
                "create_fcs_dir",
                "Browse",
                "Select the directory of all the FCS files"
              )),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "create_create_button",
                "Create", width = "100%"
              ))
          ),

          mainPanel(
            width = 8,
            tabsetPanel(
              id = "create_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("create_log")),
              tabPanel("Set", verbatimTextOutput("create_set")),
              tabPanel("Pheno", dataTableOutput("create_pheno_table")),
              tabPanel("Panel", dataTableOutput("create_panel_table"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Setup",
        box(
          title = "Setup", width = 12,
          sidebarPanel(
            width = 4,
            tags$fieldset(
              tags$legend("Pheno: identify batches, anchors, reference"),
              textInput(
                "setup_batch_pattern",
                "Set the pattern to determine batches",
                ".+?_([Bb]atch\\d+)_.+"
              ),
              textInput(
                "setup_ref_sample_pattern",
                "Set the pattern to identify anchors",
                "_c20$"
              ),
              # TODO: test button
              # TODO: UI for default processing parameters
              # tags$legend("Panel: set default processing parameters"),
              # selectInput(
              #   "setup_batch_method",
              #   "Method to adjust batch effect",
              #   choices = c("percentile_hi", "percentile_lohi"),
              #   selected = ""),
              # textInput(
              #   "setup_batch_params",
              #   "Percentile to adjust batch effect",
              #   ""),
              # checkboxInput(
              #   "setup_batch_zero",
              #   "Exclude the zeroes in percentiles",
              #   FALSE),
              # checkboxInput(
              #   "setup_batch_transf",
              #   "Apply to transformed intensities",
              #   FALSE)
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "setup_setup_button",
                "Finalize", width = "100%"
              )
            ),
            tags$fieldset(
              tags$legend("Edit and Reload from disk"),
              class="form-group shiny-input-container",
              tags$text("Now edit the panel file to set batch model parameters. You could also edit the pheno file."), tags$br(),
              actionButton(
                "setup_explorer",
                "Open project dir",
                width = "100%"
              ), tags$br(), tags$br(),
              tags$label("Once done, click below"), tags$br(),
              actionButton(
                "setup_reload_button",
                "Reload", width = "100%"
              )
            ),
          ),
          mainPanel(
            width = 8,
            tabsetPanel(
              id = "setup_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("setup_log")),
              tabPanel("Pheno", dataTableOutput("setup_pheno_table")),
              tabPanel("Panel", dataTableOutput("setup_panel_table"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Tune",
        tabBox(
          id = "Tune_tabset", width = 12,
          title = "Tune params",
          tabPanel(
            "Tune parameters",
            fluidRow(
              sidebarPanel(
                width = 4,
                textInput(
                  "tune_load_ncells",
                  "Define the amount of cells per FCS file",
                  5000
                ),
                tags$div(
                  class="form-group shiny-input-container",
                  tags$label("Extract a sample of cells"), tags$br(),
                  actionButton(
                    "tune_sample_button",
                    "Sample", width = "100%"
                  )),
                selectizeInput(
                  "tune_channel",
                  "Select the channel to process",
                  choices = ""),
                tags$fieldset(
                  tags$legend("Batch adjust"),
                  selectInput(
                    "tune_batch_method",
                    "Set the method to adjust batch effect",
                    choices = c("percentile_hi",
                                "percentile_lohi",
                                "percentile_lohi_pos",
                                "none",
                                "quantiles")
                  ),
                  textInput(
                    "tune_batch_params",
                    "Set the percentile to adjust batch effect"
                  ),
                  checkboxInput(
                    "tune_batch_zero",
                    "Exclude the zeroes from percentiles"
                  ),
                  checkboxInput(
                    "tune_batch_transf",
                    "Apply to transformed intensities"
                  )
                ),
                tags$fieldset(
                  tags$legend("Transform"),
                  selectInput(
                    "tune_transf_method",
                    "Set the function",
                    choices = c("asinh")
                  ),
                  textInput(
                    "tune_transf_params",
                    "Set the cofactor"
                  )
                ),
                tags$fieldset(
                  tags$legend("Graphical options"),
                  textInput("tune_plot_height", "Plot height", "450")
                )
              ),
              mainPanel(
                width = 8,
                uiOutput("tune_ui_plot_adj"),
                uiOutput("tune_ui_plot_raw")
              )
            )
          ),
          tabPanel(
            "Review scaling",
            fluidRow(
              sidebarPanel(
                width = 4,
                selectizeInput(
                  "revcoef_channel",
                  "Select the channel",
                  choices = ""),
                tags$fieldset(
                  tags$legend("Graphical options"),
                  selectInput(
                    "revcoef_scale",
                    "Display coefficients as",
                    choices = c(
                      "difference of transformed values"="diff_tr",
                      "ratio of untransformed values"="ratio_un"
                    ), selected = "diff"),
                  textInput("revcoef_plot_height", "Plot height", "800")
                )
              ),
              mainPanel(
                width = 8,
                uiOutput("revcoef_ui_plots")
              )
            )
          ),
          tabPanel(
            "Review functions",
            fluidRow(
              sidebarPanel(
                width = 4,
                selectizeInput(
                  "revtran_channel",
                  "Select the channel",
                  choices = ""),
                selectizeInput(
                  "revtran_batch",
                  "Select the batch",
                  choices = "",
                  multiple = TRUE),
                tags$fieldset(
                  tags$legend("Graphical options"),
                  selectInput(
                    "revtran_ncol",
                    "Select the number of columns",
                    choices = 2:9, selected = 4),
                  selectInput(
                    "revtran_jitter",
                    "Amount of jittering",
                    choices = c(0, 0.05, 0.1, 0.3, 0.5),
                    selected = 0.1),
                  selectInput(
                    "revtran_point",
                    "Point style",
                    choices = c("smallest"=".", "small"="20", "medium"="16"),
                    selected = "."),
                  textInput("revtran_plot_height", "Plot height", "800")
                )
              ),
              mainPanel(
                width = 8,
                uiOutput("revtran_ui_plots")
              )
            )
          ),
          tabPanel(
            "Review bi-param plot",
            fluidRow(
              sidebarPanel(
                width = 4,
                selectizeInput(
                  "revbipl_channel_x",
                  "Select the X channel",
                  choices = ""),
                selectizeInput(
                  "revbipl_channel_y",
                  "Select the Y channel",
                  choices = ""),
                selectizeInput(
                  "revbipl_batch",
                  "Select the batch",
                  choices = "",
                  multiple = TRUE),
                tags$fieldset(
                  tags$legend("Graphical options"),
                  selectInput(
                    "revbipl_ncol",
                    "Select the number of columns",
                    choices = 1:5, selected = 3),
                  selectInput(
                    "revbipl_hexbin",
                    "Number of bins",
                    choices = c("32", "64", "96", "128", "192"),
                    selected = "64"),
                  selectInput(
                    "revbipl_aspect",
                    "Aspect ratio of plots",
                    choices = c("1" = "1", "1/2" = "0.5", "1/3" = "0.33", "1/4" = "0.25"),
                    selected = "1"),
                  textInput("revbipl_plot_height", "Plot height", "900")
                )
              ),
              mainPanel(
                width = 8,
                uiOutput("revbipl_ui_plots")
              )
            )
          )
        )
      ),

      tabItem(
        tabName = "Process",
        box(
          title = "Process", width = 12,
          sidebarPanel(
            width = 4,
            tags$fieldset(
              tags$legend("QC: preview the normalization"),
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("To preview all channels in all batches for all reference files based on the sampled cells, click the 'Preview' button"), tags$br(),
              actionButton(
                "proc_preview_button",
                "Preview", width = "100%"
              )
            ),
            tags$fieldset(
              tags$legend("Process"),
            ),
            tags$div(
              class="form-group shiny-input-container",
              tags$label("Once you have reviewed all the channels, enter the optional prefix/suffix and click the 'Process' button. Once the process is complete, open the directory"), tags$br(),
              textInput(
                "proc_file_prefix",
                "File name prefix"
              ),
              textInput(
                "proc_file_suffix",
                "File name suffix",
                "_cbn"
              ),
              splitLayout(  # alt. div
                actionButton(
                  "proc_apply_button",
                  "Process", width = "100%"
                ),
                actionButton(
                  "proc_explorer",
                  "Open project dir", width = "100%"
                )
              )
            ),
            uiOutput("proc_review_ui")
          ),

          mainPanel(
            width = 8,
            tabsetPanel(
              id = "proc_tabset",
              type = "pills",
              tabPanel("Log", verbatimTextOutput("proc_log"))
            )
          )
        )
      ),

      tabItem(
        tabName = "Options",
        box(
          title = "Options", width = 12,
          tags$div(
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$label("Graphical options"), tags$br(),
                textInput("opt_ui_plot_height", "Plot height", "600")
              )
            ),
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$br()
              )
            ),
            sidebarPanel(
              width = 4,
              tags$div(
                class="form-group shiny-input-container",
                tags$br()
              )
            )
          ),
          tags$div(
            column(
              width = 12,
              actionButton(
                "opt_apply_button",
                "Apply", width = "100%"
              )
            )
          ),
        )
      ),

      tabItem(
        tabName = "Save",
        NULL
      )
    )
  )
)
