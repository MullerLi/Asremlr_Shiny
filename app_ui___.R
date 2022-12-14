#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinyWidgets bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic


    bs4Dash::bs4DashPage(
      title = "MrBean",
      skin = NULL,
      freshTheme = NULL,
      preloader = NULL,
      options = NULL,
      fullscreen = TRUE,
      help = FALSE,
      dark = FALSE,
      scrollToTop = FALSE,
      header = bs4DashNavbar(
        title = dashboardBrand(
          title = "MrBean",
          color = "white",
          href = "https://mrpackages.netlify.app/",
          image = "www/beans3.png",
          opacity = 0.8
        ),
        status = "white",
        fixed = F,
        # leftUi = tagList(
        #   HTML("<a href='https://www.buymeacoffee.com/mrbean'><img src='https://img.buymeacoffee.com/button-api/?text=Buy me a coffee&emoji=&slug=mrbean&button_colour=039a16&font_colour=ffffff&font_family=Cookie&outline_colour=ffffff&coffee_colour=FFDD00'></a>")
        # ),
        HTML("<script type='text/javascript' src='https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js' data-name='bmc-button' data-slug='mrbean' data-color='#FFFFFF' data-emoji=''  data-font='Cookie' data-text='Buy MrBean a coffee' data-outline-color='#000' data-font-color='#000' data-coffee-color='#fd0' ></script>"),
        # HTML("<a href='https://www.buymeacoffee.com/mrbean'><img src='https://img.buymeacoffee.com/button-api/?text=Buy me a coffee&emoji=&slug=mrbean&button_colour=039a16&font_colour=ffffff&font_family=Cookie&outline_colour=ffffff&coffee_colour=FFDD00'></a>"),
        # HTML('<a href="https://www.buymeacoffee.com/mrbean"><img src="https://img.buymeacoffee.com/button-api/?text=Buy me a coffee&emoji=&slug=mrbean&button_colour=039a16&font_colour=ffffff&font_family=Cookie&outline_colour=ffffff&coffee_colour=FFDD00"></a>'),
        "Web Application for Spatial Analysis!",
        rightUi = bs4DropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          href = "http://buymeacoffee.com/mrbean",
          messageItem(
            from = "MrBean",
            message  = "If you want to contribute...",
            time = "today", image = "www/beans3.png",
            href = "http://buymeacoffee.com/mrbean"
          )
        )
      ),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "success",
        # title = "Mr.Bean",
        # brandColor = "white",
        # url = "https://mrpackages.netlify.app/",
        # src = "www/beans3.png",
        elevation = 3,
        # opacity = 0.8,
        fixed = F,
        bs4SidebarMenu(id = "tabs",
                       # bs4SidebarHeader("Menu"),
                       # # Import data
                       # bs4SidebarMenuItem(
                       #   "Data",icon = shiny::icon("database"),startExpanded = F,
                       #   bs4SidebarMenuItem(
                       #     text = "Upload", tabName = "Data", icon = shiny::icon("file-upload")
                       #   ),
                       #   bs4SidebarMenuItem(
                       #     text = "Descriptives", tabName = "descriptives",icon = shiny::icon("chart-line")
                       #   ),
                       #   bs4SidebarMenuItem(
                       #     text = "Distribution",
                       #     tabName = "distrib",
                       #     icon = shiny::icon("chart-area")
                       #   )
                       # ),

                       bs4SidebarHeader("Smooth analysis"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "SET-analysis", icon = shiny::icon("braille"), startExpanded = F,
                         bs4SidebarMenuItem(
                           text = "DataUpload", tabName = "Data", icon = shiny::icon("file-upload")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", color = "success"))) ,
                           tabName = "SET_analysis", icon = shiny::icon("circle-thin")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("TI_analysis", bs4Badge("new", position = "right", color = "success"))) ,
                           tabName = "TI_analysis", icon = shiny::icon("circle-thin")
                         )
                       ),

                       # bs4SidebarHeader("Time interval analysis"),
                       # # Single spatial analysis ASReml
                       # bs4SidebarMenuItem(
                       #   # text = "TI-analysis", icon = shiny::icon("braille"), startExpanded = F,
                       #   # bs4SidebarMenuItem(
                       #   #   text = "DataUpload", tabName = "Data1", icon = shiny::icon("file-upload")
                       #   # ),
                       #   bs4SidebarMenuSubItem(
                       #     HTML(paste("TI_analysis", bs4Badge("new", position = "right", color = "success"))) ,
                       #     tabName = "TI_analysis", icon = shiny::icon("circle-thin")
                       #   )
                       # ),


                       bs4SidebarHeader("ASReml"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "Single-Site", icon = shiny::icon("braille"), startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", color = "success"))) ,
                           tabName = "spats_asreml", icon = shiny::icon("circle-thin")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("BLUPs/BLUEs", bs4Badge("new", position = "right", color = "success"))) ,
                           tabName = "spats_asreml_effects", icon = shiny::icon("circle-thin")
                         )
                       ),
                       # Un-replicated analysis
                       bs4SidebarMenuItem(
                         "Unreplicated", icon = shiny::icon("crosshairs"), startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs", bs4Badge("new", position = "right", color = "danger"))) ,
                           tabName = "aug_model", icon = shiny::icon("circle-thin")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("BLUPs/BLUEs", bs4Badge("new", position = "right", color = "danger"))) ,
                           tabName = "aug_result", icon = shiny::icon("circle-thin")
                         )
                       )
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          # chooseSliderSkin("Modern"),
          # bs4TabItem(
          #   tabName = "home",
          #   mod_home_module1_ui("home_module1_ui_1")
          # ),
          # Import data
          bs4TabItem(
            tabName = "Data",
            mod_import_dt_ui("import_dt_ui_1")
          ),
          # bs4TabItem(
          #   tabName = "Data1",
          #   mod_import_dt_ti_ui("import_dt_ti_ui_1")
          # ),
          bs4TabItem(
            tabName = "descriptives",
            HTML('<h1 style="font-weight: bold; color: #00a65a;">Descriptive Plots</h1>'),
            fluidRow(
              column(width=6,
                     mod_descrip_raw_ui("descrip_raw_ui_1", "Scatterplot", "fadeInLeftBig",  "fadeOutLeftBig", T)
              ),
              column(width=6,
                     mod_descrip_raw_ui("descrip_raw_ui_2", "Boxplot", "fadeInRightBig",  "fadeOutRightBig", F )
              )
            )
          ),
          bs4TabItem(
            tabName = "distrib",
            mod_distribution_ui("distribution_ui_1")
          ),
          # # Single spatial analysis SpATS
          # bs4TabItem(
          #   tabName = "modelo",
          #   mod_spats_single_ui("spats_single_ui_1") ,
          #   mod_info_spats_ui("info_spats_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "blupspat",
          #   mod_effects_spats_ui("effects_spats_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "resispat",
          #   mod_residuals_spats_ui("residuals_spats_ui_1"),
          #   HTML( "<script data-name='BMC-Widget' src='https://cdnjs.buymeacoffee.com/1.0.0/widget.prod.min.js' data-id='mrbean' data-description='Support me on Buy me a coffee!' data-message='Thank you for visiting.' data-color='#28a745' data-position='right' data-x_margin='18' data-y_margin='18'></script>" )
          # ),
          # # Multiple-single analysis
          # bs4TabItem(
          #   tabName = "msa",
          #   mod_MSA_ui("MSA_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "msa_result",
          #   mod_MSA_results_ui("MSA_results_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "multi_trait",
          #   mod_M_traits_ui("M_traits_ui_1")
          # ),

          # SET analysis
          bs4TabItem(
            tabName = "SET_analysis",
            mod_SET_analysis_ui("SET_analysis_ui_1")
          ),
          bs4TabItem(
            tabName = "TI_analysis",
            mod_TI_analysis_ui("TI_analysis_ui_1")
          ),

          # Single spatial analysis ASReml
          bs4TabItem(
            tabName = "spats_asreml",
            mod_spats_asreml_ui("spats_asreml_ui_1")
          ),
          bs4TabItem(
            tabName = "spats_asreml_effects",
            mod_spats_asreml_effects_ui("spats_asreml_effects_ui_1")
          ),
          # Un-replicated analysis
          bs4TabItem(
            tabName = "aug_model",
            mod_aug_model_ui("aug_model_ui_1")
          ),
          bs4TabItem(
            tabName = "aug_result",
            mod_aug_result_ui("aug_result_ui_1")
          )
          # # Model selector
          # bs4TabItem(
          #   tabName = "asreml_selector",
          #   mod_asreml_selector_ui("asreml_selector_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "asr_sel_selected",
          #   mod_asreml_selector_effects_ui("asreml_selector_effects_ui_1")
          # ),
          # # Two-Stage MET
          # bs4TabItem(
          #   tabName = "met",
          #   mod_MET_ui("MET_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "met_result",
          #   mod_MET_results_ui("MET_results_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "met_fa",
          #   mod_MET_FA_ui("MET_FA_ui_1")
          # ),
          # # lme4 basic models
          # bs4TabItem(
          #   tabName = "mixed",
          #   mod_lme4_single_ui("lme4_single_ui_1")
          # ),
          # bs4TabItem(
          #   tabName = "boxes",
          #   mod_residuals_lme4_ui("residuals_lme4_ui_1")
          # ),
          # # About
          # bs4TabItem(
          #   tabName = "valueboxes",
          #   mod_about_ui("about_ui_1")
          # )
        )
      ),
      controlbar = bs4DashControlbar(
        skin = "light",
        # pinned = TRUE,
        br(),
        col_4(),
        col_4(
          h5("Go to:"),
          actionLink(inputId = "toAwesome00", label = "Home", icon = icon("home")),br(),
          actionLink(inputId = "toAwesome11", label = "Data", icon = icon("database")),br(),
          actionLink(inputId = "toAwesome22", label = "Spatial", icon = icon("braille")),br(),
          actionLink(inputId = "toAwesome33", label = "About", icon = icon("bar-chart-o")),br()
        ),
        col_4()
      ),
      footer = bs4DashFooter(
        fixed = F,
        left  = tagList(
          "v.2.0.6",
          HTML("&nbsp; &nbsp; &nbsp; &nbsp;"),
          a(
            href = "https://www.linkedin.com/in/johan-steven-aparicio-arce-b68976193/",
            target = "_blank", "J.Aparicio@cgiar.org"
          )
        ),
        right = "2020"
      )
    )
##########

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SETapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
