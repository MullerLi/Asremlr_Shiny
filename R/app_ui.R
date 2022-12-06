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
          title = "Spatial Analysis",
          color = "gray",
          href = NULL,
          image = "www/mm.png",
          opacity = 0.8
        ),

        status = "navy",
        fixed = F,
        "Web Application for Spatial Analysis!",

        rightUi =

      ),


      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "success",
        # title = "Mr.Bean",
        # brandColor = "white",
        # url = "https://mrpackages.netlify.app/",
        # src = "www/beans3.png",
        elevation = 3,
        # opacity = 0.8,
        fixed = F,


        bs4SidebarMenu(id = "tabs",
                       bs4SidebarHeader("SET analysis"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "SET-analysis",
                         icon = shiny::icon("braille"),
                         startExpanded = F,
                         bs4SidebarMenuItem(
                           text = "DataUpload",
                           tabName = "Data",
                           icon = shiny::icon("file-upload")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs",
                                      bs4Badge("new",
                                               position = "right",
                                               color = "success"))) ,
                           tabName = "SET_analysis",
                           icon = shiny::icon("circle-thin")
                         )
                       ),


                       bs4SidebarHeader("TI analysis"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "SET-analysis",
                         icon = shiny::icon("braille"),
                         startExpanded = F,
                         bs4SidebarMenuItem(
                           text = "DataUpload",
                           tabName = "Data1",
                           icon = shiny::icon("file-upload")
                         ),
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs",
                                      bs4Badge("new",
                                               position = "right",
                                               color = "success"))) ,
                           tabName = "TI_analysis",
                           icon = shiny::icon("circle-thin")
                         )

                       ),


                       bs4SidebarHeader("ASReml"),
                       # Single spatial analysis ASReml
                       bs4SidebarMenuItem(
                         text = "Single-Site",
                         icon = shiny::icon("braille"),
                         startExpanded = F,


                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs",
                                      bs4Badge("new",
                                               position = "right",
                                               color = "success"))) ,
                           tabName = "spats_asreml",
                           icon = shiny::icon("circle-thin")
                         )
                       ),


                       # Un-replicated analysis
                       bs4SidebarMenuItem(
                         "Unreplicated",
                         icon = shiny::icon("crosshairs"),
                         startExpanded = F,
                         bs4SidebarMenuSubItem(
                           HTML(paste("Model Specs",
                                      bs4Badge("new",
                                               position = "right",
                                               color = "danger"))) ,
                           tabName = "aug_model",
                           icon = shiny::icon("circle-thin"))))
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
          bs4TabItem(
            tabName = "Data1",
            mod_import_dt_ti_ui("import_dt_ti_ui_1")
          ),

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

        )
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
