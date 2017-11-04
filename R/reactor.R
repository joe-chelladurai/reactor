globalVariables(".rs.restartR")

#' Reactor - a lite-GUI for R based in Shiny
#'
#' \emph{Reactor} provides users an ability to leverage and debug functions and objects
#' inside a Shiny application without using standard debugging functions like \code{browser()},
#' \code{debug()}, and breakpoints. \emph{Reactor} provides the ability to run both R and Markdown
#' code, save the code, and to generate reports in multiple formats.
#'
#' @section Functions:
#' \describe{
#'
#'  \item{reactorUI()}{The UI component, with variuos layout options}
#'
#'  \item{reactorModule()}{Load the reactor server module}
#'
#'  \item{reactorCore()}{A small Shiny app to demonstrate \emph{reactor}}
#'
#' }
#' @param namespace \emph{character string} Standard namespace convention for Shiny modules.This value
#' will be appended onto all UI ID's, so choose a namespace value that does not conflict with other
#' UI objects in your application.
#' @param layout \emph{character string} Whether the split between the UI (user interface) pane and the rendering pane should be
#' \code{vertical} or \code{horizontal}.  Default \code{vertical}.  Adjusting the split should
#' be useful in fitting \emph{reactor} to different UI layouts in Shiny applications.
#' @param directory \emph{character string} The directory where \emph{reactor} should save reports.  Default \code{NULL}
#' which will have \emph{reactor} generate a folder in the user's \emph{Documents} directory. If write permissions do not allow
#' for creation of a folder, \emph{reactor} will attempt to create a temporary folder. If that should fail, the user will be prompted
#' to manually create a folder and then relaunch the app. Value passed can be a reactive expression or a static value
#' @param envir \emph{For advanced users} The environment where R scripts and reports should be evaluated.
#' Defaults to the primary server environment of the Shiny session, providing access to all session objects.
#' Other common options would be the current calling environment, \code{environment()} or the global environment
#' \code{globalenv()}. However, for most uses access to the server domain, activated through
#' the default entry, will be best.
#'
#' @return A reactor module, either UI or server
#'
#' @import shiny
#' @import knitr
#' @import rmarkdown
#' @examples
#'
#' ## DO NOT RUN
#' library(shiny)
#' library(reactor)
#'
#' # Define UI for application that draws a histogram
#' ui <- navbarPage(title = 'Reactor Test',
#'           tabPanel('Old Faithful',
#'              # Application title
#'              # Sidebar with a slider input for number of bins
#'              sidebarLayout(
#'                 sidebarPanel(
#'                    sliderInput("bins",
#'                                "Number of bins:",
#'                                min = 1,
#'                                max = 50,
#'                                value = 30)
#'                 ),
#'                 # Show a plot of the generated distribution
#'                 mainPanel(
#'                    titlePanel("Old Faithful Geyser Data"),
#'                    plotOutput("distPlot")
#'                 )
#'              )
#'           ),
#'           tabPanel('Reactor', reactorUI('test'))
#' )
#'
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'
#'    data <- reactive({ faithful })
#'
#'    output$distPlot <- renderPlot({
#'       # generate bins based on input$bins from ui.R
#'       x    <- data()[, 2]
#'       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#'
#'       # draw the histogram with the specified number of bins
#'       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#'    })
#'
#'    # need to pass in some data to avoid error of
#'    # reading objects from shinyoutput object not allowed
#'    r <- callModule(reactor, 'test')
#' }
#'
#' # Run the application
#' shinyApp(ui = ui, server = server)
#'
#' @name reactor
NULL


# The UI module
#' @rdname reactor
#' @export
reactorUI <- function(namespace, layout = c('vertical', 'horizontal')) {
  library(shiny)
  tryCatch(
    namespace <- as.character(namespace[1]),
    error = function(e){
      stop(message(e))
    }
  )
  ns<-NS(namespace)

  layout <- match.arg(layout, c('vertical', 'horizontal'))

  # from user selection... extensible via the switch if more layouts added
  ui <- switch(layout,
                'vertical' = vert.tags(ns),
                'horizontal' = horz.tags(ns))

  return(ui)
}

# A wrapper function to allow the user to call the reactor without
# manually doing callModule(). This is important to passing in the environment
# where all the reactive values are located

# Load the server module
#' @rdname reactor
#' @export
reactorModule <- function(namespace = NULL, directory = NULL, envir = NULL) {
  if(is.null(envir) | !class(envir)=='environment') envir_ <- parent.frame()
  else envir_ <- envir

  tryCatch(
    namespace <- as.character(namespace[1]),
    error = function(e){
      stop(message(e))
    }
  )

  r<-do.call(callModule,
          args = list(module = reactor, id = namespace,
                      directory = directory,
                      environment = envir_),
          envir = parent.frame())
  return(r)
}


# the actual reactor module server code
reactor <- function(input, output, session,
                    directory = NULL,
                    environment = NULL) {

  rValues <- reactiveValues()
  rValues$rmd <- NULL
  rValues$out <- NULL

  # making the directory reactive, so it can be changed from the main shiny interface... say
  # through an actionButton call to choose.dir
  directory_ <- reactive({
    if(is.reactive(directory)) return(directory())
    return(directory)
  })

  # We make the environment variable reactive to allow for the scope to change
  # or re-evaluate via user input or some other method
  environment_ <- reactive({ environment })

  # processes the reactive directory_
  observe({
    if(is.null( directory_() )) {
      dir_ <- paste0(
        file.path(Sys.getenv("USERPROFILE"),"Documents"),
        "\\reactor.reports")
    } else dir_ <- paste0(directory_(), "\\reactor.reports")

    dir.create(dir_, showWarnings = FALSE)
    rValues$permissions <- TRUE

    # we'll check to see if we have permissions to create the dir
    # if not, we'll create a tmp dir and use that
    if(!dir.exists(dir_)) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      dir_ <- paste0(tempdir(), "\\reactor.reports")
      oldWd <- setwd( dir_ )

      dir.create(dir_, showWarnings = FALSE)

      # if we can't even create a temp dir, then we send a message asking the
      # user to manually create a directory and restart
      if(!dir.exists(dir_)) {
        showModal(modalDialog(
          title = 'PERMISSIONS ERROR!',
          'Having difficulty creating a directory. Please manually create
          Documents\reactor.reports under your user profile and then relaunch.',
          easyClose = TRUE,
          footer = modalButton("Ok")
        ))
        rValues$permissions <- FALSE
      }

      on.exit( setwd(oldWd) )
    }

    # add the directory to the resource path so the browser can access
    addResourcePath("reactor.reports", dir_)
    rValues$directory <- dir_
  })

  # for handling the import of files... automatically detects the extension
  observeEvent(input$file, {
    isolate({
      rValues$file <- input$file$datapath
      script_ <- paste0(readLines( rValues$file ), sep = '', collapse = '\n')

      updateTextAreaInput(session, 'script', value = script_)

      ext_ <- tools::file_ext( input$file )[1]
      name_ <- sub(paste0(".", ext_), "", input$file$name)
      updateTextInput(session, 'scriptName', value = name_)
      updateTextInput(session, 'reportname', value = paste0(name_, "_", Sys.Date()))

      substr(ext_, 1, 1)<-toupper(substr(ext_, 1, 1))
      updateSelectInput(session, 'filetype', selected = ext_)
      updateSelectInput(session, 'scriptExt', selected = paste0(".",ext_))

    })
  })

  observeEvent(input$run, {
    req(rValues$permissions)

    # establishing a temperory file to accept the script as .Rmd, which makes
    # it easy to render into html for inclusion.  Rendering this way helps to include
    # both static and interactive plots and objects
    tmp <- paste0(rValues$directory, "\\.last.reactor.report.Rmd")
    file.create(tmp)

    script_ <- unlist(strsplit( input$script, "\n" ))

    withProgress(message = "Rendering... please be patient", value = .2, {
      incProgress(.3, detail = 'Translating to markdown')
        rValues$rmd <- switch(input$filetype,
                          "R" = knitr::spin(text = script_,
                                     knit = FALSE),
                          "Rmd" = script_)
        writeLines(rValues$rmd, tmp)

      incProgress(.3, detail = 'Generating report')

        tryCatch(
          {
            width_ <- paste0("output_", session$ns("out"), "_width")
            rValues$out <- rmarkdown::render(input = tmp,
                                  output_format = html_document(),
                                  output_file = sub('.Rmd', '.html', tmp),
                                  output_options = list(
                                    out.width = "90%"
                                  ),
                                  envir = environment_(),
                                  runtime = 'shiny')
          },
          # instead of having the whole application crash on error, this will pop a modal dialog with
          # the error, and then halt execution
          error = function(e) {
              showModal(modalDialog(
                title = 'EVALUATION ERROR!',
                e,
                easyClose = TRUE,
                footer = modalButton("Ok")
              ))
              rValues$out <- NULL
            }
        )


      setProgress(.9)
      # remove the temporary file
      file.remove(tmp)
    })
  })

  output$out <- renderUI({
    req(input$run > 0, !is.null(rValues$out))
    tags$iframe(
          src = 'reactor.reports/.last.reactor.report.html',
          width = '100%',
          height = '100%',
          frameborder = 0,
          scrolling = 'auto')
  })


  # similar to the stuff above, but done in a way to save the script
  observeEvent(input$saveScript, {
      req(rValues$permissions)

      file <- paste0(input$scriptName, input$scriptExt)
      file <- paste0(rValues$directory, "\\", file)
      file.create(file)

      if(input$scriptExt == '.R') {
        out <- switch(input$filetype,
                         R = input$script,
                         Rmd = {
                           rmdToScript(unlist(strsplit( input$script, "\n") ))
                         }
                        )
      } else if(input$scriptExt == '.Rmd') {
        out <- switch(input$filetype,
                         R = knitr::spin(knit = FALSE,
                                text = unlist(strsplit( input$script, split = "\n") )),
                         Rmd = input$script
                        )
      } else out <- input$script
      writeLines(out, file)

      showModal(modalDialog(
        title = 'Saved!',
        file,
        easyClose = TRUE,
        footer = modalButton("Ok")
      ))
  })

  # similar to the stuff above, but done in a way to save the report
  observeEvent(input$downloadReport, {
      req(rValues$permissions)

      # set up a temporary file to accept the rendered script
      tmp <- paste0(rValues$directory, "\\.last.reactor.report.Rmd")
      file.create(tmp)
      file <- paste(input$reportname, sep = '.',
                    switch(input$format,
                           PDF = 'pdf',
                           HTML = 'html',
                           Word = 'docx'))

      file <- paste0(rValues$directory, "\\", file)

      # when we hit the download button, we'll see if the user has already hit the "run" button,
      # which populates the rValues$rmd var. downloadReport will save the rendered report if there,
      # but if no report has been rendered, then it will do the equivalent of the "run" routine here
      rmd <- rValues$rmd

      withProgress(message = 'Rendering... please be patient', value = .1, {
        incProgress(.3, detail = 'Translating to markdown')

          # if no script yet ran, then do that here
          if(is.null(rmd)) {
            rmd <- switch(input$filetype,
                            "R" = spin(text = unlist(
                                          strsplit( input$script, split = "\n")),
                                     knit = FALSE),
                            "Rmd" = input$script)
          }
          writeLines(rmd, tmp)

      incProgress(.3, detail = 'Generating report')
        tryCatch(
            {
              library(knitr)
              library(rmarkdown)
              rmarkdown::render(input = tmp,
                     output_format = switch(
                          input$format,
                          PDF = pdf_document(),
                          HTML = html_document(),
                          Word = word_document()
                       ),
                     output_file = file,
                     envir = environment_()
                    )
            },
            # instead of having the whole application crash on error, this will pop a modal dialog with
            # the error, and then halt execution
            error = function(e) {
                rValues$success <- FALSE
                showModal(modalDialog(
                  title = 'EVALUATION ERROR!',
                  e,
                  easyClose = TRUE,
                  footer = modalButton("Ok")
                ))
              }
        )
        setProgress(.9)
        # get rid of the temporary file
        file.remove(tmp)

        showModal(modalDialog(
          title = 'Saved!',
          file,
          easyClose = TRUE,
          footer = modalButton("Ok")
        ))
      })
  })

  return (rValues)
}

# A small app to demonstrate reactor
#' @rdname reactor
#' @export
reactorCore <- function() {
  library(shiny)
  runApp(system.file('R/app.R', package = 'reactor'))
}


