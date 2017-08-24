  # splits the IDE vs rendering tabs vertically, with the IDE on the left
vert.tags <- function(ns){
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(width = 6,
            fluidRow(
              column(10, fileInput(ns('file'), 'Load file:',
                                         multiple = F, accept = c('.R', '.Rmd'))),
              column(2, selectInput(ns('filetype'),
                                  'File Type:', choices = c('R', 'Rmd')))
            ),
            textAreaInput(ns('script'), label = 'Script:',
                          height = '500px', resize = 'vertical'),
            fluidRow(column(3,
                actionButton(ns('run'), 'Run', width = '150px'))),
            hr(),
            fluidRow(
              column(10, textInput(ns('scriptName'), 'Script name:'),
                                    value = paste0('script_', Sys.Date())),
              column(2, selectInput(ns('scriptExt'), 'Extension:', choices = c('.R', '.Rmd')))
            ),
            fluidRow(column(6, actionButton(ns('saveScript'),
                                label = 'Save Script',
                                icon = icon('download'),
                                width = '150px')
            )),
            hr(),
            fluidRow(
              column(7, textInput(ns('reportname'), label = "Report name:",
                                  value = paste0('report_', Sys.Date()))),
              column(5, radioButtons(ns('format'), 'Report format:',
                      c('PDF', 'HTML', 'Word'), inline = TRUE))
            ),
            fluidRow(column(4, actionButton(ns('downloadReport'),
                                  label = 'Download Report',
                                  icon = icon('download'),
                                  width = '150px')
            ))
        ),
        mainPanel(
         htmlOutput(ns('out'))
        )
      )
    )
  )
}

  # splits the IDE vs rendering tabs horizontally, with the rendered report below the interface
horz.tags <- function(ns){
  tagList(
    fluidPage(
      fluidRow(
        column(12,
          wellPanel(
            fluidRow(
              column(6, fileInput(ns('file'), 'Load file:',
                                         multiple = F, accept = c('.R', '.Rmd'))),
              column(2, selectInput(ns('filetype'),
                                  'File Type:', choices = c('R', 'Rmd')))
            ),
            textAreaInput(ns('script'), label = 'Script:',
                          height = '500px', resize = 'vertical'),
            fluidRow(
              column(3, actionButton(ns('run'), 'Run', width = '150px'))
            ),
            hr(),
            fluidRow(
              column(6, textInput(ns('scriptName'), 'Script name:'),
                                  value = paste0('script_', Sys.Date())),
              column(6, textInput(ns('reportname'), label = "Report name:",
                                  value = paste0('report_', Sys.Date())))
            ),
            fluidRow(
              column(2, selectInput(ns('scriptExt'), 'Extension:',
                                    choices = c('.R', '.Rmd'))),
              column(6, offset = 4, radioButtons(ns('format'), 'Report format:',
                      c('PDF', 'HTML', 'Word'), inline = TRUE))
            ),
            fluidRow(
              column(2, actionButton(ns('saveScript'),
                                  label = 'Save Script',
                                  icon = icon('download'),
                                  width = '150px')),
              column(2, offset = 4, actionButton(ns('downloadReport'),
                                  label = 'Download Report',
                                  icon = icon('download'),
                                  width = '150px'))
            )
          )
        )
      ),
      fluidRow(
        column(12, htmlOutput(ns('out')))
      )
    )
  )
}
