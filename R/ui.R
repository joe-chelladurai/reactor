# modifications to Shiny UI elements, to allow us to hard-set the height

reactorMainPanel <- function (..., width = 8, height = NULL)
{
  div(class = paste0("col-sm-", width), style = if (!is.null(height))
    paste0("height: ", shiny::validateCssUnit(height), ";"),
      ...)
}

# mod to htmlOutput
reactorOutput <- function (outputId, height = NULL, inline = FALSE,
                           container = if (inline) span else div,
          ...)
{
  if (anyUnnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }

  height_ <- paste0("height: ",
                    ifelse(!is.null(height), shiny::validateCssUnit(height), "100%"),
                    ";")

  container(id = outputId, class = "shiny-html-output",
            style = height_, ...)
}
