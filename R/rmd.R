
# code to "manually" render .Rmd lines from within Shiny
renderRmdLines<-function(lines, envir = parent.frame()){
  l <- knitr::knit2html(text = lines, fragment.only = TRUE, options = '',
      stylesheet = '')
  l <- gsub("&lt;!--/html_preserve--&gt;", "", l)
  l <- gsub("&lt;!--html_preserve--&gt;", "", l)
  l <- HTML(l)
  return(l)
}

# same as above, but reads the file in path arg
renderRmd<-function(path, envir = parent.frame()){
   l <- paste(readLines(path, warn = FALSE), collapse = '\n')
   l <- renderRmdLines(l, envir = envir)
   HTML(l)
}

# coverts an .Rmd file to basic R script, with Roxygen-style comments
# for Markdown portions.  This is the opposite of knitr::spin and works quite
# handily with it
rmdToScript <- function(lines){
  # identify the start of code blocks
  cdStrt <- which(grepl(lines, pattern = "```{r*", perl = TRUE))
  # identify the end of code blocks
  cdEnd <- sapply(cdStrt, function(x){
    preidx <- which(grepl(lines[-(1:x)], pattern = "```", perl = TRUE))[1]
    return(preidx + x)
  })
  # define an expansion function
  # strip code block indacators
  lines[c(cdStrt, cdEnd)] <- ""
  expFun <- function(strt, End){
    strt <- strt+1
    End <- End-1
    return(strt:End)
  }
  idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd,
                       SIMPLIFY = FALSE))
  # add comments to all lines except code blocks
  comIdx <- 1:length(lines)
  comIdx <- comIdx[-idx]
  for(i in comIdx){
    lines[i] <- paste("#' ", lines[i], sep = "")
  }
  return(lines)
}
