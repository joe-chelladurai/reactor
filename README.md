# reactor
A lite-GUI for R, coded in Shiny as an add-on to a Shiny application, allowing for
scripting and reporting while in a reactive session.

- Code and run R scripts that leverage reactive expressions and inputs defined
in your Shiny application
- Save your script as either *.Rmd* or *.R* 
- Generate and show reports in the *reactor* UI, or save them in various formats
- Debug your application with a lower risk of crashing it

### Installation

Grab *reactor* from GitHub:

```S
devtools::install_github("klaukh/reactor")
```

### Usage

Add the UI module to your Shiny application, selecting one of the provided layouts
and providing a namespace that works with your application.  Add the UI module
as a separate tab, or as part of a tab or page.

```S
reactorUI('faithful', layout = 'vertical') 
```

Add the server module, then run your Shiny application. 
```S
reactorModule('faithful')
```
  

### Try *reactor*
```S
reactor_core()
```

