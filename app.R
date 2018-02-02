library(shiny)
library(XML)
library(magrittr)
library(purrr)
library(stringr)

ui <- fluidPage(
   titlePanel("HTML to R Converter"),
   fluidRow(
      column(5, textAreaInput("html", "HTML", rows=20, value = ' <table style="width:100%">
  <tr>
    <th>Firstname</th>
    <th>Lastname</th>
    <th>Age</th>
  </tr>
  <tr>
    <td>Jill</td>
    <td>Smith</td>
    <td>50</td>
  </tr>
  <tr>
    <td>Eve</td>
    <td>Jackson</td>
    <td>94</td>
  </tr>
</table>')
      ),
      column(2, checkboxInput("prefix", "Prefix"), actionButton("convert", "Convert")),
      column(5, tags$pre(textOutput("rCode")))
      ),
   fluidRow(tags$a(href = "https://github.com/alandipert/html2r", "Github"))
      )

makeAttrs <- function(node) {
   attrs <- xmlAttrs(node)
   names(attrs) %>%
      Map(function (name) {
         val <- attrs[[name]]
         paste0(name, ' = ', if (val == "") "NA" else paste0('"', val, '"'))
      }, .)
}

Keep <- function(fun, xs) Map(fun, xs) %>% Filter(Negate(is.null), .)

renderNode <- function(node, indent = 0, prefix = FALSE) {
   if (xmlName(node) == "text") {
      txt <- xmlValue(node)
      if (nchar(trimws(txt)) > 0) {
         paste0('"', trimws(txt), '"')
      }
   } else {
      tagName <- if (prefix) paste0("tags$", xmlName(node)) else xmlName(node)
      newIndent <- indent + length(tagName) + 1
      xmlChildren(node) %>%
         Keep(partial(renderNode, indent = newIndent, prefix = prefix), .) %>%
         append(makeAttrs(node), .) %>%
         paste(collapse = str_pad(",\n", width = newIndent, side = c("right"))) %>%
         trimws(which = c("left")) %>%
         paste0(tagName, "(", ., ")")
   }
}

html2R <- function(htmlStr, prefix = FALSE) {
   htmlStr %>%
      htmlParse %>%
      getNodeSet("/html/body/*") %>%
      `[[`(1) %>%
      renderNode(prefix = prefix)
}

server <- function(input, output, session) {
   
   rcode <- eventReactive(input$convert, {
      html2R(input$html, prefix = input$prefix)
   }, ignoreInit = TRUE)
   
   output$rCode <- renderText(rcode())
}

shinyApp(ui, server)