shinyUI(pageWithSidebar(
  headerPanel('Developing data products - Course app'),
  
  sidebarPanel(
	p('This simple app can help you find who was the US President at given year or when a President with a given name was in power'),
  radioButtons("type", "I want to search...", c("By name" = TRUE, "By date" = FALSE)),
	htmlOutput("nameUI"),
	htmlOutput("dateUI"),
	checkboxGroupInput('columns', 'Also show:',
	choices = c("First lady" = 3, "Vice President" = 4)),
	tags$a(href="http://www.loc.gov/rr/print/list/057_chron.html","The data is the courtesy of the Library of Congress")
	),

  mainPanel(
    h3('The President is:'),
	tableOutput('faux'),
	#verbatimTextOutput('oyear'),
	#verbatimTextOutput('dddd'),
	#verbatimTextOutput('dddd2'),
	#verbatimTextOutput('oyear2'),
	plotOutput('plot1')
	)

	))

      