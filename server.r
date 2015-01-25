library(xtable)
library(shiny)

pr = read.csv("presidents2.txt", sep="\t", stringsAsFactors = F)
# pr <- read.csv("http://hauu.in/static/presidents.txt", sep="\t", stringsAsFactors = F)
# preparing data set


for (i in 1:dim(pr)[1])
{
  begin_date = substring(pr[i,1],0,4)
  end_date = substring(pr[i,1],6,10)
  pr[i,5] <- begin_date
  pr[i,6] <- end_date
  
  if (pr[i,6] == "") pr[i,6] = pr[i,5]
  if (pr[i,5] == 2009) pr[i,6] = 2015
}

pr$V5 = as.numeric(pr$V5)
pr$V6 = as.numeric(pr$V6)
names(pr) = c("YEAR","PRESIDENT","FIRST LADY","VICE PRESIDENT","BEGIN","END")


shinyServer(function(input, output)
{
# debug values. commented for production

#output$oyear = renderPrint(head(showw))
#output$dddd = renderPrint(keepCurrent())
#output$dddd2 = renderPrint(plot_data())
#output$oyear2 = renderPrint(colors())


# choosing which search to do, render appropriate UI

output$nameUI <- renderUI({
    if (input$type == TRUE) 
    {
      textInput('name', 'Please enter the name:', value="Barack Obama")
    }
      })

output$dateUI <- renderUI({
  if (input$type == FALSE) 
    {
       numericInput('term', 'Please enter the year:', value=2015, max=2015, min=1789)
	  }
})  

output$faux = renderTable({outputter()})

outputter = reactive({
# true means name search, false means date search
if(input$type == TRUE)
{
	if (dim(pr[named(),][columns()])[1] > 0)
	{
	pr[named(),][columns()]
	} else return(invalid_name())

} 
else if (input$type == FALSE)
	{
	if (dim(pr[elected(),][columns()])[1] > 0)
	{
	pr[elected(),][columns()]
	} else return(invalid_date())
	}

})

# grep for name, output: logical vector of lines
named = reactive({
	
	found = grepl(input$name, pr[1:64,2])
	return(found)
  
})

# search for date, output: logical vector of lines
elected = reactive({
  
	bound = input$term
	found2 = (pr$BEGIN <= bound & pr$END >= bound)
	return(found2)

})

# method to check for additional columns (first lady & vice president)
columns = reactive({
	if (!is.null(input$columns))
	{
	columns = as.numeric(c(1,2,input$columns))
	}
	else 
	{
	columns = c(1,2)
	}
	
	return(columns)

})

# error message for invalid date
invalid_date = reactive({
  
  a = as.data.frame("Year range 1789-2015 accepted");
  colnames(a) = "Invalid year"
  rownames(a) = "ERROR"
  return(a)
  
})

# error message for invalid name
invalid_name = reactive({
  
  a = as.data.frame("There is no such president");
  colnames(a) = "Invalid name"
  rownames(a) = "NOT FOUND"
  return(a)
  
})

# preparing unique data for barplot
# plot data should be used with [,2] if for drawing barplot

plot_data = reactive({
#subterms = data.frame(matrix(nrow = 64, ncol = 2))
subterms = data.frame(character(),numeric(), stringsAsFactors=F)
names(subterms) = c("NAME","YEARS")
pr2 = pr
for (i in 1:dim(pr)[1])
{
pr2[i,7] = pr2[i,6] - pr2[i,5]
}
names(pr2) = c("YEAR","PRESIDENT","FIRST LADY","VICE PRESIDENT","BEGIN","END","SUBTERM")

for (i in 1:dim(pr)[1])

	{
		termsum = sum(pr2[pr2$PRESIDENT == pr[i,2],"SUBTERM"])
		row = cbind(pr2[i,2],termsum)
		subterms <- rbind(subterms, row)
	}

ust = unique(subterms)
ust$termsum = as.numeric(ust$termsum)
return (ust)
})

# keep the current president, whether by name or date
keepCurrent = reactive({
if(input$type == TRUE)
{
current = pr[named(),][columns()][[2]]
} else if (input$type == FALSE)
{
current = pr[elected(),][columns()][[2]]
}
return(current)
})

# generate a logical vector of presidents (un)selected
logiCurrent = reactive({
logical_current = (plot_data()$NAME == keepCurrent())
return(logical_current)
})

# generate a color vector for barplot
colors = reactive({
color_list = "grey"
uniq = plot_data()

for (i in 1:43)
{
	if (uniq[i,1] %in% keepCurrent())
	{
	color_list[i] = 'red'
	} else color_list[i] = 'grey'
}

return (color_list)

})

# draw plot
output$plot1 = renderPlot({
par(mar=c(12,4,2,2))
barplot(plot_data()[,2], names.arg = plot_data()$V1, las = 2, col = colors(), ylab="Term length", main="Selected president position on timeline") 

})
}
)