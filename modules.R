percentage_barUI = function(id){
  ns = NS(id)
  tagList(
    plotlyOutput(ns("percentage_bar"))
  )
}
  
percentage_bar = function(input,output,session,vec,orientation='h',
                              trace_name ="Percentage",margin = list(l=80,r=80,b=80,t=80,pad=0),
                              title = NULL,ylab=NULL,xlab=NULL,range = "auto", color = "rgb(22, 33, 150)"){
  output$percentage_bar = renderPlotly({
    d = vec()/sum(vec())
    if(range[1]=="auto"){
      range = c(0,max(d))
    }
    if(orientation == 'h'){
      graph = plot_ly(x=d, y=names(d) , type= 'bar',  orientation = 'h', name = trace_name, marker = list(color = color)) %>%
        layout(title = title, margin = margin,
               xaxis = list(title = xlab,tickformat = "%", range = range),
               yaxis = list(side = 'left', title = ylab))
    }else if(orientation == 'v'){
      graph = plot_ly(x=names(d), y=d , type= 'bar', name = trace_name, marker = list(color = color)) %>%
        layout(title = title, margin = margin,
               xaxis = list(title = xlab),
               yaxis = list(side = 'left', title = ylab,tickformat = "%", range = range))
    }else{
      stop("orientation must be 'h' or 'v' see documentation  https://plot.ly/r/reference/")
    }
    graph$elementId = NULL
    graph
  })
}

percentage_barTest = function(d,orientation='h',
                          trace_name ="Percentage",margin = list(l=80,r=80,b=80,t=80,pad=0),
                          title = NULL,ylab=NULL,xlab=NULL,range = "auto", color = "rgb(22, 33, 150)"){
  d = d/sum(d)
  if(range[1]=="auto"){
    range = c(0,max(d))
  }
  if(orientation == 'h'){
    graph = plot_ly(x=d, y=names(d) , type= 'bar',  orientation = 'h', name = trace_name, marker = list(color = color)) %>%
      layout(title = title, margin = margin,
             xaxis = list(title = xlab,tickformat = "%", range = range),
             yaxis = list(side = 'left', title = ylab))
  }else if(orientation == 'v'){
    graph = plot_ly(x=names(d), y=d , type= 'bar', name = trace_name, marker = list(color = color)) %>%
      layout(title = title, margin = margin,
             xaxis = list(title = xlab),
             yaxis = list(side = 'left', title = ylab,tickformat = "%", range = range))
  }else{
    stop("orientation must be 'h' or 'v' see documentation  https://plot.ly/r/reference/")
  }
  graph$elementId = NULL
  graph
}

