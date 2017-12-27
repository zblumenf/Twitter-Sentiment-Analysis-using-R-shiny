percentage_barUI = function(id,width = "800px",height = "600px"){
  ns = NS(id)
  tagList(
    div(
      #style = "display:inline-block",
      plotlyOutput(ns("percentage_bar"),height = height,width = width)
    )
  )
}
  
percentage_bar = function(input,output,session,vec,orientation='h',
                              trace_name ="Percentage",margin = list(l=100,r=80,b=80,t=80,pad=0),
                              title = NULL,ylab=NULL,xlab=NULL,range = "auto", color = "rgb(55, 90, 127)",
                              paper_bgcolor='transparent',plot_bgcolor='transparent', 
                          font_color = 'rgb(255,255,255)',font_size = 18,font_family = "Lato"){
  output$percentage_bar = renderPlotly({
    d = vec()/sum(vec())
    d_names = factor(1:length(d),labels = names(d))
 #   cat(file=stderr(), "finding what d_names is:  ",str(d), "\n")
    if(range[1]=="auto"){
      range = c(0,max(d))
    }
    if(orientation == 'h'){
      graph = plot_ly(x=d, y=d_names , type= 'bar',  orientation = 'h', name = trace_name, marker = list(color = color)) %>%
        layout(title = title, margin = margin,
               xaxis = list(title = xlab,tickformat = "%", range = range),
               yaxis = list(side = 'left', title = ylab),
               font = list(color = font_color, family = font_family,size=font_size),
                paper_bgcolor=paper_bgcolor,plot_bgcolor=plot_bgcolor)
    }else if(orientation == 'v'){
      graph = plot_ly(x=d_names, y=d , type= 'bar', name = trace_name, marker = list(color = color)) %>%
        layout(title = title, margin = margin,
               xaxis = list(title = xlab),
               yaxis = list(side = 'left', title = ylab,tickformat = "%", range = range),
               font = list(color = font_color, family = font_family,size=font_size),
               paper_bgcolor=paper_bgcolor,plot_bgcolor=plot_bgcolor)
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

