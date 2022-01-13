#' Pie Plot
#' @description The function "grafica_pie" create a Pie Plot
#' @param df 	Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify(). If not specified, must be supplied in each layer added to the plot.
#' @param y_axis variable to be plotted
#' @param fill_c color fill
#' @param show_legend shows the plotted data
#' @param etiquetas_c color of the data
#' @param i number of partitions of the pie
#' @param legend_title Legend title to graph, as character
#' @param titulo Title to graph, as character
#' @param subtitulo Subtitle to graph, as character
#' @return BoxPlot with Banxico format
#' @references Ggplot2: Elegant Graphics for Data Analysis
#' @author Marcos Daniel Ramirez Santiago, \email{danielrs98@@outlook.com}
#' @export


grafica_pie <- function(df,y_axis,fill_c,show_legend=T,etiquetas_c,i,legend_title="",
                        titulo,subtitulo){

  g<-ggplot(df,aes(x = "", y = {{y_axis}}, fill = {{fill_c}}))
  g+geom_col(show.legend = if(show_legend == T){show.legend=T} else {show.legend=show_legend}) +
    scale_fill_manual(values = colores) +
    geom_text(aes(label = {{etiquetas_c}}), color = rep(c("#FDFEFE"),each=i),
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") +
    guides(fill = guide_legend(title = if(legend_title == T){title=""} else {title=legend_title}))+
    labs(title = "",
         subtitle = "")+
    theme_void()

}

