#' BoxPlot
#' @description The function "grafica_BoxPlot" create a BoxPlot for multiple series in a dataset
#' @param df 	Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify(). If not specified, must be supplied in each layer added to the plot.
#' @param x_axis data of variable x
#' @param y_axis variable to be plotted
#' @param group_c color fill
#' @param color_bp color of the BoxPlot from colores Banxico
#' @param color_outlier outlier
#' @param legend_position Position adjustment of the legend
#' @param legend_title Legend title to graph, as character
#' @param x_t x axis title to graph, as character
#' @param y_t y axis title to graph, as character
#' @param y_position Position of y axis, ie. ("right","left)
#' @param formato_fecha date format
#' @param titulo Title to graph, as character
#' @param subtitulo Subtitle to graph, as character
#' @param break_x x axis partitions
#' @param f_wrap if f_wrap=T, then facet_wrap() function will be activate
#' @param f_wrap_1 variables to graph
#' @param scales_fw scales of the variables graphed
#' @param t_hjust Title position, values between [0,1]
#' @param s_hjust Subtitle position, values between [0,1]
#' @param bigotes T
#' @return BoxPlot with Banxico format
#' @references Ggplot2: Elegant Graphics for Data Analysis
#' @author Marcos Daniel Ramirez Santiago, \email{danielrs98@@outlook.com}
#' @export

grafica_BoxPlot <- function(df,x_axis,y_axis,group_c,color_bp=colores[2],
                            color_outlier="#FDFEFE",legend_position="right",
                            legend_title="",x_t="",y_t="",y_position="right",
                            formato_fecha="%m-%Y",titulo="",subtitulo="",
                            break_x="8 week",t_hjust=0.0,s_hjust=0.0,
                            f_wrap=F,f_wrap_1,scales_fw=T,bigotes=F){

  g<-ggplot(df,aes(x = {{x_axis}}, y = {{y_axis}}, group = {{group_c}}))
  g+geom_boxplot(fill=color_bp, outlier.colour =color_outlier) +
    {if(bigotes)stat_boxplot(geom = "errorbar")}+
    theme_bw()+
    theme(legend.position = legend_position, #bottom, none
          legend.text = element_text(size = 12,vjust = 0.5),
          plot.title = element_text(color="black", size=12,hjust = t_hjust,face="bold"),
          plot.subtitle = element_text(color="black", size=12,hjust = s_hjust),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(color = "#A6A6A6",
                                            size = 0.5,
                                            linetype = 2),
          axis.line = element_line(colour = "#182B47",
                                   size = 0.5, linetype = "solid"),
          strip.background = element_blank()) +
    scale_y_continuous(position = y_position,expand=c(0,0))+
    scale_x_date(date_labels = formato_fecha,
                 breaks = date_breaks(break_x),expand=c(0,0))+
    labs(title = titulo,
         subtitle = subtitulo)+
    xlab(x_t)+
    ylab(y_t)+
    {if(f_wrap)facet_wrap(vars({{f_wrap_1}}),
                          scales = if(scales_fw == T){position="free"}else{scales=scales_fw})}

}

