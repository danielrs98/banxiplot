colores <- c("#182B47", "#BF0A33", "#00727C", "#FF7800", "#646567",
             "#31B133", "#FFCB00", "#6D3A77", "#8C734A","#A6A6A6","#182B47",
             "#652F6C")
#Azul, Rojo, Verde, Naranja, Gris, Verde2, Amarillo, Morado, Oro, Gris_lineas
#,negro,colorREF_morado

#' Area Plot
#' @description The function "grafica_area" create an area plot for multiple series in a dataset
#' @param df 	Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify(). If not specified, must be supplied in each layer added to the plot.
#' @param x_axis data of variable x
#' @param y_axis variable to be plotted
#' @param fill_C color fill
#' @param is_date if x-axis format are dates
#' @param is_num if x-axis format is numeric
#' @param is_word if x-axis format are words
#' @param legend_position Position adjustment of the legend
#' @param legend_title Legend title to graph, as character
#' @param x_t x axis title to graph, as character
#' @param y_t y axis title to graph, as character
#' @param y_position Position of y axis, ie. ("right","left)
#' @param formato_fecha date format
#' @param titulo Title to graph, as character
#' @param subtitulo Subtitle to graph, as character
#' @param break_x x axis partitions
#' @param line_v if line_v=T, then add a vertical line
#' @param x_int Date where the vertical line will be placed
#' @param f_wrap if f_wrap=T, then facet_wrap() function will be activate
#' @param f_wrap_1 variables to graph
#' @param scales_fw scales of the variables graphed
#' @param t_hjust Title position, values between [0,1]
#' @param s_hjust Subtitle position, values between [0,1]
#' @param add_line If add_line = T then, add a line
#' @param ext_line variable to be plotted
#' @param ext_line_col select color of extra line
#' @param vline_col color of vertical line
#' @param size_t title font size
#' @param size_s subtitle font size
#' @param size_x_t x-axis title font size
#' @param size_y_t y-axis title font size
#' @param size_l legend title font size
#' @param size_lt legend text font size
#' @param size_x_axis x-axis text font size
#' @param size_y_axis x-axis text font size
#' @param font_t title font style
#' @param font_s subtitle font style
#' @param font_l legend font style
#' @param font_x x-axis font style
#' @param font_y y-axis font style
#' @param font_lt legend text font style
#' @param col_t title color
#' @param col_s subtitle color
#' @param angle_x angle of x-axis text
#' @param angle_y angle of y-axis text
#' @param x_axis_tf title bold,plain, etc
#' @param y_axis_tf subtitle bold,plain, etc
#' @param x_axis_f bold,plain, etc
#' @param y_axis_f bold,plain, etc
#' @param area_alpha area plot opacity
#' @param vline_alpha vline opacity
#' @param ext_line_alpha extra line opacity
#' @param escala adjust y-axis scale
#' @param ay y-axis scale lower limit
#' @param by y-axis scale upper limit
#' @return An area plot with Banxico format
#' @references Ggplot2: Elegant Graphics for Data Analysis
#' @author Marcos Daniel Ramirez Santiago, \email{danielrs98@@outlook.com}
#' @export

grafica_area <- function(df,x_axis,y_axis,fill_C,is_date=F,is_num=F,is_word=F,
                         legend_position="right",
                         legend_title="",x_t="",y_t="", y_position="right",
                         formato_fecha="%m-%Y",titulo="",subtitulo="",
                         break_x="8 week",line_v=F,x_int,f_wrap=F,f_wrap_1,scales_fw=T,
                         t_hjust=0.5,s_hjust=0.5,add_line=F,ext_line,ext_line_col="black",
                         vline_col="black",size_t=9,size_s=9,
                         size_x_t=7,size_y_t=7,size_l=7,size_lt=7,
                         size_x_axis=9,size_y_axis=9,
                         font_t="Calibri",font_s="Calibri",font_l="Calibri",
                         font_x="Calibri",font_y="Calibri",font_lt="Calibri",
                         col_t=colores[12],col_s=colores[12],
                         angle_x=0,angle_y=0,x_axis_tf="plain",y_axis_tf="plain",
                         x_axis_f="plain",y_axis_f="plain",area_alpha=1,
                         vline_alpha=1,ext_line_alpha=1,escala=F,ay,by){

  g<-ggplot(df,aes(x = {{x_axis}}, y = {{y_axis}}, fill = {{fill_C}}))
  g+geom_area(alpha = area_alpha) +
    {if(line_v)geom_vline(xintercept=x_int,linetype = "longdash",
                          colour=vline_col,alpha = vline_alpha)}+
    {if(add_line)geom_line(aes(y = {{ext_line}}), color = ext_line_col,
                           size = 1,alpha = ext_line_alpha)}+
    scale_fill_manual(values = colores,name=legend_title) +
    theme_bw()+
    theme(legend.position = legend_position, #bottom, none
          legend.title=element_text(size=size_lt,family=font_lt),
          legend.text = element_text(size = size_l,vjust = 0.5,family=font_l),
          plot.title = element_text(color=col_t, size=size_t,
                                    hjust = t_hjust,family=font_t,face="bold"),
          plot.subtitle = element_text(color=col_t, size=size_s,
                                       hjust = s_hjust,family=font_s),
          axis.title.x = element_text(color="black", size=size_x_t,
                                      family=font_x,face=x_axis_tf),
          axis.text.x = element_text(size = size_x_axis,colour=colores[11],
                                     angle = angle_x,family="Calibri",face=x_axis_f),
          axis.title.y = element_text(color="black", size=size_y_t,
                                      family=font_y,face=y_axis_tf),
          axis.text.y = element_text(size = size_x_axis,colour=colores[11],
                                     angle = angle_x,family="Calibri",face=y_axis_f),
          panel.grid.major = element_blank(),#quita la cuadricula para tener fondo blanco
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(color = "#A6A6A6",
                                            size = 0.5,
                                            linetype = 2),
          axis.line = element_line(colour = "#182B47",
                                   size = 0.4, linetype = "solid"),
          strip.background = element_blank()) +
    scale_y_continuous(position = y_position,expand=c(0,0))+
    {if(is_date)scale_x_date(date_labels = formato_fecha,
                             breaks = date_breaks(break_x),expand=c(0,0))}+
    {if(is_num)scale_x_continuous(expand=c(0,0))}+
    {if(is_word)scale_x_discrete(expand=c(0,0))}+
    labs(title = titulo,
         subtitle = subtitulo)+
    xlab(x_t)+
    ylab(y_t)+
    {if(escala)coord_cartesian(ylim = c(ay,by))}+
    {if(f_wrap)facet_wrap(vars({{f_wrap_1}}),
                          scales = if(scales_fw == T){position="free"}else{scales=scales_fw})}


}

