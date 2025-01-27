% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grafica_area.R
\name{grafica_area}
\alias{grafica_area}
\title{Area Plot}
\usage{
grafica_area(
  df,
  x_axis,
  y_axis,
  fill_C,
  is_date = F,
  is_num = F,
  is_word = F,
  legend_position = "right",
  legend_title = "",
  x_t = "",
  y_t = "",
  y_position = "right",
  formato_fecha = "\%m-\%Y",
  titulo = "",
  subtitulo = "",
  break_x = "8 week",
  line_v = F,
  x_int,
  f_wrap = F,
  f_wrap_1,
  scales_fw = T,
  t_hjust = 0.5,
  s_hjust = 0.5,
  add_line = F,
  ext_line,
  ext_line_col = "black",
  vline_col = "black",
  size_t = 9,
  size_s = 9,
  size_x_t = 7,
  size_y_t = 7,
  size_l = 7,
  size_lt = 7,
  size_x_axis = 9,
  size_y_axis = 9,
  font_t = "Calibri",
  font_s = "Calibri",
  font_l = "Calibri",
  font_x = "Calibri",
  font_y = "Calibri",
  font_lt = "Calibri",
  col_t = colores[12],
  col_s = colores[12],
  angle_x = 0,
  angle_y = 0,
  x_axis_tf = "plain",
  y_axis_tf = "plain",
  x_axis_f = "plain",
  y_axis_f = "plain",
  area_alpha = 1,
  vline_alpha = 1,
  ext_line_alpha = 1,
  escala = F,
  ay,
  by
)
}
\arguments{
\item{df}{Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify(). If not specified, must be supplied in each layer added to the plot.}

\item{x_axis}{data of variable x}

\item{y_axis}{variable to be plotted}

\item{fill_C}{color fill}

\item{is_date}{if x-axis format are dates}

\item{is_num}{if x-axis format is numeric}

\item{is_word}{if x-axis format are words}

\item{legend_position}{Position adjustment of the legend}

\item{legend_title}{Legend title to graph, as character}

\item{x_t}{x axis title to graph, as character}

\item{y_t}{y axis title to graph, as character}

\item{y_position}{Position of y axis, ie. ("right","left)}

\item{formato_fecha}{date format}

\item{titulo}{Title to graph, as character}

\item{subtitulo}{Subtitle to graph, as character}

\item{break_x}{x axis partitions}

\item{line_v}{if line_v=T, then add a vertical line}

\item{x_int}{Date where the vertical line will be placed}

\item{f_wrap}{if f_wrap=T, then facet_wrap() function will be activate}

\item{f_wrap_1}{variables to graph}

\item{scales_fw}{scales of the variables graphed}

\item{t_hjust}{Title position, values between [0,1]}

\item{s_hjust}{Subtitle position, values between [0,1]}

\item{add_line}{If add_line = T then, add a line}

\item{ext_line}{variable to be plotted}

\item{ext_line_col}{select color of extra line}

\item{vline_col}{color of vertical line}

\item{size_t}{title font size}

\item{size_s}{subtitle font size}

\item{size_x_t}{x-axis title font size}

\item{size_y_t}{y-axis title font size}

\item{size_l}{legend title font size}

\item{size_lt}{legend text font size}

\item{size_x_axis}{x-axis text font size}

\item{size_y_axis}{x-axis text font size}

\item{font_t}{title font style}

\item{font_s}{subtitle font style}

\item{font_l}{legend font style}

\item{font_x}{x-axis font style}

\item{font_y}{y-axis font style}

\item{font_lt}{legend text font style}

\item{col_t}{title color}

\item{col_s}{subtitle color}

\item{angle_x}{angle of x-axis text}

\item{angle_y}{angle of y-axis text}

\item{x_axis_tf}{title bold,plain, etc}

\item{y_axis_tf}{subtitle bold,plain, etc}

\item{x_axis_f}{bold,plain, etc}

\item{y_axis_f}{bold,plain, etc}

\item{area_alpha}{area plot opacity}

\item{vline_alpha}{vline opacity}

\item{ext_line_alpha}{extra line opacity}

\item{escala}{adjust y-axis scale}

\item{ay}{y-axis scale lower limit}

\item{by}{y-axis scale upper limit}
}
\value{
An area plot with Banxico format
}
\description{
The function "grafica_area" create an area plot for multiple series in a dataset
}
\references{
Ggplot2: Elegant Graphics for Data Analysis
}
\author{
Marcos Daniel Ramirez Santiago, \email{danielrs98@outlook.com}
}
