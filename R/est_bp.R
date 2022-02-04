colores <- c("#182B47", "#BF0A33", "#00727C", "#FF7800", "#646567",
             "#31B133", "#FFCB00", "#6D3A77", "#8C734A","#A6A6A6","#182B47",
             "#652F6C")
#Azul, Rojo, Verde, Naranja, Gris, Verde2, Amarillo, Morado, Oro, Gris_lineas
#,negro,colorREF_morado


#' Statistics of a BoxPlot
#' @description The function "est_bp" Calculate the statistics of a boxplot
#' @param boxp Graph created with "grafica_BoxPlot"
#' @param date_factor Dates as a factor
#' @param name_file .xlsx file name
#' @return Returns the quantiles 25, 50-25, 75-50 and the average of the data
#' @references Ggplot2: Elegant Graphics for Data Analysis
#' @author Marcos Daniel Ramirez Santiago, \email{danielrs98@@outlook.com}
#' @export



est_bp<-function(boxp,date_factor,name_file){
  df<-ggplot_build(boxp)$data
  p_25<-df[[1]]["lower"]#
  colnames(p_25)<-c("p_25")
  p_50<-df[[1]]["middle"]
  p_75<-df[[1]]["upper"]
  media<-df[[2]]["y"]#
  colnames(media)<-c("promedio")

  p_50_p_25<-p_50-p_25
  colnames(p_50_p_25)<-c("p50-p25")

  p_75_p_50<-p_75-p_50
  colnames(p_75_p_50)<-c("p75-p50")


  bp_data<-cbind(date_factor,p_25,p_50_p_25,p_75_p_50,media)

  write.xlsx(bp_data,paste(name_file,".xlsx",sep=""))

}

