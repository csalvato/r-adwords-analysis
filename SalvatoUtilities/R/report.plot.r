#' Report Plot
#'
#' Creates a plot for an exported report.
#' Writes a ggplot object to a plot using plot() or writes the plot to a PNG file.
#'
#' @param ggplot_object ggplot object to be plotted.
#' @param file (optional) File name for the file to be saved.  If not supplied
#' then the data frame's name is used.  Note, ".csv" is appended automatically.
#' @export
#' @examples
#' test <- ggplot(
#'        keywords_weekly_conversion_metrics, 
#'        aes(x=week, y=est_search_impression_share, fill=campaign_name)) +
#'        geom_bar(stat="identity", position="dodge") +
#'        ggtitle("AdWords - Weekly Impression Share by Geo") + 
#'        ylim(0, 1) +
#'        facet_wrap(~keyword, ncol=2)
#'      )
#' report.plot(test, "example.png") # Writes to file "example.png"
#' report.plot(test, "") # Writes to file test.png
#' report.plot(test, NULL) # Plots without writing to file.

report.plot <- function(ggplot_object, file=""){
  # If the file name is not specified, use the data frame variable name.
  if( file == "" ) {
  	file <- deparse(substitute(data_frame)) #gets variable name as string	
  }

  # If doesn't have .png extension in the name, add it.
  if ( !grepl("\\.png$", file) ) {
  	 file <- paste0(file,".png")
 	}

  if( !is.null(file) ) {
    require(SalvatoUtilities)
    png( filename = file, 
         width = 1280, 
         height = 800)
    plot( ggplot_object )
    dev.off()
  } else {
    plot( ggplot_object )
  }

  
}