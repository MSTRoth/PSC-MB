# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Process contract obligations data from BGov, yielding the total transaction value  by funding agency and fiscal year
#'
#'@param data.frame data frame
#'@param funding_agency_name The name of the funding agency or subgroup
#'@param funding_agency_type The field or subfield the agency is listed under
#'@param FY Fiscal year to filter out, commonly the current fiscal year due to incomplete data
#'@param scale Dollar scale; default to 1000000
#'
#'@return data frame with the total transaction value (contract obligations) per fiscal year per funding agency or subfield (all at the same level)
#'
#'
#'@export
process.data.get.sum <- function(data.frame, funding_agency_name, funding_agency_type, FY = 1, scale = 1000000){
  data.agency.year <- data.frame %>%
    select("Fiscal Year", funding_agency_type, "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = funding_agency_type,
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != FY) %>%
    filter(funding_agency == funding_agency_name) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

  data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)

  data.agency.year
}


#' Create a bar graph from contract obligation data for one plot with a single scale
#'
#'@param data.frame data frame
#'@param num_size size of the text that labels the total transaction value in the bar chart; enerally 3 or 4
#'@param facet_var  The facetted variable for facet_grid()
#'@param scale_text The dollar scale in text form; defaults to "Millions"
#'@param company_name The name of the vendor
#'@param FY_range Fiscal years displayed in the bar chart as a text string; e.g. "FY14-FY17"
#'
#'@return ggplot
#'
#'@examples plot.one(data, facet_var = "funding_agency", 3, scale_text = "Millions", "Leidos", "FY14-FY17")
#'
#'
#'@export
plot.one <- function (data.frame, facet_var, num_size, scale_text = "Millions", company_name, FY_range){
  plot <- ggplot(data.frame, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
    labs(x="Fiscal Year", y = paste("Contract Obligations (in ", scale_text, ")", sep = ""),
         title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())

  plot

}


#' Create a bar graph from contract obligation data
#'
#'@param data.frame data frame
#'@param num_size size of the text that labels the total transaction value in the bar chart; generally 3 or 4
#'@param facet_var  The facetted variable for facet_grid()
#'@param scale_text The dollar scale in text form; defaults to "Millions"
#'
#'@return ggplot
#'
#'@details This is the first plot in a series of plots (at least two), meant to be used with grid.arrange with other plots
#'
#'
#'@export
plot.first <- function (data.frame, facet_var, num_size, scale_text){
  plot <- ggplot(data.frame, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
    labs(y = paste("Contract Obligations (in ", scale_text, ")", sep = "")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())

  plot
}



#' Create a bar graph from contract obligation data
#'
#'@param data.frame data frame
#'@param facet_var  The facetted variable for facet_grid()
#'@param num_size size of the text that labels the total transaction value in the bar chart; generally 3 or 4
#'
#'@return ggplot
#'
#'@details This is the last plot in a series of plots (at least two), meant to be used with grid.arrange
#'
#'
#'@export
plot.last <- function (data.frame, facet_var, num_size){
  plot <- ggplot(data.frame, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(),
          axis.title.y = element_blank())

  plot
}


#' Create a bar graph from contract obligation data
#'
#'@param data.frame data frame
#'@param facet_var  The facetted variable for facet_grid()
#'@param num_size size of the text that labels the total transaction value in the bar chart; generally 3 or 4
#'
#'@return ggplot
#'
#'@details This is for any middle plot in a series of plots (at least three), meant to be used with grid.arrange
#'
#'@examples
#'
#'
#'@export
plot.middle <- function (data.frame, facet_var, num_size){
  plot <- ggplot(data.frame, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(),
          axis.title.y = element_blank())+ guides(fill="none")

  plot

}

#' #' Extract a common legend for multiple plots
#' #'
#' #'@param data.frame data frame
#' #'@param facet_var  The facetted variable for facet_grid()
#' #'@param num_size size of the text that labels the total transaction value in the bar chart; generally 3 or 4
#' #'
#' #'@return ggplot
#' #'
#' #'@details https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#' #'
#' #'
#' #'@export
#' 
#' g_legend<-function(a.gplot){
#'   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#'   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#'   legend <- tmp$grobs[[leg]]
#'   return(legend)}
#' 
#' mylegend<-g_legend(p1)
#' 
#' p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
#'                                p2 + theme(legend.position="none"),
#'                                nrow=1),
#'                    mylegend, nrow=2,heights=c(10, 1))