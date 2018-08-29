#' Creating a basic, unscaled bar chart of Prime Contract Obligations by Agency
#'
#' @param company_name Name of the company, used in filename and chart title
#' @param FY Fiscal year to filter out, commonly the current fiscal year due to incomplete data
#' @param n_agencies The number of agencies to include in the view; defaults to 6
#' @param scale Dollar scale; default to 1000000
#' @param scale_text Dollar scale as text; defaults to Millions
#' @param FY_range Fiscal Year range included, for example "FY14-FY17"
#' @param num_size Size of heading text for agency names, default to 3.  Note that the fewer
#' the agencies used, the larger the text should be
#' @param h Height of saved chart; defaults to 6
#' @param w Width of saved chart; defaults to 11.  Note that more agencies requires a greater width
#'
#' @return A bar chart
#'
#' @details The returned chart will determine whether this function is sufficient or a different,
#' more scaled chart is needed
#'
#' @examples bar_primeob_by_agency(company_name = "Leidos", FY = 2018, n_agencies = 5,
#'                                 scale = 1000000000, scale_text = "Billions", FY_range = "FY14-FY17",
#'                                 num_size = 3, h = 7, w = 13)

bar_primeob_by_agency <- function(company_name,
                                  FY = 1,
                                  n_agencies = 6,
                                  scale = 1000000,
                                  scale_text = "Millions",
                                  FY_range,
                                  num_size = 3,
                                  h = 6,
                                  w = 11){

  #Location for saving charts
  setwd("X:/1 Marielle Folder/Data For R/Created Charts/2018/Contract Obligations by Agency")

  data <- read_csv(paste("X:/1 Marielle Folder/Data For R/Contract Obligation by Agency/Company Profiles/", company_name,
                         " Company Profile.csv", sep = ""))
  ###Get top n agencyies by obligation
  top_n_agencies <- data %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != FY) %>%
    group_by(funding_agency) %>%
    dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
    arrange(desc(grand_total_transaction_value)) %>%
    top_n(n_agencies)

  top_n_agencies <- top_n_agencies$funding_agency

  ###Process Data to get total transaction value by year

  data.agency.year <- data %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != FY) %>%
    filter(funding_agency %in% top_n_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

  data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)
  data.agency.year$facet = factor(data.agency.year$funding_agency, levels = c(top_n_agencies))

  ###Create Barplot and Save as JPG
  plot <- plot.one(data.agency.year, "funding_agency", num_size, scale_text, company_name, FY_range)

  plot
  data.agency.year

  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot,
         width = w, height = h, units = "in")
}


#' Creating a bar chart of Prime Contract Obligations by Agency that is scaled into a Top
#' Range and Bottom Range (two scalings)
#'
#' @param company_name Name of the company, used in filename and chart title
#' @param FY Fiscal year to filter out, commonly the current fiscal year due to incomplete data
#' @param n_agencies The number of agencies to include in the view; defaults to 6
#' @param scale Dollar scale; default to 1000000
#' @param scale_text Dollar scale as text; defaults to Millions
#' @param FY_range Fiscal Year range included, for example "FY14-FY17"
#' @param top_range agencies to be included in the first scaling
#' @param bottom_range agencies to be included in the second scaling
#' @param optional_third_range if scaling data by three ranges, use this field; otherwise defaults to NULL
#' @param grid_division respective sizes of ranges to arrange the final grid.  This generally is
#' the number of agencies in the top_range and the number in the bottom_range written as a vector.
#' @param num_size Size of heading text for agency names, default to 3.  Note that the fewer
#' the agencies used, the larger the text should be
#' @param h Height of saved chart; defaults to 6
#' @param w Width of saved chart; defaults to 11.  Note that more agencies requires a greater width
#'
#' @return A bar chart, with two differently scales charts of the data
#'
#' @details Unless already known, use the bar_primeob_by_agency() function to determine the Top Range
#' and Bottom Range agencies.
#'
#'
#' @examples bar_primeob_by_agency _scaling(company_name = "Leidos", FY = 2018,
#'                                          n_agencies = 5, scale = 1000000000,
#'                                          scale_text = "Billions", FY_range = "FY14-FY17",
#'                                          top_range = c(1:2,4), bottom_range = c(3,5),
#'                                          grid_division = c(3,2), num_size = 3,
#'                                          h = 7, w = 13)
#'
#'
bar_primeob_by_agency_scaling <- function(company_name,
                                          FY = 1,
                                          n_agencies = 6,
                                          scale = 1000000,
                                          scale_text = "Millions",
                                          FY_range,
                                          top_range,
                                          bottom_range,
                                          optional_third_range = NULL,
                                          grid_division,
                                          num_size = 4,
                                          h = 6,
                                          w = 11){

  #Location for saving charts
  setwd("X:/1 Marielle Folder/Data For R/Created Charts/2018/Contract Obligations by Agency")

  data <- read_csv(paste("X:/1 Marielle Folder/Data For R/Contract Obligation by Agency/Company Profiles/", company_name,
                         " Company Profile.csv", sep = ""))

  ###Get top n agencyies by obligation
  top_n_agencies <- data %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != FY) %>%
    group_by(funding_agency) %>%
    dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
    arrange(desc(grand_total_transaction_value)) %>%
    top_n(n_agencies)

  if(is.null(optional_third_range)){
    ##Seperate Out different scales
    top_n_agencies <- top_n_agencies$funding_agency
    top_agencies <- top_n_agencies[top_range]
    bottom_agencies <- top_n_agencies[bottom_range]


    ###Process Data to get total transaction value by year - for top and bottom agencies

    data.agency.year.top <- data %>%
      select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
      dplyr::rename(fiscal_year = "Fiscal Year",
                    funding_agency = "Funding Agency",
                    transaction_value = "Transaction Value") %>%
      filter(fiscal_year != FY) %>%
      filter(funding_agency %in% top_agencies) %>%
      dplyr::group_by(funding_agency, fiscal_year) %>%
      dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

    data.agency.year.top$fiscal_year = as.character(data.agency.year.top$fiscal_year)
    data.agency.year.top$facet = factor(data.agency.year.top$funding_agency, levels = c(top_agencies))


    data.agency.year.bottom <- data %>%
      select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
      dplyr::rename(fiscal_year = "Fiscal Year",
                    funding_agency = "Funding Agency",
                    transaction_value = "Transaction Value") %>%
      filter(fiscal_year != FY) %>%
      filter(funding_agency %in% bottom_agencies) %>%
      dplyr::group_by(funding_agency, fiscal_year) %>%
      dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

    data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)
    data.agency.year.bottom$facet = factor(data.agency.year.bottom$funding_agency, levels = c(bottom_agencies))
    ###Create Barplot and Save as JPG
    plot1 <- plot.first(data.agency.year.top, "funding_agency", num_size, scale_text)


    plot2 <- plot.last(data.agency.year.bottom, "funding_agency", num_size)

    plot.all<-grid.arrange(plot1, plot2, nrow = 1, widths = grid_division,
                           top = textGrob(paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year")



    ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot.all,
           width = w, height = h, units = "in")
    data.agency.year.all <- rbind(data.agency.year.top, data.agency.year.bottom)

  }
  else{
    ##Seperate Out different scales
    top_n_agencies <- top_n_agencies$funding_agency
    top_agencies <- top_n_agencies[top_range]
    middle_agencies <- top_n_agencies[optional_third_range]
    bottom_agencies <- top_n_agencies[bottom_range]



    ###Process Data to get total transaction value by year - for top and bottom agencies

    data.agency.year.top <- data %>%
      select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
      dplyr::rename(fiscal_year = "Fiscal Year",
                    funding_agency = "Funding Agency",
                    transaction_value = "Transaction Value") %>%
      filter(fiscal_year != FY) %>%
      filter(funding_agency %in% top_agencies) %>%
      dplyr::group_by(funding_agency, fiscal_year) %>%
      dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

    data.agency.year.top$fiscal_year = as.character(data.agency.year.top$fiscal_year)
    data.agency.year.top$facet = factor(data.agency.year.top$funding_agency, levels = c(top_agencies))


    data.agency.year.bottom <- data %>%
      select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
      dplyr::rename(fiscal_year = "Fiscal Year",
                    funding_agency = "Funding Agency",
                    transaction_value = "Transaction Value") %>%
      filter(fiscal_year != FY) %>%
      filter(funding_agency %in% bottom_agencies) %>%
      dplyr::group_by(funding_agency, fiscal_year) %>%
      dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

    data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)
    data.agency.year.bottom$facet = factor(data.agency.year.bottom$funding_agency, levels = c(bottom_agencies))


    data.agency.year.middle <- data %>%
      select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
      dplyr::rename(fiscal_year = "Fiscal Year",
                    funding_agency = "Funding Agency",
                    transaction_value = "Transaction Value") %>%
      filter(fiscal_year != FY) %>%
      filter(funding_agency %in% middle_agencies) %>%
      dplyr::group_by(funding_agency, fiscal_year) %>%
      dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

    data.agency.year.middle$fiscal_year = as.character(data.agency.year.middle$fiscal_year)
    data.agency.year.middle$facet = factor(data.agency.year.middle$funding_agency, levels = c(middle_agencies))



    ###Create Barplot and Save as JPG
    plot1 <- plot.first(data.agency.year.top, "funding_agency", num_size, scale_text)

    plot2 <- plot.middle(data.agency.year.middle, "funding_agency", num_size)


    plot3 <- plot.last(data.agency.year.bottom, "funding_agency", num_size)


    plot.all<-grid.arrange(plot1, plot2, plot3, nrow = 1, widths = grid_division,
                           top = textGrob(paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year")



    ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot.all,
           width = w, height = h, units = "in")
    data.agency.year.all <- rbind(data.agency.year.top, data.agency.year.middle, data.agency.year.bottom)
  }
  plot.all
  data.agency.year.all

}

#' Creating a bar chart of Prime Contract Obligations by Agency looking at an agency
#' (or sub-agency) chosen by name to display
#'
#' @param company_name Name of the company, used in filename and chart title
#' @param funding_agency_type1 Which field or subfield the agency is listed under
#' @param funding_agency_name1 The specific agency to display
#' @param funding_agency_type2 optional second agency type; defaults to null
#' @param funding_agency_name2 optional second agency name; defaults to null
#' @param funding_agency_type3 optional third agency type; defaults to null
#' @param funding_agency_name3 optional third agency type; defaults to null
#' @param funding_agency_type4 optional fourth agency type; defaults to null
#' @param funding_agency_name4 optional fourth agency name; defaults to null
#' @param funding_agency_type5 optional fifth agency type; defaults to null
#' @param funding_agency_name5 optional fifth agency name; defaults to null
#' @param FY Fiscal year to filter out, commonly the current fiscal year due to incomplete data
#' @param scale Dollar scale; default to 1000000
#' @param scale_text Dollar scale as text; defaults to Millions
#' @param grid_division respective sizes of ranges to arrange the final grid.  This generally is
#' the number of agencies in each plot written as a vector
#' @param num_size Size of heading text for agency names, default to 3.  Note that the fewer
#' the agencies used, the larger the text should be
#' @param h Height of saved chart; defaults to 6
#' @param w Width of saved chart; defaults to 11.  Note that more agencies requires a greater width
#'
#' @return A bar chart, with two differently scales charts of the data
#'
#' @details Unless already known, use the bar_primeob_by_agency() function to determine the Top Range
#' and Bottom Range agencies.
#'
#'
#' @examples bar_primeob_by_agency_choosing(company_name = "LMI Consulting",
#'                                          funding_agency_type1 = "Funding Bureau",
#'                                          funding_agency_name1 = "Customs and Border Protection
#'                                          (CBP)", FY = 2018, scale = 1000000,
#'                                          scale_text = "Millions", FY_range = "FY14-FY17",
#'                                          num_size = 3, h = 6, w = 11)
#'
#' @export
bar_primeob_by_agency_choosing <- function(company_name,
                                           funding_agency_type1,
                                           funding_agency_name1,
                                           funding_agency_type2 = NULL,
                                           funding_agency_name2 = NULL,
                                           funding_agency_type3 = NULL,
                                           funding_agency_name3 = NULL,
                                           funding_agency_type4 = NULL,
                                           funding_agency_name4 = NULL,
                                           funding_agency_type5 = NULL,
                                           funding_agency_name5 = NULL,
                                           FY = 1,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           grid_division = NULL,
                                           FY_range,
                                           num_size = 3,
                                           h = 6,
                                           w = 11){

  data <- read_csv(paste("X:/1 Marielle Folder/Data For R/Contract Obligation by Agency/Company Profiles/", company_name,
                         " Company Profile.csv", sep = ""))


  ###Process Data to get total transaction value by year
  if(is.null(funding_agency_name2)){
    data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
    ###Create Barplot and Save as JPG
    plot.all <- plot.one(data.agency.year1, "funding_agency", num_size, scale_text, funding_agency_name1, FY_range)

    data.agency.year.all <- data.agency.year1

  }
  else{
    if(is.null(funding_agency_name3)){
      data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
      data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)

      ###Create Barplot and Save as JPG
      plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
      plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
      
      legend <- g_legend(plot1 + theme(legend.position='right'))

      plot.all<-grid_arrange_shared_legend(plot1, plot2, legend, widths = grid_division,
                             top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
      data.agency.year.all <- rbind(data.agency.year1, data.agency.year2)

    }
    else{
      if(is.null(funding_agency_name4)){
        data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
        data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
        data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)

        ###Create Barplot and Save as JPG
        plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
        plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
        plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
        
        legend <- g_legend(plot1 + theme(legend.position='right'))

        plot.all<-grid_arrange_shared_legend(plot1, plot2, plot3, legend, widths = grid_division,
                               top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                              gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
        data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3)

      }
      else{
        if(is.null(funding_agency_name5)){
          data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
          data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
          data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
          data.agency.year4 <- process.data.get.sum(data, funding_agency_name4, funding_agency_type4, FY, scale)

          ###Create Barplot and Save as JPG
          plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
          plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
          plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
          plot4 <- plot.middle(data.agency.year4, "funding_agency", num_size)

          legend <- g_legend(plot1 + theme(legend.position='right'))
          
          plot.all<-grid_arrange_shared_legend(plot1, plot2, plot3, plot4, legend, widths = grid_division,
                                 top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                                gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
          data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3, data.agency.year4)

        }
        else{
          data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
          data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
          data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
          data.agency.year4 <- process.data.get.sum(data, funding_agency_name4, funding_agency_type4, FY, scale)
          data.agency.year5 <- process.data.get.sum(data, funding_agency_name5, funding_agency_type5, FY, scale)

          ###Create Barplot and Save as JPG
          plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
          plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
          plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
          plot4 <- plot.middle(data.agency.year4, "funding_agency", num_size)
          plot5 <- plot.middle(data.agency.year5, "funding_agency", num_size)

          plot.all<-grid.arrange(plot1, plot2, plot3, plot4, plot5, legend, widths = grid_division, 
                                 top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                                gp = gpar(fontsize = 24)), bottom = "Fiscal Year")

          data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3,
                                        data.agency.year4, data.agency.year5)

        }

      }

    }

  }


  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot.all,
         width = w, height = h, units = "in")
  plot.all
  data.agency.year.all

}


