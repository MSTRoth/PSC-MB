#'Quarter comparisons for Contract Obligations - Civilian/Defense Breakout
#'
#'@param year year(s) to be included in the chart, as a vector
#'@param num_size Size of number text labels, default to 3; labels for total sum equals num_size + 1
#'@param notyear_prop The current year, not to include a proportion
#'@param FY_range Range of fiscal years displayed for the title, as text string
#'@param title Title of chart, as text string; use \n for multiple lines; defaults to paste("Contract Obligations by Quarter: ", FY_range, sep = "")
#'@param subtitle Subtitle of chart, as text string; defaults to NULL
#'@param h Height of saved chart; defaults to 6
#'@param w Width of saved chart; defaults to 11
#'@param file_ext file extension, as text string; .jpg or .pdf; defaults to .jpg
#'
#'@return stacked bar chart
#'
#'@details The returned chart will display all contract obligations by quarter for a given range of fiscal years
#'with a breakout by civilian and defense agencies
#'
#'@import colorspace
#'
#'@export

contract_obs_by_quarter <- function(year, num_size = 3, notyear_prop,
                                    FY_range, title = paste("Contract Obligations by Quarter: ", FY_range, sep = ""),
                                    subtitle = NULL, h = 6, w = 11, file_ext = ".jpg"){

#Location for saving charts
setwd("X:/1 Marielle's Folder/Data For R/Created Charts/2018/Government-Wide") #location charts are saved

data <- read_csv("X:/1 Marielle's Folder/Data For R/Government-Wide and DPAP Visualizations/Civilian and Defense Data by quarter.csv")

data$Year = as.character(data$Year)

data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year %in% year)


plot -> ggplot(data.civdef, aes(x = civ_def, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = num_size, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civdef, Year != notyear_prop), aes(label = sprintf('%.0f%%', prop), y = label_y), size = num_size, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = sum(num_size,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
  facet_grid(~FYYear, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions",
       title = title, subtitle = subtitle) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold"), axis.title.x = element_blank())


  ggsave(paste("All Contract Obligations Civ-Def ",FY_range, " by quarter", file_ext, sep = ""), plot,
         width = w, height = h, units = "in")


}


#' Total Contract Spending, divided into services and products, based on type
#'
#'@param type text string; choose one of "Government Wide", "DoD", or "Civilian"
#'@param num_size Size of number text labels, default to 4
#'@param FY_range Range of fiscal years displayed for the title, as text string
#'@param h Height of saved chart; defaults to 6
#'@param w Width of saved chart; defaults to 11
#'@param file_ext file extension, as text string; .jpg or .pdf; defaults to .jpg
#'
#'@return a bar chart
#'
#'@detail Returns a stacked bar chart with DPAP services defined and products, with labels for total services and products
#'
#'@export

total_contract_spending <- function(type,
                                    num_size = 4,
                                    FY_range,
                                    h = 6,
                                    w = 11,
                                    file_ext = ".jpg"){
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - ",
                       type, ".csv", sep = ""))


data$`DPAP Category` <- factor(data$`DPAP Category`,
                                  levels = c("Products", "Construction Services",
                                             "Electronic & Communication Services",
                                             "Equipment Related Services",
                                             "Facility Related Services",
                                             "Knowledge Based Services",
                                             "Logistics Management Services",
                                             "Medical Services",
                                             "Research and Development",
                                             "Transportation Services"),
                                  ordered = is.ordered(data$`DPAP Category`))

DPAP <- data %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$ billions`))


label_height <- DPAP %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$ billions`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP, by = c("Fiscal Year", "pors", "pors$") )



plot <- ggplot(label_height, aes(x = `Fiscal Year`, y = `$ billions`,
                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = num_size, vjust = 1.5, check_overlap = TRUE)+
  ##############################scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = paste(type, " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())

ggsave(paste(type, " Total Contract Spending Service Product ", FY_range, file_ext, sep = ""), plot,
       width = w, height = h, units = "in")

}





