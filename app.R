# This is the Shiny web application for STA141B final project
# 
# You can run the application by clicking the 'Run App' button above.
#

library(tigris)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(jsonlite)
library(httr)
library(dplyr)
library(rvest)
library(ggmap)
library(leafletCN)
library(geojsonio)
library(leaflet)
library(shiny)
library(DT)
library(lubridate)
library(plotly)

## Data Prep
##Section 1a :Data Import(CHINA)

DXY ="https://raw.githubusercontent.com/BlankerL/DXY-COVID-19-Data/master/csv/DXYArea.csv"
data <-read_csv(url(DXY)) %>%
    filter(countryEnglishName == "China") %>%
    select(countryName, countryEnglishName, 
           provinceEnglishName, 
           confirmedCount=province_confirmedCount,
           suspectedCount=province_suspectedCount, 
           curedCount=province_curedCount,
           deadCount=province_deadCount)

proname <- data.frame(provinceEnglishName = c("Taiwan", "China", "Hong Kong", "Guangdong", "Hubei", "Beijing", "Shanghai", "Gansu", "Zhejiang", "Heilongjiang", "Shandong", "Shaanxi", "Fujian", "Sichuan", "Guangxi", "Liaoning", "Tianjin", "Neimenggu", "Macau", "Guizhou", "Yunnan", "Hainan", "Ningxia", "Chongqing", "Jiangsu", "Henan", "Hunan", "Shanxi", "Hebei", "Jilin", "Jiangxi", "Xinjiang", "Anhui", "Qinghai", "Xizang"), provinceShortName = c("台湾", "中国", "香港", "广东", "湖北", "北京", "上海", "甘肃", "浙江", "黑龙江", "山东", "陕西", "福建", "四川", "广西", "辽宁", "天津", "内蒙古", "澳门", "贵州", "云南", "海南", "宁夏", "重庆", "江苏", "河南", "湖南", "山西", "河北", "吉林", "江西", "新疆", "安徽", "青海", "西藏"))

data<- data %>%
    left_join(proname)  %>%
    mutate(provinceShortName = as.character(provinceShortName))

u <- "https://raw.githubusercontent.com/chemzqm/geomap/master/china-province.geojson"
cngeojson<- geojson_read(u, what = "sp")

cnjoin <- geo_join(cngeojson, 
                   data, 
                   by_sp = "NAME", 
                   by_df = "provinceShortName",
                   how = "inner")

# time series data 

library (readr)

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

COVID <-read_csv(url(urlfile))
ts_data <- COVID %>%
    filter(`Country/Region` == "China") %>%
    select(-c("Country/Region","Lat","Long")) %>%
    rename(provinceEnglishName = `Province/State`) %>%
    pivot_longer(-provinceEnglishName, names_to = "updateTime", values_to = "confirmedNum")

ts_data$updateTime <- as.Date(paste(ts_data$updateTime,"20",sep = ""), "%m/%d/%Y")

# ##Section 1b :Data Import(USA)
# extract data
wikihtml <- "https://en.wikipedia.org/w/index.php?title=2020_coronavirus_outbreak_in_the_United_States&oldid=944107102"

wiki_table = read_html(wikihtml) %>% html_nodes("table") %>% .[[5]] %>% 
    html_table(fill = TRUE)
us_cases <- wiki_table[2:(dim(wiki_table)[1]-5),2:(dim(wiki_table)[2]-6)]
names(us_cases) <- wiki_table[1,2:(dim(wiki_table)[2]-6)]
for(j in 1:dim(us_cases)[2]){
    for(i in 1:dim(us_cases)[1]){
        if (us_cases[i,j] == "") us_cases[i,j] = "0"
    }
}
confirm_num <- sapply(us_cases, as.integer)
us_data <- cbind(updateTime = as_date(paste("2020",wiki_table[2:(dim(wiki_table)[1]-5),1])),
                 as.data.frame(apply(confirm_num, 2, cumsum)))
us_data_total <- us_data[dim(us_data)[1],]
us_data <- us_data %>%
    pivot_longer(-updateTime, names_to = "State", values_to = "confirmedNum")
us_data_total <- us_data_total %>%
    pivot_longer(-updateTime, names_to = "State", values_to = "confirmedNum")


us_map <- "https://raw.githubusercontent.com/loganpowell/census-geojson/master/GeoJSON/5m/2018/state.json"
usgeojson<- geojson_read(us_map, what = "sp")

usjoin <- geo_join(usgeojson, 
                   us_data_total, 
                   by_sp = "STUSPS", 
                   by_df = "State",
                   how = "inner")


# news from Yahoo

num_of_pages = 5
news_list <- vector("list", num_of_pages)

for (i in 1:num_of_pages){
    page <- 0
    news_url_path <- paste("https://news.search.yahoo.com/search;_ylt=AwrXoCHBj3Rer0AAhSrQtDMD;_ylu=X3oDMTFhN3Q4bTFjBGNvbG8DZ3ExBHBvcwMxBHZ0aWQDQjk3NzlfMQRzZWMDcGFnaW5hdGlvbg--?p=coronavirus&pz=10&fr=uh3_news_vert_gs&fr2=p%3Anews%2Cm%3Asa&bct=0&b=", page, "1&pz=10&bct=0&xargs=0", sep = '')
    h4 <- read_html(news_url_path) %>%
        html_nodes("h4")
    news_list[[i]] <- tibble(
        title = h4 %>% html_text(),
        url = h4 %>%  html_node("a") %>% html_attr("href")
    ) 
}

news_df <- do.call("rbind", news_list)
news_df <- news_df %>% 
    mutate(id = sprintf("%02d", row_number())) 
news_content <- NULL
for (i in seq_len(nrow(news_df))) {
    news_content <- bind_rows(
        news_content, 
        read_html(news_df$url[i]) %>% 
            html_nodes("article p") %>% 
            html_text() %>% {
                tibble(id = news_df$id[i], text = c(news_df$title[i], .)) #add title
            }
    )
}

news_tokens <- news_content %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    group_by(id) %>%
    count(word, sort = TRUE) %>% 
    arrange(id)

# Define UI for application that draws a histogram
##Section 2: set up the user interface

ui = navbarPage("NAVIGATION",
                tabPanel("China",
                         fluidPage(
                             #allows layout to fill browser window
                             
                             titlePanel("COVID19 UI PLATFORM"),
                             #adds a title to page and browser tab
                             #-use "title = 'tab name'" to name browser tab
                             
                             sidebarPanel(
                                 #add selectinput boxs
                                 htmlOutput("state_selector")# from objects created in server
                             ),
                             
                             mainPanel(
                                 titlePanel("CHINA"),
                                 tabsetPanel(
                                     tabPanel("Map", leafletOutput("map_density", height = "900px")), 
                                     tabPanel("Time Series Plot", plotlyOutput("time_series_plot", height = "500px")),
                                     tabPanel("Summary", DT::dataTableOutput("mytable1")))
                                 #leafletOutput("map_density1", height = "900px")
                             )
                         )),
                tabPanel("US",
                         fluidPage(
                             #allows layout to fill browser window
                             
                             titlePanel("COVID19 UI PLATFORM"),
                             #adds a title to page and browser tab
                             #-use "title = 'tab name'" to name browser tab
                             
                             sidebarPanel(
                                 #add selectinput boxs
                                 htmlOutput("state_selector_us")# from objects created in server
                             ),
                             mainPanel(
                                 titlePanel("US"),
                                 tabsetPanel(
                                     tabPanel("Map", leafletOutput("map_density_us", height = "900px")), 
                                     tabPanel("Time Series Plot", plotlyOutput("time_series_plot_us", height = "500px")),
                                     tabPanel("Summary", DT::dataTableOutput("mytable2")))
                             )
                         )
                ),
                tabPanel("More Information",
                         fluidPage(
                             #allows layout to fill browser window
                             
                             titlePanel("COVID19 UI PLATFORM"),
                             #adds a title to page and browser tab
                             #-use "title = 'tab name'" to name browser tab
                             
                             sidebarPanel(
                                 htmlOutput("new_selector"),# from objects created in server
                                 sliderInput("freq",
                                             "Minimum Frequency:",
                                             min = 1,  max = 50, value = 15),
                                 sliderInput("max",
                                             "Maximum Number of Words:",
                                             min = 1,  max = 300,  value = 100)
                             ),
                             mainPanel(
                                 titlePanel("News"),
                                 tabsetPanel(
                                     tabPanel("Summary", DT::dataTableOutput("mytable3")),
                                     tabPanel("Content", textOutput(outputId="news_content")),
                                     tabPanel("Word Cloud",plotOutput("word_cloud", width = "100%")))
                             )
                         ))
)

# Define server logic required to draw a histogram
##Section 3 

#China
server = shinyServer(function(input, output) {
    
    cnjoin_DT = cnjoin@data[,c("provinceEnglishName", "confirmedCount", 
                               "suspectedCount", "curedCount", "deadCount")]
    cnjoin_DT = cnjoin_DT[order(cnjoin_DT$confirmedCount, decreasing = FALSE),]
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(cnjoin_DT)
    })  
    
    
    
    output$state_selector = renderUI({#creates state select box object called in ui
        
        #creates a reactive list of available counties based on the State selection made
        
        selectInput(inputId = "state", #name of input
                    label = "Province:", #label displayed in ui
                    choices = c("All", sort(unique(cnjoin$provinceEnglishName), decreasing = FALSE)), #calls list of available counties
                    selected = "All")
    })
    
    output$state_selector_us = renderUI({#creates state select box object called in ui
        
        #creates a reactive list of available counties based on the State selection made
        selectInput(inputId = "state_us", #name of input
                    label = "State:", #label displayed in ui
                    choices = c("All", sort(unique(usjoin$NAME), decreasing = FALSE)), #calls list of available counties
                    selected = "All")
    })
    
    output$map_density <- renderLeaflet({
        
        bins <- c(0,1,10,50,100,500,1000,1500,10000,Inf)
        pal <- colorBin("YlOrRd", domain = cnjoin@data$confirmedCount, bins = bins, right = FALSE)
        
        # create labels for zipcodes
        labels <- 
            paste0(
                "Province: ", cnjoin@data$provinceEnglishName, "<br/>",
                "Confirmed: ",cnjoin@data$confirmedCount, "<br/>",
                "Suspected: ",cnjoin@data$suspectedCount, "<br/>",
                "Cured: ",cnjoin@data$curedCount, "<br/>",
                "Dead: ",cnjoin@data$deadCount) %>%
            lapply(htmltools::HTML)
        
        selected_confirmedCount = cnjoin@data$confirmedCount
        if (input$state != "All") {
            selected_confirmedCount = rep(0, nrow(cnjoin@data))
            selected_state_index = which(cnjoin@data$provinceEnglishName == input$state)
            selected_confirmedCount[selected_state_index] = cnjoin@data$confirmedCount[selected_state_index]
        }
        
        leaflet() %>%
            # add base map
            addTiles() %>% 
            setView(114.31,30.52, 4) %>%
            # add zip codes
            addPolygons(data = cnjoin, 
                        color = ~pal(selected_confirmedCount),
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        fillOpacity = 1,
                        label = labels) %>%
            # add legend
            addLegend(pal = pal, 
                      values = cnjoin@data$confirmedCount, 
                      opacity = 0.7, 
                      title = htmltools::HTML("Condition of <br> 
                                    Coronavirus in China"),
                      position = "bottomright")
        
        
    })
    
    #time series 
    
    output$time_series_plot <- renderPlotly({
        
        if (input$state != "All") {
            selected_ts =  ts_data[ts_data$provinceEnglishName == input$state,]
        } else     {selected_ts = ts_data %>% group_by(updateTime) %>%
            summarize(confirmedNum = sum(confirmedNum))}
        
        selected_ts %>%
            plot_ly(x = ~updateTime, y = ~confirmedNum) %>%
            add_lines(color = "#E7B800") %>%
            layout(title = 'Time Series Plot of Confirmed Number',
                   xaxis = list(title = 'Update Time'),
                   yaxis = list(title = 'Confirmed Number'))
    })
    
    #US
    
    usjoin_DT = usjoin@data[, c("NAME", "confirmedNum")]
    usjoin_DT = usjoin_DT[order(usjoin_DT$confirmedNum, decreasing = TRUE), ]
    
    output$mytable2 <- DT::renderDataTable({
        DT::datatable(usjoin_DT)
    })
    
    
    output$map_density_us <- renderLeaflet({
        
        bins <- c(0,1,10,50,100,500,1000,1500,10000,Inf)
        pal <- colorBin("YlGnBu", domain = cnjoin@data$confirmedCount, bins = bins, right = FALSE)
        
        
        # create labels for zipcodes
        labels <- 
            paste0(
                "State: ", usjoin@data$STUSPS, "<br/>",
                "Confirmed: ", usjoin@data$confirmedNum, "<br/>") %>%
            lapply(htmltools::HTML)
        
        selected_confirmedCount_us = usjoin@data$confirmedNum
        if (input$state_us != "All") {
            selected_confirmedCount_us = rep(0, nrow(usjoin@data))
            selected_state_index_us = which(usjoin@data$NAME == input$state_us)
            selected_confirmedCount_us[selected_state_index_us] = usjoin@data$confirmedNum[selected_state_index_us]
        }
        
        
        leaflet() %>%
            # add base map
            addTiles() %>% 
            setView(-97.3702735,30.3496528, 4) %>%
            # add zip codes
            addPolygons(data = usjoin, 
                        fillColor = ~pal(selected_confirmedCount_us),
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        label = labels)  %>%
            # add legend
            addLegend(pal = pal, 
                      values = usjoin@data$confirmedNum, 
                      opacity = 0.7, 
                      title = htmltools::HTML("Condition of <br> 
                                    Coronavirus in the US"),
                      position = "bottomright")
        
    })
    
    #time series 
    
    output$time_series_plot_us <- renderPlotly({
        
        if (input$state_us != "All") {
            selected_ts_us =  us_data[us_data$State ==  usjoin@data[which(usjoin@data$NAME == input$state_us),"STUSPS"],]
        } else     {selected_ts_us = us_data %>% group_by(updateTime) %>%
            summarize(confirmedNum = sum(confirmedNum))}
        
        selected_ts_us %>%
            plot_ly(x = ~updateTime, y = ~confirmedNum) %>%
            add_lines(color = "#00AFBB") %>%
            layout(title = 'Time Series Plot of Confirmed Number',
                   xaxis = list(title = 'Update Time'),
                   yaxis = list(title = 'Confirmed Number'))
    })
    
    #News
    output$mytable3 <- DT::renderDataTable({
        DT::datatable(news_df)
    })  
    
    output$new_selector = renderUI({#creates state select box object called in ui
        
        #creates a reactive list of available counties based on the State selection made
        
        selectInput(inputId = "id", #name of input
                    label = "News id:", #label displayed in ui
                    choices = unique(news_content$id), #calls list of available counties
                    selected = "01")
    })
    
    selected_text <- news_content %>%
        filter(id == input$id) %>%
        select(text)
    selected_content <- c()
    for(i in 1:dim(selected_text)[1]) {
        selected_content<- c(selected_content,as.character(selected_text[i,]),"\n","\n")
    }
    output$news_content <- renderText({ selected_content })

    
    output$word_cloud <- renderPlot({
        print(
            news_tokens %>% 
                filter(id == input$id) %>% 
                with(wordcloud(
                    word, n,scale=c(4,0.5), 
                    min.freq = input$freq, max.words=input$max, random.order = FALSE,
                    colors = brewer.pal(8, "Dark2")))
        )
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
