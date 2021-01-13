library(shiny)
library(leaflet)
library(sp)
library(dplyr)
library(geosphere)

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:after {
  visibility: visible;
  content: 'The following origin does not exist for the chosen airline. Please choose some other option :)'; }
}
"

airports = read.csv("data/airports.csv")
carriers = read.csv("data/carriers.csv")
cancel_data = readRDS("data/rds_cancel.rds")
all_data = readRDS("data/rds_append.rds")


all_data$is_arr = ifelse(is.na(all_data$ArrDelay) | all_data$ArrDelay < 0, 0, 1)
all_data$is_dep = ifelse(is.na(all_data$DepDelay) | all_data$DepDelay < 0, 0, 1)



count_table = all_data %>% 
    group_by(Year, UniqueCarrier, Origin, Dest) %>% 
    summarise(total_flights = n(), total_cancel = sum(Cancelled),
              total_arr_delay = sum(is_arr),
              total_dep_delay = sum(is_dep))

rate_table = count_table %>% 
    mutate(cancel_rate = round(total_cancel*100/total_flights,2),
           arr_delay_rate = round(total_arr_delay*100/total_flights,2),
           dep_delay_rate = round(total_dep_delay*100/total_flights,2))



#origin selectinput
ori_namedata = left_join(cancel_data, airports, by = c("Origin" = "iata"))

choices = data.frame(
    var = unique(ori_namedata$airport),
    num = unique(cancel_data$Origin)
)

# List of choices for selectInput
mylist = as.list(choices$num)
# Name it
names(mylist) <- choices$var

#airport select input
cancel_carrier_name = left_join(cancel_data, carriers, by = c("UniqueCarrier" = "Code"))

choices2 = data.frame(
    var = unique(cancel_carrier_name$Description),
    num = unique(cancel_data$UniqueCarrier)
)

# List of choices for selectInput
mylist2 = as.list(choices2$num)
# Name it
names(mylist2) <- choices2$var



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    title = "Path of Cancelled Flights",
    
    leafletOutput("plot"),
    
    hr(),
    
    fluidRow(tags$style(type="text/css", css),
             textOutput("text1"),
             column(2,
                    selectInput("car", 
                                label = "Select Carrier",
                                choices = mylist2,
                                selected = "AA"
                    ),
                    selectInput("air",
                                label = "Select Origin",
                                choices = mylist,
                                selected = "SEA"),
                    selectInput("yr",
                                label = "Select Year",
                                choices = c(1999, 2003))
             ),
             column(4,
                    dataTableOutput("tab")
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderLeaflet({
        
        subset_year = subset(cancel_data, Year == input$yr)
        carrier_data = subset(subset_year, UniqueCarrier == input$car)
        
        carrier_data_sea = subset(carrier_data, Origin == input$air)
        
        origin_join = left_join(carrier_data_sea, airports, by = c("Origin" = "iata"))
        origin_join = rename(origin_join, lat.origin = lat)
        origin_join = rename(origin_join, long.origin = long)
        fin = left_join(origin_join, airports, by = c("Dest" = "iata"))
        fin = rename(fin, lat.dest = lat)
        fin = rename(fin, long.dest = long)
        
        inters = NULL
        for (i in 1:nrow(fin)) {
            k = gcIntermediate(c(fin$long.origin[1],fin$lat.origin[1]),
                               c(fin$long.dest[i], fin$lat.dest[i]),
                               n = 150, sp = TRUE)
            inters = c(k, inters)
        }
        
        inters = rev(inters)
        ll0 <- lapply( inters , function(x) `@`(x , "lines"))
        ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines"))
        Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 )))
        
        icons_ori <- awesomeIcons(
            icon = 'fa-plane',
            iconColor = 'black',
            library = 'fa',
            markerColor = "darkred"
        )
        
        icons_dep <- awesomeIcons(
            icon = 'fa-plane',
            iconColor = 'black',
            library = 'fa',
            markerColor = "darkblue"
        )
        
        fin$label_origin = paste0(fin$airport.x ,"(", fin$Origin, ")")
        fin$label_dest = paste0(fin$airport.y ,"(", fin$Dest, ")")
        
        leaflet(Sl) %>% addTiles() %>% addPolylines(color = "white") %>% 
            addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% 
            setView(lng = -71.0589, lat = 42.3601, zoom = 4) %>% 
            addAwesomeMarkers(~long.origin, ~lat.origin, label = fin$label_origin, data = fin,
                              icon = icons_ori) %>% 
            addAwesomeMarkers(~long.dest, ~lat.dest, label = fin$label_dest, data = fin, icon = icons_dep)
        
    })
    
    fin_data = reactive({ 
        rate_table %>%
            filter(Year == input$yr, UniqueCarrier == input$car, Origin == input$air)
    })
    
    output$tab = renderDataTable({
        fin_data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
