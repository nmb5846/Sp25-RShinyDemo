
library("devtools")
library("raster")
library("terra")
library("dplyr")
library("ggplot2")
library("sf")
library("tidyterra")
library("lubridate")
library("shiny")
library("bs4Dash")
library("readr")
library("plotly")
library("leaflet")
library("DT")
library("fresh")
library("tigris")
library("CropScapeR")
library("rinat")



Species1 <- get_inat_obs(taxon_name = "Osmia", place_id = 42, quality = "research", maxresults = 10000)


#confirm the number of observations pulled from iNaturalist (as of 2/26/2025 this is 196)
nrow(Species1)

#preview distribution of observations on a map
Sp1_map<- inat_map(Species1, plot = TRUE)

#SPECIES2 Dataset


Species2 <- get_inat_obs(taxon_name = "Xylocopa", place_id = 42, quality = "research", maxresults = 10000)
nrow(Species2)
Sp2_map <- inat_map(Species2, plot = TRUE)


#OK! So now we have our data!



#--SPECIES1--

Sp1year <-lubridate::year(Species1$observed_on) #if observations were pulled from different years
Sp1month <-lubridate::month (Species1$observed_on)

Sp1num_sightings <- nrow(Species1) #we are going to report this value on our RShiny page
Sp1over_years <- names(which.max(table(Sp1year))) #this one too (as an info box); if you are working with only 1 year of data you may choose to customize this info box differently


#--SPECIES2---

Sp2year <-lubridate::year(Species2$observed_on) 
Sp2month <-lubridate::month (Species2$observed_on)

Sp2num_sightings <- nrow(Species2) 
Sp2over_years <- names(which.max(table(Sp2year)))




#----Load and wrangle Cropscape CDL information---

cdl_colormap <- read.csv("~/RShiny Sp25 Demo/Building-the-RShiny-App_files/cdl_colormap.csv")
   View(cdl_colormap)

data <- GetCDLData(aoi = '42', year = 2023, type = 'f')
data

terra::crs(data)
Pennsylvania <- rast(data)

levels(Pennsylvania) <- cdl_colormap[,c(1,6)]
plot(Pennsylvania)

#--manipulate legend to make it more legible--#

ggplot() +  # blank base plot
  geom_spatraster(data = Pennsylvania, aes(fill = class_name)) 

#_________________
Sp1Obs <- st_as_sf(Species1, 
                   coords=c("longitude","latitude"), #indicate the x and y columns
                   crs = 4326) #set the crs

PA = counties(state = "PA") %>% ##match lat/longs with county level info
  st_transform(crs=4326)

Sp1County = st_join(Sp1Obs, PA)
Sp1number_counties <- ((length(unique(Sp1County$NAME)))/67)*100 #(I manually put in 67, since PA has 67 counties. If you are working with a different state this value will need to be updated)
#__________________

Sp2Obs <- st_as_sf(Species2, 
                   coords=c("longitude","latitude"), #indicate the x and y columns
                   crs = 4326) #set the crs

Sp2County = st_join(Sp2Obs, PA)
Sp2number_counties <- ((length(unique(Sp2County$NAME)))/67)*100 #adjust the 67 if working in a different state

#---MAP INAT OBSERVATIONS ONTO THE PA RASTER LAYER----

Sp1CropInfo <- extract(Pennsylvania, Sp1Obs, cells=FALSE, method="simple")

Sp2CropInfo <- extract(Pennsylvania, Sp2Obs, cells=FALSE, method="simple")

plot_colour <- "#B5DB93"

theme <- create_theme(
  bs4dash_color(
    yellow = "#eddb68", #Customize color themes using Color Picker
    orange = "#e39e14",
    green = "#B5DB93"
  ),
  bs4dash_status(
    primary = "#f0eac7",
    info = "#E4E4E4"
  )
)

  ui <- dashboardPage(
    title = "iNat Observations",
    
    freshTheme = theme,
    dark = NULL,
    help = NULL,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    
    # Header ----
    
    header = dashboardHeader(
      status = "yellow",
      title = dashboardBrand(
        title = "iNat Observations",
        color = "orange",
        image = "https://images.unsplash.com/photo-1672532324606-896877df8065?q=80&w=1404&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
      )
    ),
       # Sidebar ----
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "sidebarMenuid",
        menuItem(
          "Home",
          tabName = "Home",
          icon = icon("home")
        ),
        menuItem(
          "Species1",
          tabName = "Species1",
          icon = icon("bar-chart")
        ),
        menuItem(
          "Species2",
          tabName = "Species2",
          icon = icon("bar-chart")
        )
        
      )
    ),
    
    # Control bar ----
    controlbar = dashboardControlbar(),
    
    # Footer ----
    footer = dashboardFooter(
      left = "Natalie Boyle", #insert your name in here!!
      right = "RShiny Demo Spring 2025"
    ),

    # Body ----
    body = dashboardBody(
      tabItems(
        
        # Home tab ----
        tabItem(
          tabName = "Home",
          
          jumbotron(
            title = "Welcome!",
            status = "info",
            lead = "Visualizing iNaturalist Observations Using an RShiny Framework",
            href = "https://www.inaturalist.org/",
            "Data freely available from iNaturalist.org"
          ),
          
          fluidRow(
            
            userBox(
              collapsible = FALSE,
              title = userDescription(
                title = "INSECT NET RShiny Workshop",
                subtitle = "March 25, 2025",
                image = "https://inaturalist-open-data.s3.amazonaws.com/photos/456249/medium.jpg",
                type = 1
              ),
              status = "orange",
              "Visit our website: insectnet.psu.edu"
            ),
            
            box(
              title = "Think about these questions as you customize your dashboard:",
              width = 6,
              collapsible = FALSE,
              blockQuote("How is the distribution of my two species of interest similar or different? When and where might I be mostly likely to encounter my species of interest? Where might there be biases in my data? What is a new figure I'd like to generate, informed by the data contained herein?", color = "purple")
            )
            
          )
          
        ),

                # Sp 1 tab ----
        tabItem(
          tabName = "Species1",
          
          ## Info boxes for sp 1 ----
          fluidRow(
            
            column(
              width = 4,
              infoBox(
                width = 12,
                title = "Total Observations",
                value = Sp1num_sightings,
                icon = icon("list"),
                color = "primary"
              )
            ),
            
            column(
              width = 4,
              infoBox(
                width = 12,
                value = Sp1over_years,
                title = "Year with most observations",
                icon = icon("bug"),
                color = "primary"
              )
            ),
            
            column(
              width = 4,
              infoBox(
                width = 12,
                value = format(round(Sp1number_counties, 0), nsmall = 0),
                title = "%age of PA counties with obs",
                icon = icon("location-dot"),
                color = "primary"
              )
            )
            
          ),
          
          ## Sortable boxes for Sp1----
          fluidRow(
            sortable(
              width = 6,
              
              box(
                title = "Observations by Landscape (2023 CDL Data)", 
                width = 12, 
                status = "orange",
                collapsible = FALSE, 
                
                plotlyOutput("Sp1plot_CDL")
              ),
              
              
              tabBox(
                id = "tabcard",
                title = "Observations Over Time",
                width = 12,
                status = "orange",
                solidHeader = TRUE,
                type = "tabs",
                tabPanel(
                  title = "By Month",
                  width = 12,
                  status = "orange",
                  
                  plotOutput("Sp1monthlyobs")
                ),
                tabPanel(
                  title = "By Year",
                  width = 12,
                  status = "orange",
                  
                  plotOutput("Sp1annualobs")
                )
              )
              
            ),
            
            sortable(
              width = 6,
              
              box(
                title = "Observations by Location",
                width = 12,  
                status = "orange",
                collapsible = FALSE,
                maximizable = TRUE,
                
                leafletOutput("Sp1plot_sightings_by_location")
                
              ),
              
              box(
                title = "Mason Bees", ##Customize for your chosen Species1 of interest
                width = 12,
                collapsible = FALSE,
                blockQuote("Mason bee is a name now commonly used for species of bees in the genus Osmia, of the family Megachilidae. Mason bees are named for their habit of using mud or other masonry products in constructing their nests, which are made in naturally occurring gaps such as between cracks in stones or other small dark cavities. When available, some species preferentially use hollow stems or holes in wood made by wood-boring insects.", color = "orange") ##Customize for your chosen Species1 of interest
              )
              
              
            )
          )
        ),
        
        #Sp 2 tab
        tabItem(
          tabName = "Species2",
          
          ## Info boxes ----
          fluidRow(
            
            column(
              width = 4,
              infoBox(
                width = 12,
                title = "Total Observations",
                value = Sp2num_sightings,
                icon = icon("list"),
                color = "primary"
              )
            ),
            
            column(
              width = 4,
              infoBox(
                width = 12,
                value = Sp2over_years,
                title = "Year with most observations",
                icon = icon("bug"),
                color = "primary"
              )
            ),
            
            column(
              width = 4,
              infoBox(
                width = 12,
                value = format(round(Sp2number_counties, 0), nsmall = 0),
                title = "%age of PA counties with obs",
                icon = icon("location-dot"),
                color = "primary"
              )
            )
            
          ),
          
          ## Sortable boxes ----
          fluidRow(
            sortable(
              width = 6,
              
              box(
                title = "Observations by Landscape (2023 CDL Data)", 
                width = 12, 
                status = "orange",
                collapsible = FALSE, 
                
                plotlyOutput("Sp2plot_CDL")
              ),
              
              
              tabBox(
                id = "tabcard",
                title = "Observations Over Time",
                width = 12,
                status = "orange",
                solidHeader = TRUE,
                type = "tabs",
                tabPanel(
                  title = "By Month",
                  width = 12,
                  status = "orange",
                  
                  plotOutput("Sp2monthlyobs")
                ),
                tabPanel(
                  title = "By Year",
                  width = 12,
                  status = "orange",
                  
                  plotOutput("Sp2annualobs")
                )
              )
              
            ),
            
            sortable(
              width = 6,
              
              box(
                title = "Observations by Location",
                width = 12,  
                status = "orange",
                collapsible = FALSE,
                maximizable = TRUE,
                
                leafletOutput("Sp2plot_sightings_by_location")
                
              ),
              
              box(
                title = "Carpenter Bees",
                width = 12,
                collapsible = FALSE,
                blockQuote("Carpenter bees are species in the genus Xylocopa of the subfamily Xylocopinae. The genus includes some 500 bees in 31 subgenera. The common name carpenter bee derives from their nesting behavior; nearly all species burrow into hard plant material such as dead wood or bamboo. The main exceptions are species in the subgenus Proxylocopa, which dig nesting tunnels in suitable soil.", color = "orange")
              )
            )
          )
        )
      )
    ))
  
  server <- function(input, output) {
    
    #------- Getting all data in a readable format for RShiny-------------# 
    #Number of Moths observations in each CDL landscape type bar chart
    
    output$Sp1plot_CDL <- renderPlotly({
      
      Sp1CropInfo %>% 
        ggplot(aes(x = class_name)) + 
        geom_bar(fill = "purple") + 
        labs(
          x = ""
        ) + 
        theme_bw() + 
        coord_flip()
    })
    
    output$Sp2plot_CDL <- renderPlotly({
      
      Sp2CropInfo %>% 
        ggplot(aes(x = class_name)) + 
        geom_bar(fill = "purple") + 
        labs(
          x = ""
        ) + 
        theme_bw() + 
        coord_flip()
    })
    
  output$Sp1monthlyobs <- renderPlot({hist(Sp1month)}) #Observations by month
  
  output$Sp1annualobs <- renderPlot({hist(Sp1year)}) #Observations by year
  
  output$Sp2monthlyobs <- renderPlot({hist(Sp2month)}) #Observations by month
  
  output$Sp2annualobs <- renderPlot({hist(Sp2year)}) #Observations by year
  
    
    #Interactive map of observations
    output$Sp1plot_sightings_by_location <- renderLeaflet({
      
      
      leaflet(data = Species1) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addCircleMarkers(
          ~longitude,
          ~latitude,
          radius = 3,
          color = plot_colour,
          fillOpacity = 1,
          popup = ~paste0("Date of observation: ", observed_on)
        )
    })
    
    output$Sp2plot_sightings_by_location <- renderLeaflet({
      
      
      leaflet(data = Species2) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addCircleMarkers(
          ~longitude,
          ~latitude,
          radius = 3,
          color = plot_colour,
          fillOpacity = 1,
          popup = ~paste0("Date of observation: ", observed_on)
        )
    })
    
  }
  
  shinyApp(ui, server)


