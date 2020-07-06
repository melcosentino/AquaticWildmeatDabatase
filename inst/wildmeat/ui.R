library(rdrop2)
library(RSQLite)
# access database in dropbox
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
drop_download('bushmeat.db', overwrite = TRUE)
db <- dbConnect(SQLite(), dbname = "bushmeat.db")
# country list to select code
myList1 <- dbGetQuery(db, "SELECT Country from Countries")
country_code <- dbGetQuery(db, "SELECT Code from Countries")
myList <- data.frame(myList1, country_code)
#species list, to select code 
#mySpecies <- as.list (dbGetQuery(db, "SELECT Scientificname from SpeciesPerCountry"))

SpList <- dbGetQuery(db, "SELECT EntName from SpeciesPerCountry")
#SpN <- dbGetQuery(db, "SELECT Commonname from SpeciesPerCountry")
#ScN <- dbGetQuery(db, "SELECT Scientificname from SpeciesPerCountry")
#SpList <- data.frame(CodeSp, SpN, ScN)


#loads map
geochartPrereqs <- tagList(
  tags$head(
    # hr(),
    tags$script(src="https://www.google.com/jsapi"),
    tags$script(src="geochart.js")
  )
)

row <- function(...) {
  tags$div(class="row", ...) }

col <- function(width, ...) {
  tags$div(class=paste0("col-sm-", width), ...) }


fluidPage(
  tags$head( ## colour and font type of the title of the page
    tags$style(HTML("
       @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
       h1 {
       font-family: 'Lobster';
       font-weight: 500;
       line-height: 1.1;
       color: #337AB7;
       }
        "))
    ),
  
  headerPanel(HTML(paste("Aquatic Wild Meat Database", "<h4>Global database of aquatic mammal utilisation</h4>", sep = "<br/>")), 
              "Aquatic Wild Meat Database"),
  mainPanel(width = 12,
    tabsetPanel(     #creates the different tabs
      tabPanel('Map',
       pageWithSidebar(
         titlePanel(""),
         sidebarPanel(width = 3,
             #br(), br(),
             HTML("<b>Please choose an indicator</b>"),
             helpText("Hunts, Bycatch, Stranding, and Unknown refer to how animals were obtained. Records indicates the total number of records."),
             selectInput('my_indicator', "", choices = c("Bycatch", "Hunts", "Stranding", "Unknown", "Records", "Total")),
             hr(), 
             br(), 
             paste("Currently the database has:"),
             br(),
             tableOutput("PageUpdates"),
             br(), br(), #br(), 
             hr(),
             tags$img(height = 100, width = 100, src = "CSILOGO.png"),
             br(), br(), #br(),
             paste("Acknowledgements: Cetacean Society International for funding the development of this site."),
             hr(),
             paste("Site managed by Mel Cosentino"),
             br()
             
                   ),  #sidebarPanel
         mainPanel(width = 7, 
             htmlOutput("gvis")
                   )
                      ) #pageWithSidebar
             ), #tabpanel Map
      tabPanel('Per Country',
             #  col(0.5 
            #     ),
            #   col(8, 
            pageWithSidebar(
                 titlePanel(""),
                 sidebarPanel(width = 3,
                      br(),
                      selectInput('graphCountry', "Select country", choices = myList1),
                      hr(),
                      HTML("<b>Please choose an indicator</b>"),
                      helpText("Hunts, Bycatch, Stranding, and Unknown refer to how animals were obtained. Records indicates the total number of records."),
                      selectInput('graphInd', "", choices = c("Hunts", "Stranding", "Bycatch", "Unknown", "Records", "Total")),
                      hr(), br(), br(), br(), br(),br(), br(),br(), br(),br(), br(),
                      tags$img(height = 100, width = 100, src = "CSILOGO.png"),
                      br(), br(),
                      paste("Site managed by Mel Cosentino")
                            ),  #sidebarPanel
                 mainPanel(width = 7, 
                  row(
                   # trends of the country
                    htmlOutput("TrendsTxt"),
                    #hr(),
                    plotOutput('trendGraph', width = "100%"),
                    hr()
                    #tableOutput('CountryTable'),
                    #tableOutput('SpCTable')
                      ), # end row         
                  row(
                    htmlOutput("TablesTxt"),
                    col(4, 
                        tableOutput('CountryTable')),
                    col(0.5
                        #nothing
                        ),
                    col(8, 
                      tableOutput('SpCTable'))
                    
                    
                  ) # end row                         
                 ) #mainPanel
               ) #PageWithSidebar
                #) # col
      ), #tabpanel ends
      tabPanel('Per Species',
               tags$div(
                 #class = "container",
                 row(
                   col(1, ''),
                   col(8, 
                       row(
                         wellPanel(
                           selectInput("SpNameInp", "Please choose a species", choices = SpList)
                         )
                       ), #row ends
                       row(
                         col(0.5, ''),
                         col(4,# create a table for that particular country
                             #tableOutput('CountryTable')
                             #'table per country'
                             br(), br(),
                             #SpName,
                             imageOutput("SpNameOut"), 
                             br(), br()
                         ),
                         col(0.9, ''),
                         col(8, # create a table for that particular country
                             htmlOutput("SpNameTxt"),
                             hr(),
                             includeHTML("TABLESp.html"),
                             tableOutput("CSpTable")
                         ) # Col ends
                       ) #row ends
                   ) #col ends
                 ) #row end
               ) #tag$div end
      ), #tabpanel end Species 
    tabPanel('Forms',
             tags$div(
               row(
                 col(1, ''),
                 col(8, tabsetPanel(
                   tabPanel('Single Records',
                            br(),
                            helpText("Please follow the instructions and complete as many fields as possible."),
                            helpText(HTML("<font color='red'>*</font> indicates mandatory fields")),
                            wellPanel( h4("LOCATION"),
                                        #helpText("Please insert here information about where the event occurred."),
                                        textInput("lat", "Latitude", "", placeholder = ""),
                                        textInput("longit", "Longitude", "", placeholder = ""),
                                        textInput("areas", "Area", "", placeholder = "Area (e.g., beach name"),
                                        textInput("city", "City", "", placeholder = "City or closest city"),
                                        textInput("county", "State", "", placeholder = "State/Province/County"),
                                        selectInput("countryName", HTML("Country <font color='red'>*</font>"), choices = c("", myList$Country)), 
                                        textInput("continent", "Continent", "", placeholder = "Continent"),                                        
                                        br(),
                                        style = "padding: 5px;"
                            ),
                            wellPanel( h4("EVENT"),
                                       #helpText("'Use' refers to ...and include additional information in the 'Notes' section below."),
                                       textInput("day", "Day of the month", "", placeholder = ""),
                                       selectInput("month", "Month of the year", "", choices = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec")),
                                       numericInput("yeardata", HTML("Year of the event <font color='red'>*</font>"), ""),

                                       textInput("species", "Species - common name (in English or otherwise)", ""),#, placeholder = "Please use the local common name or the common name in English"), 
                                       selectInput("sciname", "Full name - Please select from the list provided", choices = c("", SpList)),
                                       textInput("genus", "Genus", "" , placeholder = "Genus - Complete only if species is unknown - (e.g., Globicephala)"),
                                       textInput("family", "Family", "", placeholder = "Family - Complete only if species is unknown - (e.g, Delphinidae)"),
                                       textInput("order1", "Order", "", placeholder = "Order - Complete only if species is unknown - (e.g., Cetacea)"),
                                       textInput("classe", "Class", "", placeholder = "Class - Complete only if species is unknown - (e.g., Mammalia)"),

                                       selectInput("sex", "Sex of the animals caught/used", "", choices = c("", "Female", "Male", "Unknown")),
                                       selectInput("age", "Age class of the animals caught/used", "", choices = c("", "Calf", "Juvenile", "Adult", "Unknown")),
                                       numericInput("number", HTML("Number of animals caught/used <font color='red'>*</font>"), ""),
                                       selectInput("origin", HTML("How was the animal obtained? <font color='red'>*</font>"), 
                                                   choices = c("", "Bycatch", "Hunts", "Stranding", "Unknown")), #select or write.
                                       style = "padding: 5px;"
                            ),
                            wellPanel( h4("UTILISATION"),
                                       selectInput("parts", HTML("Which parts of the animals were used? <font color='red'>*</font>"), 
                                                   choices = c("", "All", "Blubber", "Bones", "Eyes", "Fins", "Flesh", "Genitals", "Head", "Skin", "Tail",
                                                               "Other", "Unknown")),
                                       selectInput("uses", HTML("Main use <font color='red'>*</font>"), 
                                                  choices = c("", "Amulets", "Bait", "Food", "Medicine", "Religious purposes", "Rituals", "Other", "Unknown")), 
                                       selectInput("trade", HTML("Trade<font color='red'>*</font> - If there was international trade, please also select country of origin and destination"), choices = c("", "No", "Local", "Regional", "National", "International", "Unknown")),
                                       selectInput("countrydest", "Country of Destination", "", choices = c("", myList$Country)), 
                                       style = "padding: 5px;" 
                            ),
                            wellPanel( h4 ("REFERENCES"),
                                       helpText("Please insert information about how the event you are reporting (e.g., online news, systematic surveys). If possible provide a link or a reference."),
                                       selectInput("type", HTML("Source <font color='red'>*</font>"), "", choices = c("", "Conference paper", "Online news", "Opportunistic observation", "Published scientific paper", 
                                                  "Published report", "Survey", "Unpublished report",  "Other (Please explain in 'Notes')")),
                                       textAreaInput("reference", HTML("Reference<font color='red'>*</font>"), "", placeholder = "If the event was reported elsewhere please insert a link or a reference in a format similar to: Cosentino AM and Fisher S. 2016. The Utilization of Aquatic Bushmeat from Small Cetaceans and Manatees in South America and West Africa, Frontiers in Marine Science, pp 1-8.", height = 100), 
                                       fileInput("imageId", "Upload images or any supporting documents", multiple = TRUE),
                                       textAreaInput("note1", "Notes", "", height = 110, placeholder = "Please include any additional information that you think will help us"),
                                       style = "padding: 5px;"
                            ),
                            wellPanel( h4("CONTACT DETAILS"),
                                       helpText("Please provide your contact details. They will only be used if more information is required. 
                                       They will not be shared with third parties."),
                                       textInput("yourname", HTML("Name <font color='red'>*</font>"), "", placeholder = "Format: Name, SURNAME. 
                                                 Eg. Mel COSENTINO"),
                                       textInput("email", HTML("Email <font color='red'>*</font>"),"", placeholder = "Enter your email address. 
                                                 Make sure it is spelled properly"),
                                       br(),
                                       helpText("Please state who owns the data provided. If the owner is a person please provide name and surname as Mel COSENTINO. 
                                                If it is an organisation, please provide the full name of the organisation and the acronym E.g. Wild Earth Foundation (WEF)"),
                                       textInput("owner", HTML("Ownership <font color='red'>*</font>"), "", placeholder = ""),
                                       actionButton("submit", "SUBMIT", class="btn-primary")
                            )
                                       ),
                    tabPanel('Contact person',
                             br(),
                             helpText("Please provide your contact details or those of someone you know works with wild meat or can occasionally encounter wild meat utilisation. These details will be used to create a database of stakeholders and will not be shared with third parties."),
                             wellPanel( h4("PERSONAL DETAILS"),
                                        textInput("surname", "Surname", ""),
                                        textInput("yourname2", "Name", ""),
                                        textInput("affiliation", "Affiliation", ""),
                                        textInput("address", "Address", ""),
                                        textInput("postcode", "Post Code", ""),
                                        selectInput("yourcountry", "Country", "", choices = c("", myList$Country)),
                                        textInput("email2", "Email",""),
                                        textInput("phone", "Phone",""),
                                        textAreaInput("note", "Notes", "", height = 110, placeholder = "Please include any additional information you believe is relevant"),
                                        br(),
                                        br(),
                                        actionButton("submit2", "SUBMIT", class="btn-primary"),
                                        style = "padding: 5px;"             
                                        ) # end wellPanel Personal Details
                    ), # end tabPanel Contact Person
                   tabPanel('Ancient Records',
                            br(), br(), 
                   "Page under Construction"
                   ) #,
                  ) # end Single Records
                 )
               )
          )
    ),
    tabPanel("About this project",
             tags$div(
               #class = "container",
               row(
                 col(1, ''),
                 col(8, 
                     includeHTML("ABOUT_SITE.html")
                 ) #col end
               ) #row end
             ) #tagd$div end
    ), #tabpanel end 
    tabPanel("References",
             tags$div(
               #class = "container",
               row(
                 col(2, ''),
                 col(8,
                     includeHTML("References.html"))   
                 ),
               row(
                 col(2, ''),
                 col(8, 
                 DT::dataTableOutput('RefTableRender')
                     # includeHTML("ABOUT_SITE.html")) 
                 )#col end
               ) #row end
             ) #tagd$div end
    ),
    tabPanel("Contributors",
             tags$div(
               #class = "container",
               row(
                 col(1, ''),
                 col(8, 
                     br(), br(), 
                     #"Page under Construction"
                     includeHTML("CONTRIBUTORS.html")
                 ) #col end
               ) #row end
             ) #tagd$div end
    ), #tabpanel end 
    
    tabPanel("About us",
             tags$div(
               #class = "container",
               row(
                 col(0.5, ''),
                 col(2,
                     br(), br(), # br(), # br(),
                     tags$img(height = 240, width = 180, src = "Mel.jpg"), 
                     br() # br() # br(), br(), br() #,
                      ),
                 col(0.9, ''),
                 col(8, 
                     #"Under construction"
                     includeHTML("MEL.html")
                 ) #col end
                 ), #row end
               
               row(
                 col(0.5, ''),
                 col(2,
                     br(), # br(), br(), # br(), br(),
                     tags$img(height = 240, width = 180, src = "Lucre.jpg"),
                    # br(), br(), br(), br(), br(), br(), br(),
                     br()
                 ),
                 col(0.9, ''),
                 col(8, 
                     #"Under construction"
                     includeHTML("LUCRE.html")
                 ) #col end
               ) 
             ) #tagd$div end
    ) #tabpanel end 
  ) #end tabsetPanel
) #end mainPanel
) #end fluidpage

