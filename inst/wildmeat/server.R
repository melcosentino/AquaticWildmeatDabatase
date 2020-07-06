## This app was developed with R version 
# And R Studio 1.0.136

library(wppExplorer)
library(reshape2)
library(googleVis)
library(gsubfn)
library(proto)
library(plyr)
library(ggplot2)
library(RSQLite)
library(sqldf)
library(rdrop2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(car)
library(plotly)
library(grid)
library(gridExtra)
#library(DT)

### to publish in shinyapps.io
##setwd('C:/MEL/Shiny/bushmeat/inst/wildmeat')
#library(rsconnect)
#rsconnect::deployApp(server = "shinyapps.io")

#### the app ###
shinyServer(function(input, output, session) {

  # Create the textGrobs 
  Text1 = textGrob(paste("Largest x-value is"))
  
#downloads database into computer from dropbox
  db <- dbConnect(SQLite(), dbname = "bushmeat.db")

  auth <- dbGetQuery(db, "SELECT Author from Ref")
  tit <- dbGetQuery(db, "SELECT Title from Ref")
  jour <- dbGetQuery(db, "SELECT Journal from Ref")
  yr <- dbGetQuery(db, "SELECT Year from Ref")
  RefTable <- data.frame(auth, tit, jour, yr)#, yr)#, yr)
  output$RefTableRender <- DT::renderDataTable(RefTable)
  
  ### make some fields mandatory for single records
  observe({
    fieldsMandatSR <- c("countryName", "uses", "yourname", "email", "type", "origin", "number", "yeardata", "owner", "trade", "reference")
    # check if all mandatory fields have a value
    mandatoryFilledSR <-
      vapply(fieldsMandatSR,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilledSR <- all(mandatoryFilledSR)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilledSR)
  })
  
  observeEvent(input$submit, {
    
    # add a pop up message for people to wait
    showModal(modalDialog(
      title = HTML("<center><b>THANK YOU!</b></center>"),
      HTML("<center>Thank you for your submission! Your data is very important to us.
      Please ensure the submission form is empty before leaving the page.
      Your submission has been added to the database. A moderator will revise the record and contact you if required.</center>"),
      easyClose = TRUE,
      footer = NULL
    ))
 
  AddSR <- paste0("INSERT INTO SingleRecords (Latitude, Longitude, Area, City, County, CountryCode, Country, 
                Continent, Day, Month, Year_Data, SpeciesCommName, SciCode, SpeciesName, Genus, Family, Order1, Class, 
                Sex, Age, Number, Origin, Parts, Use, Trade, Country_Dest, Type, Owner, 
                Reference, Notes, Contributor_name, Email) 
                VALUES('", input$lat, "' , '", input$longit, "' , '", input$areas, "' , '", input$city, "' , 
                '", input$county, "' , '", substr(input$countryName, 1, 2), "' ,  '", input$countryName, "' , '", input$continent, "' , 
                '", input$day, "' , '", input$month, "' , '", input$yeardata, "' , '", input$species, "' , '", substr(input$sciname, 1, 3), "' , 
                '", input$sciname, "' , '", input$genus, "' , '", input$family, "' , 
                '", input$order1, "' , '", input$classe, "' , '", input$sex, "' , '", input$age, "', 
                '", input$number, "' , '", input$origin, "' , '", input$parts, "' , '", input$uses, "' , 
                '", input$trade, "' , '", substr(input$countrydest, 1, 2), "' , 
                '", input$type, "' , '", input$owner, "' , 
                '", input$reference,"' , '", input$note1,"' , '", input$yourname,"' , '", input$email,"')" )
    ## fill the form in the database ##
     dbExecute(db, AddSR) # updates single records table
    
     # updates tables Bycatch, Hunts, Stranding or Unknown - IT WORKS (puts the number, it doesn't sum)
     UpdTable <- paste0("UPDATE '", input$origin, "'
                        SET '", substr(input$countryName, 1, 2), "' = (SELECT '", substr(input$countryName, 1, 2), "' FROM '", input$origin, "'
                                                                      WHERE year = '", input$yeardata, "') + ", input$number, "
                        WHERE year = '", input$yeardata, "';")
     dbExecute(db, UpdTable) #updates table Records
     
     # updates Records --- IT WORKS
     UpdTable <- paste0("UPDATE 'Records'
                      SET '", substr(input$countryName, 1, 2), "' = (SELECT '", substr(input$countryName, 1, 2), "' FROM 'Records'
                                                                      WHERE year = '", input$yeardata, "') + 1
                      WHERE year = '", input$yeardata, "'")
     dbExecute(db, UpdTable) #updates table Records
     
     UpdTot <- paste0("UPDATE 'Total'
                      SET '", substr(input$countryName, 1, 2), "' = (SELECT '", substr(input$countryName, 1, 2), "' FROM 'Total'
                                                                      WHERE year = '", input$yeardata, "') + 1
                      WHERE year = '", input$yeardata, "'")
     dbExecute(db, UpdTot) #updates table Records
     
      #updates table Countries, columns of origin
     UpdCnt2 <- paste0("UPDATE 'Countries' 
                      SET '", input$origin, "' = (SELECT '", input$origin , "' FROM 'Countries'
                                                  WHERE Code = '", substr(input$countryName, 1, 2), "') + ", input$number, "
                      WHERE Code = '", substr(input$countryName, 1, 2), "';")
     dbExecute(db, UpdCnt2) #updates table Countries, origin columns
      
     #updates table Countries, Records column
     UpdCntR <- paste0("UPDATE 'Countries'
                      SET 'Records' = Records + 1
                      WHERE Code = '", substr(input$countryName, 1, 2), "';")
     dbExecute(db, UpdCntR) #updates table Records
     #updates table Countries, Records column
     UpdCntT <- paste0("UPDATE 'Countries'
                      SET 'Total' = Total + ", input$number, "
                      WHERE Code = '", substr(input$countryName, 1, 2), "';")
     dbExecute(db, UpdCntT) #updates table Records

          
    # updates the Species per Country table
     UdSpCount <- paste0("UPDATE 'SpeciesPerCountry' 
                         SET ", substr(input$countryName, 1, 2), " = ", substr(input$countryName, 1, 2), " + ", input$number, " 
                         WHERE SciCode = '", substr(input$sciname, 1, 3), "';") #it works
     dbExecute(db, UdSpCount) # updates species per country table
     UdSpCount <- paste0("UPDATE 'SpeciesPerCountry' 
                         SET Total = Total + ", input$number, " 
                         WHERE SciCode = '", substr(input$sciname, 1, 3), "';") #it works
     dbExecute(db, UdSpCount) # updates species per country table

    # update Species And Country   

     UdSpAndCount <- paste0("UPDATE 'SpeciesAndCountry' 
                         SET ", input$origin, " = ", input$origin, " + ", input$number, " 
                         WHERE SciCode = '", substr(input$sciname, 1, 3), "'AND CountryCode = '", substr(input$countryName, 1, 2), "';") #it works
     dbExecute(db, UdSpAndCount) # updates species per country table

     if (all(input$uses == "Food") | all(input$uses == "Bait") | all(input$uses == "Ritual") ){
       UdSpAndCount1 <- paste0("UPDATE 'SpeciesAndCountry' 
                         SET ", input$uses, " = ", input$uses, " + ", input$number, " 
                              WHERE SciCode = '", substr(input$sciname, 1, 3), "'AND CountryCode = '", substr(input$countryName, 1, 2), "';") #it works
       dbExecute(db, UdSpAndCount1) # updates species per country table
       
     } else if (all(input$uses == "Unknown")) {
       UdSpAndCount <- paste0("UPDATE 'SpeciesAndCountry' 
                         SET Unknown1 = Unknown1 + ", input$number, " 
                         WHERE SciCode = '", substr(input$sciname, 1, 3), "'AND CountryCode = '", substr(input$countryName, 1, 2), "';") #it works
       dbExecute(db, UdSpAndCount) # updates species per country table
     } else {
       UdSpAndCount <- paste0("UPDATE 'SpeciesAndCountry' 
                         SET Other = Other + ", input$number, " 
                              WHERE SciCode = '", substr(input$sciname, 1, 3), "'AND CountryCode = '", substr(input$countryName, 1, 2), "';") #it works
       dbExecute(db, UdSpAndCount) # updates species per country table
     }
     
      
## uploads database back in dropbox ##
    drop_upload("bushmeat.db")
  
    #empty form
    updateTextInput(session, "lat", "Latitude", NA) 
    updateTextInput(session, "longit", "Longitude", NA) 
    updateTextInput(session, "areas", "Area", NA) 
    updateTextInput(session, "city", "City", NA) 
    updateTextInput(session, "county", "County", NA)
    updateTextInput(session, "countryName", "Country", NA)
    updateTextInput(session, "continent", "Continent", NA)
    # EVENTS
    updateTextInput(session, "day", "Day", NA) 
    updateTextInput(session, "month", "Month", NA) 
    updateTextInput(session, "yeardata", "Year", NA)
    
    updateTextInput(session, "species", "Species - Common name", NA)
    updateTextInput(session, "sciname", "Species - Scientific Name", NA)
    updateTextInput(session, "genus", "Genus", NA)
    updateTextInput(session, "family", "Family", NA)
    updateTextInput(session, "order1", "Order", NA)
    updateTextInput(session, "classe", "Class", NA)
    
    updateTextInput(session, "sex", "Sex", NA)
    updateTextInput(session, "age", "Age class", NA)
    updateTextInput(session, "number", "Number of animals caught/used", NA)
    updateTextInput(session, "origin", "How was the animal obtained?", NA)
    # UTILIZATION
    updateTextInput(session, "parts", "What were the parts used?", NA)
    updateTextInput(session, "uses", "What was it used for?", NA)
    updateTextInput(session, "trade", "Trade", NA)
    updateTextInput(session, "countrydest", "Country of Destination", NA)
    # REFERENCES
    updateTextInput(session, "type", "Data type", NA)
    updateTextInput(session, "owner", "Data owner", NA)
    updateTextInput(session, "reference", "Write reference/link", NA)
    updateTextInput(session, "note1", "Notes", NA)

    updateTextInput(session, "yourname", "Name", NA)
    updateTextInput(session, "email", "Email", NA)

    })
  
  ### contact person information - form submit ## (NOT READY YET)
  observeEvent(input$submit2, {
    # add a pop up message for people to wait
    showModal(modalDialog(
      title = HTML("<center><b>THANK YOU!</b></center>"),
      HTML("<center>Thank you for your submission! The details will be used to create a database of stakeholders working on aquatic
           wild meat.</center>"),
      easyClose = TRUE,
      footer = NULL
      ))
    #drop_dir()
    db <- dbConnect(SQLite(), dbname="bushmeat.db")
     # remove commas
    surname1 <- input$surname 
    surname1 <- gsub(", ", "_", surname1) #replaces comma for an underscore
    yourname3 <- input$yourname2 
    yourname3 <- gsub(", ", "_", yourname3) #replaces comma for an underscore
    affiliation2 <- input$affiliation
    affiliation2 <- gsub(", ", "_", affiliation2) #replaces comma for an underscore
    address2 <- input$address
    address2 <- gsub(", ", "_", address2) #replaces comma for an underscore
    postcode2 <- input$postcode 
    postcode2 <- gsub(", ", "_", postcode2) #replaces comma for an underscore
    yourcountry2 <- input$yourcountry
    yourcountry2 <- gsub(", ", "_", yourcountry2) #replaces comma for an underscore
    email3 <- input$email2
    email3 <- gsub(", ", "_", email3) #replaces comma for an underscore
    phone2 <- input$phone
    phone2 <- gsub(", ", "_", phone2) #replaces comma for an underscore
    note3 <- input$note
    note3 <- gsub(", ", "_", note3) #replaces comma for an underscore

    queryFormSR <- paste0("INSERT INTO ContactPerson (Surname, Name, Affiliation, Address, 
            PostCode, Country, Email, Phone, Notes) 
                          VALUES('", surname1, "' , '", yourname3, 
                          "' , '", affiliation2, "' , '", address2, "' , '", postcode2, 
                          "' , '", yourcountry2, "' , '", email3, "' , '", phone2, "' , '", note3, "')" )
    ## fill the form in the database ##
    dbExecute(db, queryFormSR)
     ## upload database in dropbox ##
    drop_upload("bushmeat.db")
    updateTextInput(session, "surname", "Surname", NA) 
    updateTextInput(session, "yourname2", "Name", NA)
    updateTextInput(session, "affiliation", "Affiliation", NA)
    updateTextInput(session, "address", "Address", NA)
    updateTextInput(session, "postcode", "Post Code", NA)
    updateTextInput(session, "yourcountry", "Country", NA)
    updateTextInput(session, "email2", "Email", NA)
    updateTextInput(session, "phone", "Phone", NA)
    updateTextInput(session, "note", "Notes", NA)
  })
  
  observeEvent(input$imageId, {
    filename2 <- input$imageId
    #if(is.null(filename)){return()} 
    drop_upload(filename2$datapath, path = filename2$name, mode = "add", autorename = TRUE)
    
  })
  
  myData <- dbGetQuery(db, "SELECT * from Countries")
  output$gvis <- renderGvis({
    gvisGeoChart(myData,
                 locationvar="Code", colorvar=input$my_indicator, #changed Country for x___Code
                 options=list(displayMode='regions', 
                              width=750, height="auto",
                              colorAxis= "{colors: ['#8bbfbd', '#c12a41']}"
                              #"{values: [0, 10, 50, 100, 1000, 10000], 
                              #colors:['#c0dbda', '#8bbfbd', '#728eaf', '#5a5191', '#56367c', '#872d70', '#c12a41']}" 
                               ))     
  }) #,
  

  output$trendGraph <- renderPlot(
    height = "auto", width = "auto",
    {
    yearsTrend <- as.numeric(1945:2022)
    CodeCountry <- substr(input$graphCountry, 1, 2)
    UpdCntr <- paste0 ('SELECT "',  CodeCountry,'" FROM ', input$graphInd, "")
    countryTrend <- dbGetQuery(db, UpdCntr)
    countryTrend <- as.integer(countryTrend[,1])#[1]
    max_val <- max(countryTrend, na.rm = TRUE)
       ggplot(NULL, aes(x=yearsTrend, y=countryTrend)) +
         theme(panel.background = element_rect(fill = "#F5F5F5")) +
         geom_point(shape=21, fill="blue", color="blue", size=1) +    # Use hollow circles
         geom_smooth(method = "loess")+
         theme(plot.title = element_text(lineheight=1.5, face="bold", size = 18))+
         theme(plot.title = element_text(hjust = 0.5))+
         theme(axis.title.x = element_text(hjust = 0.9))+
         theme(axis.title.y = element_text(hjust = 0.95))+
         labs(x = "\nYear")+
         labs(y = input$graphInd,vjust = 1)+
         theme(axis.text.x=element_text(size=11, colour = "#337AB7"), 
               axis.text.y=element_text(size=11, colour = "#337AB7"),
               axis.title=element_text(size=16,face="bold")) +
         theme(plot.caption=element_text(face = NULL, hjust = 0.5, size = 14)) 
                    })
  
  output$SpNameOut <- renderImage({
    CDSp = substr(input$SpNameInp, 1, 3)  
    filename = normalizePath(file.path('./www',
            paste(CDSp, '.png', sep='')))
    list(src = filename, 
         width = 240,
         height = 180)
  },deleteFile = FALSE)
  
  output$SpNameTxt <- renderText({
      CodeSp = substr(input$SpNameInp, 1, 3)  
      filename1 = normalizePath(file.path('./www',
              paste(CodeSp, '.html', sep='')))
      includeHTML(filename1)
    })
 
  output$TrendsTxt <- renderText(
    {
      includeHTML("TRENDS.html")
    })
  output$TablesTxt <- renderText(
    {
      includeHTML("TABLES.html")
    })

  output$CountryTable <- renderTable({ 
      CDCountry <- substr(input$graphCountry, 1, 2)
      hunt1 = paste0('SELECT Hunts FROM Countries WHERE Code = "', CDCountry,'"')
      hunt <- dbGetQuery(db, hunt1)
      hunt <- as.integer(hunt[,1])#[1]
      bycat1 = paste0('SELECT Bycatch FROM Countries WHERE Code = "', CDCountry,'"') 
      bycat <- dbGetQuery(db, bycat1)
      bycat <- as.integer(bycat[,1])#[1]
      strand1 = paste0('SELECT Stranding FROM Countries WHERE Code = "', CDCountry,'"')
      strand <- dbGetQuery(db, strand1)
      strand <- as.integer(strand[,1])#[1]
      unkno1 = paste0('SELECT Unknown FROM Countries WHERE Code = "', CDCountry,'"')
      unkno <- dbGetQuery(db, unkno1)
      unkno <- as.integer(unkno[,1])#[1]
      recor1 = paste0('SELECT Records FROM Countries WHERE Code = "', CDCountry,'"')
      recor <- dbGetQuery(db, recor1)
      recor <- as.integer(recor[,1])#[1]
      tot1 = paste0('SELECT Total FROM Countries WHERE Code = "', CDCountry,'"')
      tot <- dbGetQuery(db, tot1)
      tot <- as.integer(tot[,1])#[1]
      CountryInfo <- matrix(c(hunt,bycat,strand,unkno,recor,tot),ncol=1,byrow=TRUE)
      colnames(CountryInfo) <- c("Total")
      rownames(CountryInfo) <- c("Hunts","Bycatch","Stranding", "Unknown", "Records", "Total")
      head(CountryInfo, n = 6 )},  
            striped = TRUE,  
            hover = TRUE,
            width = '100%',
            rownames = TRUE)  
    
output$SpCTable <- renderTable({ 
      CDSpecies <- substr(input$sciname, 1, 3)
      sp1 = paste0('SELECT EntName,"',substr(input$graphCountry, 1, 2),'" FROM SpeciesPerCountry WHERE "',substr(input$graphCountry, 1, 2),'" != "0" ')
      sp <- dbGetQuery(db, sp1)
      colnames(sp) <- c("Species", "Total")
      head(sp, n = 79 )},  
      striped = TRUE,  
      hover = TRUE,
      width = '100%',
      rownames = FALSE
      ) 

output$CSpTable <- renderTable({ # table for each species
  CSp <- substr(input$SpNameInp, 1, 3)
  CT1 = paste0('SELECT country, hunts, stranding, bycatch, unknown, food, bait, ritual, other, unknown1 
               FROM SpeciesAndCountry 
               WHERE scicode = "', CSp ,'" 
                  AND (hunts != "0"
                        OR stranding != "0" OR bycatch != "0" OR unknown != "0"
                        OR food != "0" OR bait!= "0" OR ritual != "0" OR other != "0" OR unknown1 != "0") ')
  CT <- dbGetQuery(db, CT1)
  colnames(CT) <- c("Country", "Hunt", "Strand", "Byc", "Unk", "Food", "Bait", "Ritual", "Other", "Unk")
  head(CT, n = 79)},  
    striped = TRUE,  
    hover = TRUE,  
    width = '100%',
    rownames = FALSE
          )  
output$PageUpdates <- renderTable({ # table for each species
  Tot1 = paste0('SELECT SUM(Records) FROM Countries')
  TotRec <- dbGetQuery(db, Tot1)
 
  Coun1 = paste0('SELECT COUNT(Records) FROM Countries WHERE Records != "0"')
  TotC <- dbGetQuery(db, Coun1)
  
  Spec1 = paste0('SELECT COUNT(Total) FROM SpeciesPerCountry WHERE Total != "0"')
  TotSp <- dbGetQuery(db, Spec1)
 
  Totals <- matrix(c(TotRec,TotC,TotSp),ncol=1,byrow=TRUE)
  rownames(Totals) <- c("Records","Countries","Species")
  colnames(Totals) <- c("Total")

  head(Totals, n = 3 )},  
  striped = TRUE,  
  hover = TRUE,  
  digits = 0,
  width = '100%',  
  rownames = TRUE
) 

})
