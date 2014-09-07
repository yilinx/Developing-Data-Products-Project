library(shiny)

#download Child care statistics
#downloadurl <- "http://data.gov.sg/Agency_Data/MSF/0608070000000013937A.xlsx"

#setInternet2(TRUE)
#if(file.exists("dataset")==F){
#        dir.create("dataset")  #check if "dataset" folder exist. If doesnt, #create it.
#}
#download.file(downloadurl, "dataset/childcare_unedited.xlsx")

#downloaded excel spreadsheet cannnot be processed by R, need to use excel to reformat it.

#read data
library(xlsx)
library(ggplot2)
CCdata <- read.xlsx("dataset/childcare.xlsx", sheetName = "CHILD CARE SERVICES HOUSING", stringsAsFactors=F)   #read child care data
ICdata <- read.xlsx("dataset/childcare.xlsx", sheetName = "INFANT CARE SERVICES HOUSING", stringsAsFactors=F)   #read infant care data

names(CCdata) <- c("Housing Estate","PO Num","Num_of_Centres_2013","Total_Capacity_2013","Full_Day_Enrollment_2013",
                   "Half_Day_Enrollment_2013","Flexicare_Enrollment_2013","Num_of_Centres_2014",
                   "Total_Capacity_2014","Full_Day_Enrollment_2014","Half_Day_Enrollment_2014","Flexicare_Enrollment_2014")

names(ICdata) <- c("Housing Estate","PO Num","Num_of_Centres_2013","Total_Capacity_2013","Full_Day_Enrollment_2013",
                   "Half_Day_Enrollment_2013","Flexicare_Enrollment_2013","Num_of_Centres_2014",
                   "Total_Capacity_2014","Full_Day_Enrollment_2014","Half_Day_Enrollment_2014","Flexicare_Enrollment_2014")


searchEstate <- function(POhead){
        
                iRow <- grep(POhead, CCdata[,2])
                EstateInfo <- CCdata[iRow, c(1,8:12)]
                return(EstateInfo)
}

searchEstateIC <- function(POhead){
  
  iRow <- grep(POhead, ICdata[,2])
  ICEstateInfo <- ICdata[iRow, c(1,8:12)]
  return(ICEstateInfo)
}


shinyServer(
        function(input,output){
                
                #Extract first 2 numbers of the Postal Code
                POhead <- reactive({substr(input$POnum,1,2)})
                Estate <- reactive({searchEstate(POhead())[,1]})   #search the estate based on first 2 digit of PO number
                
                #Childcare Info
                CCcentres <- reactive({searchEstate(POhead())[,2]})   #number of childcare centers
                CCenrollInfo <- reactive({as.numeric(searchEstate(POhead())[,4:6])})   #Child care enrollment info
                CCcapacity <- reactive({as.numeric(searchEstate(POhead())[,3])})    #Child care total capacity
                
                output$oEstate <- renderText(Estate())     #display the housing estate
                output$oCCcentres <- renderText(CCcentres())  #display number of childcare centres
                
                #Infantcare Info
                ICcentres <- reactive({searchEstateIC(POhead())[,2]})   #number of infant care centers
                ICenrollInfo <- reactive({as.numeric(searchEstateIC(POhead())[,4:6])})   #infant care enrollment info
                ICcapacity <- reactive({as.numeric(searchEstateIC(POhead())[,3])})    #infant care total capacity
                
                output$oICcentres <- renderText(ICcentres())  #display number of infant care centres
                
                
                output$CCstackplot <- renderPlot({
                  #construct childcare info
                  CCenrolltype <- c("Full-day","Half-day","Flexicare")
                  CCenrollcount <- CCenrollInfo()
                  CCcapacity <- CCcapacity()
                  CCenroll.df <- data.frame(Centre=rep("Child Care",3),Enrollment_type=CCenrolltype,Enrollment_Count=CCenrollcount)
                  
                  #construct infant care info
                  ICenrolltype <- c("Full-day","Half-day","Flexicare")
                  ICenrollcount <- ICenrollInfo()
                  ICcapacity <- ICcapacity()
                  ICenroll.df <- data.frame(Centre=rep("Infant Care",3),Enrollment_type=ICenrolltype,Enrollment_Count=ICenrollcount)
                  
                  Enroll.df <- rbind(CCenroll.df,ICenroll.df)
                
                  ggplot(Enroll.df, aes(x=Centre,y=Enrollment_Count, fill=Enrollment_type)) + geom_bar(stat="identity", colour="white") +
                    geom_hline(yintercept=CCcapacity, colour="red", linetype=1) + annotate("text", x="Child Care", y=CCcapacity + CCcapacity*0.1, colour="red", label="Total Child Care Capacity") + 
                    geom_hline(yintercept=ICcapacity, colour="blue", linetype=2) + annotate("text", x="Infant Care", y=ICcapacity + CCcapacity*0.1, colour="blue", label="Total Infant Care Capacity")
                  
                })

                
        }
        )