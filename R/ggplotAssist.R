#' A shiny app for learn ggplot2

#' @param df A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#'
#' @return An R code for ggplot
#' @importFrom shiny div selectInput runApp fluidPage tags HTML titlePanel hr fluidRow column
#' @importFrom shiny textInput checkboxInput numericInput conditionalPanel verbatimTextOutput uiOutput h3 actionButton showModal modalDialog modalButton
#' @importFrom shiny validate need renderPrint updateTextInput updateCheckboxInput reactive renderPlot 
#' @importFrom shiny updateSelectizeInput renderUI htmlOutput tagList updateNumericInput updateSelectInput imageOutput textAreaInput updateTextAreaInput
#' @importFrom shiny observe br observeEvent renderImage stopApp plotOutput runGadget dialogViewer paneViewer h4 radioButtons sliderInput reactiveValues updateSliderInput browserViewer animationOptions
#' @importFrom shinyWidgets radioGroupButtons materialSwitch pickerInput updateMaterialSwitch updateRadioGroupButtons
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom utils capture.output
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select lead
#' @importFrom scales muted
#' @importFrom magrittr "%>%"
#' @importFrom editData checkboxInput3 numericInput3 selectInput3 textInput3 
#' @importFrom ggplot2 map_data
#' @importFrom stringr str_detect str_trim str_length str_locate str_c str_replace str_replace_all str_extract_all str_locate_all
#' @importFrom grDevices colors
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(rstudioapi)
#' library(miniUI)
#' library(moonBook)
#' library(shinyAce)
#' library(ggthemes)
#' library(shiny)
#' library(stringr)
#' library(editData)
#' library(shinyWidgets)
#' library(gcookbook)
#' library(shiny)
#'# Only run examples in interactive R sessions
#' if (interactive()) {
#'     result<-ggplotAssist(mtcars)
#'     result
#' }
ggplotAssist=function(df=NULL,viewer="browser"){

     
    # geomData=read.csv("data-raw/geom.csv",stringsAsFactors = FALSE)
    # settingData=read.csv("data-raw/setting.csv",stringsAsFactors = FALSE)
    # defaultVar=read.csv("data-raw/default.csv",stringsAsFactors = FALSE)
    # devtools::use_data(geomData,settingData,defaultVar,internal=TRUE,overwrite=TRUE)
      
     if(!isNamespaceLoaded("tidyverse")){
          attachNamespace("tidyverse")
     }
    
    if(!isNamespaceLoaded("ggplot2")){
        attachNamespace("ggplot2")
    }
    
    if(!isNamespaceLoaded("ggthemes")){
        attachNamespace("ggthemes")
    }

    #  selectInput3<-function(...,width=100){
    #     mywidth=paste(width,"px",sep="")
    #     div(style="display:inline-block;",selectInput(...,width=mywidth))
    # }

    context <- rstudioapi::getActiveDocumentContext()

    # Set the default data to use based on the selection.
    text <- context$selection[[1]]$text
    defaultData <- text

    if(is.null(df)) {
         if(nzchar(defaultData)) {
              df=defaultData
         } else {
              df="mtcars"
         }
    }
    if(any(class(df) %in% c("data.frame","tibble","tbl_df"))) {
         mydata=deparse(substitute(df))
    } else if(class(df) =="character") {

        result<-tryCatch(eval(parse(text=df)),error=function(e) "error")
        if(any(class(result) %in% c("data.frame","tibble","tbl_df"))) mydata=df
        else  return(NULL)
    }
    

    #geoms<-sort(geomData$geom)
    myfonts=c('mono','sans','serif','URWHelvetica','URWTimes','Courier','Helvetica','Times','AvantGarde','Bookman','Helvetica-Narrow','NewCenturySchoolbook','Palatino','URWGothic','URWBookman','NimbusMon','NimbusSan','NimbusSanCond','CenturySch','URWPalladio','NimbusRom')
    geomsall<-sort(unique(c(geomData$geom,unlist(strsplit(settingData$geom,",")))))
    aeses<-c("x","y","z","group","colour","fill","label","alpha","linetype","size","shape","xmin","xmax","ymin","ymax","sample")
    types<-c("mapping","setting")
    data<-eval(parse(text=mydata))
    colno=length(colnames(data))
    
    dfj<-data.frame(grp=c('A','B'),fit=4:5,se=1:2)
    datak<-data.frame(murder=datasets::USArrests$Murder,state=tolower(rownames(datasets::USArrests)))
    nmap<-ggplot2::map_data('state')
    seals$z=with(seals,sqrt(delta_long^2+delta_lat^2))
    
    geomchoices=c("",sort(unique(defaultVar[defaultVar$var=="geom",]$default)))
    statchoices=c("",sort(c(unique(defaultVar[defaultVar$var=="stat",]$default),"qq")))
    
    
    getDefault=function(data,geoms,vars){
        result <- data %>% 
            filter(str_detect(data[["geom"]],geoms),str_detect(data[["var"]],vars)) %>%
            select("default") %>% as.character() 
        if(result=="character(0)") {
            result<-""
        }
        result   
    }
    
    #x="theme(legend.margin=margin(t=0,r=0,b=0,l=0,unit='mm'),legend.background=element_rect(fill='red',size=rel(1.5)),panel.background=element_rect(fill='red'),legend.position='bottom')" 
    extractArgs=function(x){
        
        result<-tryCatch(eval(parse(text=x)),error=function(e) return("error"))
        
        if("character" %in% class(result)){
            args=character(0)
        } else {
            if(length(names(result)>0)){
                pos=unlist(str_locate_all(x,names(result)))
                pos=c(sort(pos[seq(1,length(pos),by=2)]),nchar(x)+1)
                
                args=c()
                for(i in 1:(length(pos)-1)){
                    args=c(args,substring(x,pos[i],lead(pos)[i]-2))
                }
                
            } else{
                args=character(0)
            }
        }
        args
    }
    
    setdiff2=function(args,x){
        # cat("args=",args,"\n")
        # cat('x=',x,"\n")
        result=args
        if(length(args)>0){
        args2=str_trim(str_replace_all(args,"=.*",""),side="both")    
        args2
        pos=c()
        for(i in 1:length(args2)){
            if(args2[i]==x) pos=c(pos,i)
        }
        pos
        if(!is.null(pos)) result=args[-pos]
        }
        # cat("result=",result,"\n")
        result
    }
    splitData=function(df,colname){
        # df=settingData
        # colname="setting"
        # nrow(df)
        if(nrow(df)==0){
            result=df
        } else{
            result=c()
            for(i in 1:nrow(df)){
                # cat("i=",i,"\n")
                # cat("df[[colname]][i]=",df[[colname]][i],"\n")     
                if(str_detect(df[[colname]][i],",")){
                    valuechoice=unlist(strsplit(df[[colname]][i],","))
                    valuechoice=str_trim(valuechoice)
                    for(j in 1:length(valuechoice)){
                        result=rbind(result,df[i,])
                        result[nrow(result),colname]=valuechoice[j] 
                    }
                } else{
                    result=rbind(result,df[i,])
                }
                
            } 
        }
        result
    }
    
    settingData=splitData(settingData,"setting")
    themeData=splitData(themeData,"setting")

      # retValue=runApp(list(
        ui=miniPage(
            gadgetTitleBar("Learn ggplot2"),
            
            miniContentPanel(
               
                
            fluidRow(
                
                
                
                # column(3,
                #        h4("Select function"),
                #        selectInput("iplotmain",NULL,
                #                    choices=mains,
                #                    selectize=FALSE,selected="ggplot",size=10)
                # ),
                column(3,
                       h3("Data / Preprocessing"),
                       materialSwitch("doPreprocessing","Preprocessing",value=FALSE,status="success",right=TRUE),
                       textAreaInput("preprocessing",NULL,value="",rows=3,placeholder="Enter R codes for preprocessing here !"),
                       textInput("mydata","Enter data name",value=mydata),
                       materialSwitch("showDataStr","show str",status="success",right=TRUE)),
                column(2,
                       
                       h3("Aesthetics"),
                       selectInput("aesmain",NA,choices=aeses,selectize=FALSE,size=12)
                ),
                column(3,
                      
                       h3("mapping"),
                       checkboxInput("asFactor","as factor",value=FALSE),
                       selectInput("varmain",NA,choices=colnames(data),
                                                    selectize=FALSE,size=min(10,colno),selected="")
                       
                ),
                column(4,
                       h3("R code for ggplot"),
                       textAreaInput("maincode",NULL,value="",rows=3),
                       actionButton("resetmain","reset"),
                       conditionalPanel(condition="true==false",
                       verbatimTextOutput("mainText"))
                       )
                ),
            conditionalPanel(condition="input.showDataStr==true",
                             verbatimTextOutput("text")),
                
                #plotOutput("mainPlot",width="400px",height="300px"),
                
            
            
            h3("Add Layer(s)"),
            fluidRow(
                
                column(3,
                       radioGroupButtons("selectedLayer","Select", choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale","annotate","limits","others"),status="success"),
                       selectInput("geoms",NA,choices=geomsall,selectize=FALSE,size=23,selected=""),
                       actionButton("showEx","Show Example")
                ),
                conditionalPanel(condition='input.geoms!="theme"',
                column(2,
                       textInput("geomdata","data",value=""),
                       h4("Select"),
                       radioButtons("type",NA,choices=types,selected="setting"),
                       h4("Aesthetics"),
                       selectInput("aes","aes",choices=aeses,selectize=FALSE,size=10),
                       conditionalPanel(condition="input.selectedLayer=='geom'|input.selectedLayer=='stat'",
                       selectInput("position","position",
                                   choices=c("","stack","fill","dodge","jitter","nudge","identity"))),
                       conditionalPanel(condition="input.selectedLayer=='geom'",
                                        selectInput("stat","stat",choices=statchoices)),
                       conditionalPanel(condition="input.selectedLayer=='stat'",
                                        selectInput("geom","geom",choices=geomchoices))
                ),
                column(3,
                       
                       conditionalPanel(condition="input.type=='mapping'", 
                                        h4("mapping"),
                                        checkboxInput("asFactor2","as factor",value=FALSE),
                                        selectInput("var","var",choices=colnames(data),
                                                    selectize=FALSE,size=min(8,colno),selected="")),
                       conditionalPanel(condition="input.type=='setting'", 
                                        h4("setting")),
                       conditionalPanel(condition="input.geoms!='guides'", 
                       textInput("varset","varset",width="200px"),
                       checkboxInput("addquote","addquote",value=FALSE)),
                       conditionalPanel(condition="input.geoms=='guides'",
                                        selectInput("guideaes","guideaes",choices="")
                       ),
                       conditionalPanel(condition="input.type=='setting'", 
                                        uiOutput("varsetUI")
                                        ),
                       conditionalPanel(condition="true==false",
                       textInput("guideLegend","guideLegend",value=""))
                       # ,actionButton("addmap","add")
                       
                        
                )),
                conditionalPanel(condition="input.geoms=='theme'",
                    column(3,
                           selectInput("args","arguments",choices=c(),
                                       selectize=FALSE,size=30,selected="")
                    ),
                    column(2,
                           #textInput("arg","arg",value=""),
                           uiOutput("argsUI"),
                           textInput("argresult","argresult",value=""),
                           textInput("argresult2","argresult2",value=""),
                           textAreaInput("argresult3","argresult3",value="")
                    )
                ),
                column(4,
                       h4("Layer under construction"),
                       textAreaInput("layer",NULL,value="",rows=2),
                       #actionButton("updatePreview","Update Preview"),
                       actionButton("addlayer","Add Layer"),
                       actionButton("resetmap","reset"),
                       conditionalPanel(condition="true==false",
                                        numericInput("layerno","layerno",value=0)),
                       
                       selectInput("layers","Added layers",choices="",selectize=FALSE,size=5),
                       actionButton("dellayer","Delete Layer")
                       
                       
                )
            ),
            hr(),
            fluidRow(
               column(6,
                      h4("R code for plot"),
                      aceEditor("code",value="",height="100px",showLineNumbers=TRUE),
                      plotOutput("plot")
               ),
               column(6,
                      radioGroupButtons("Option",NULL, 
                                        choices = c("Preview"=1, "Layer by layer"=2),
                                        status="success"),
                      conditionalPanel(condition="input.Option==2",
                  
                   sliderInput("no","layer",min=1,max=1,value=1,step=1,
                               animate=animationOptions(interval=1500)),
                   plotOutput("plot2",width="400px",height="300px"),
                   verbatimTextOutput("codes")),
                   conditionalPanel(condition="input.Option==1",
                                   
                                    textAreaInput("CodeUC",NULL,value="",height="100px",width="500px"),
                                    plotOutput("plot3",width="400px",height="300px")
                   )
               )
               
           )
        ))
        server=function(input,output,session){

           
            main <- reactiveValues(type=c(),aes=c(),var=c())
            layer <- reactiveValues(type=c(),aes=c(),var=c())
            layers <-reactiveValues(layer=c())
            rv=reactiveValues(theme="")
            
            #themeData
            (argGroup=unique(themeData$group))
            argchoice=lapply(1:length(argGroup),function(i) i)
            for(i in 1:length(argGroup)){
            
                argchoice[[i]]=themeData$setting[themeData$group==argGroup[i]]
                names(argchoice)[i]=argGroup[i]    
            }
           #argchoice
           updateSelectInput(session,"args",choices=argchoice)
           
           
           
            refreshMaincode=TRUE
           
            addValue=function(X,A){
                if(is.null(X)) X<-A
                else X=c(X,A)
                X
            }
            
            output$mainText=renderPrint({
                temp=makeGuideLegend()
                temp
            })

            observe(makeGuideLegend())
            
            output$text=renderPrint({
                if(input$doPreprocessing) eval(parse(text=input$preprocessing))
                df=eval(parse(text=input$mydata))
                if(!("tibble" %in% class(df))) df=as_tibble(df)
                df
            })
            
            observeEvent(input$selectedLayer,{
                #geomData=read.csv("geom.csv",stringsAsFactors = FALSE)
                
                if(input$selectedLayer=="guides"){
                   mychoices="guides" 
                } else if(input$selectedLayer=="labs"){
                    mychoices="labs" 
                } else if(input$selectedLayer=="annotate"){
                    #geoms<-sort(geomData$geom)
                    mychoices=geomsall[str_detect(geomsall,"annota")]
                } else if(input$selectedLayer=="others"){
                    mychoices=c("borders") 
                } else if(input$selectedLayer=="limits"){
                    mychoices=c("expand_limits","lims","xlim","ylim") 
                }
                else{
                    #geoms<-sort(geomData$geom)
                   mychoices=geomsall[str_detect(geomsall,paste0(input$selectedLayer,"_"))]
                }
                if(input$selectedLayer=="theme"){
                    mychoices=c("theme",mychoices)
                } else if(input$selectedLayer=="labs"){
                    mychoices=c(mychoices,"xlab","ylab","ggtitle")
                }
                updateSelectInput(session,"geoms",choices=mychoices)
            })
            
            observeEvent(input$geoms,{
          
                if(!is.null(input$geoms)){
                   
                if(input$geoms=="guides"){
                    choices<-setdiff(main$aes,c("x","y"))
                } else {
                    temp=geomData[geomData$geom==input$geoms,"aes"]
                    if(length(temp)==0) choices=c("")
                    else choices<-unlist(strsplit(temp,","))
                }
                   
                updateSelectInput(session,"aes",choices=choices,selected="")
                updateSelectInput(session,"var",selected="")
                
                if(str_detect(input$geoms,"[geom|stat]")){
                    updateSelectInput(session,"position",selected=getDefault(defaultVar,input$geoms,"position"))
                }
                if(str_detect(input$geoms,"geom")){
                    updateSelectInput(session,"stat",selected=getDefault(defaultVar,input$geoms,"stat"))
                }
                if(str_detect(input$geoms,"stat")){
                    updateSelectInput(session,"geom",selected=getDefault(defaultVar,input$geoms,"geom"))
                }
                if(input$geoms!="theme"){
                    temp=makeLayer()
                    updateTextAreaInput(session,"layer",value=temp)
                }
                }
                # if(str_detect(input$geoms,"stat")){
                #     updateSelectInput(session,"geom",selected=getDefault(defaultData,geoms,"stat"))
                # }
                
                
            })
            
            observeEvent(input$asFactor,{
                
                result=c()
                if(input$doPreprocessing) eval(parse(text=input$preprocessing))
                df=eval(parse(text=input$mydata))
                if(input$asFactor==TRUE){
                    for(i in 1:ncol(df)){
                        result=c(result,ifelse(is.numeric(df[[colnames(df)[[i]]]]),
                                           paste0("factor(",colnames(df)[i],")"),colnames(df)[i]))
                    }
                } else{
                    result=colnames(df)
                }
              
                updateSelectInput(session,"varmain",choices=result)
                
            })
            
            observeEvent(input$asFactor2,{
                
                result=c()
                if(input$doPreprocessing) eval(parse(text=input$preprocessing))
                df=eval(parse(text=input$mydata))
                if(input$asFactor2==TRUE){
                    for(i in 1:ncol(df)){
                        result=c(result,ifelse(is.numeric(df[[colnames(df)[[i]]]]),
                                               paste0("factor(",colnames(df)[i],")"),colnames(df)[i]))
                    }
                } else{
                    result=colnames(df)
                }
                
                updateSelectInput(session,"var",choices=result)
                
            })
            
            observeEvent(input$showEx,{
                refreshMaincode<<-FALSE
                updateTextInput(session,"mydata",value=geomData[geomData$geom==input$geoms,"data"])
                updateTextAreaInput(session,"maincode",value=geomData[geomData$geom==input$geoms,"code"])
               
                temp<-geomData[geomData$geom==input$geoms,"ex2"]
                temp=str_trim(temp,side="both")
                updateTextAreaInput(session,"layer",value=temp)
                temp<-geomData[geomData$geom==input$geoms,"layer"]
                if(temp!=""){
                    mychoice=unlist(strsplit(temp,"+",fixed=TRUE))
                    mychoice=str_trim(mychoice,side="both")
                    layers$layer<-mychoice
                    updateNumericInput(session,"layerno",value=length(mychoice)+1)
                    updateSelectInput(session,"layers",choices=mychoice)
                }else{
                    layers$layer<-c()
                    updateNumericInput(session,"layerno",value=1)
                    updateSelectInput(session,"layers",choices="")
                }
                
                refreshMaincode<<-TRUE
            })
            
            test=function(e){
                
                showModal(modalDialog(
                    title = "Error in preprocessing",
                    "There is an error in preprocessing. Press 'Esc' or Press 'OK' button",
                    easyClose = TRUE,
                    footer=modalButton("OK")
                ))
                updateMaterialSwitch(session,'doPreprocessing',value=FALSE)
            }
            
            observeEvent(input$doPreprocessing,{
                tryCatch(eval(parse(text=input$preprocessing)),error=function(e){test(e)})
            })
            
            observeEvent(input$mydata,{
                
                if(input$doPreprocessing){
                    eval(parse(text=input$preprocessing))
                }
                validate(
                    need(any(class(try(eval(parse(text=input$mydata)))) %in% c("tbl_df","tibble","data.frame")),
                         "Please enter the name of data")
                )
                
                data1<-eval(parse(text=input$mydata))
                
                if(!is.null(data1)) {
                    updateSelectInput(session,"varmain",
                                      choices=c(colnames(data1),
                                                "1","..density..",".count..","..prop.."),selected="")
                    updateSelectInput(session,"var",choices=colnames(data1),selected="")
                    
                }
                
                if(refreshMaincode) {
                    temp=makeMain()
                    updateTextAreaInput(session,"maincode",value=temp)
                }
            })
            
            observeEvent(input$geomdata,{
                if(input$doPreprocessing) {
                    eval(parse(text=input$preprocessing))
                }
                
                if(input$geomdata=="") {
                    data1<-eval(parse(text=input$mydata))
                } else {
                    validate(
                        need(any(class(try(eval(parse(text=input$geomdata)))) %in% c("tbl_df","tibble","data.frame")),
                             "Please enter the name of data")
                    )
                    data1<-eval(parse(text=input$geomdata))
                }
                
                if(!is.null(data1)) {
                    updateSelectInput(session,"var",choices=colnames(data1),selected="")
                    
                }
            })
            
            observeEvent(input$position,{
                if(input$selectedLayer!="scale"){
                if(input$position!=""){
                    if("position" %in% layer$aes){
                        pos=str_detect(layer$aes,"position")
                        layer$type=layer$type[!pos]
                        layer$aes=layer$aes[!pos]
                        layer$var=layer$var[!pos]
                    }
                    if(input$position!=getDefault(defaultVar,input$geoms,"position")){
                        myvar=paste0("'",input$position,"'")
                        layer$type=addValue(layer$type,"setting")
                        layer$aes=addValue(layer$aes,"position")
                        layer$var=addValue(layer$var,myvar)
                    }
                }
                temp=makeLayer()
            
                updateTextAreaInput(session,"layer",value=temp)
                }
               
            })
            observeEvent(input$stat,{
                if(input$stat!=""){
                    if("stat" %in% layer$aes){
                        pos=str_detect(layer$aes,"stat")
                        layer$type=layer$type[!pos]
                        layer$aes=layer$aes[!pos]
                        layer$var=layer$var[!pos]
                    }
                    if(input$stat!=getDefault(defaultVar,input$geoms,"stat")){
                        myvar=paste0("'",input$stat,"'")
                        layer$type=addValue(layer$type,"setting")
                        layer$aes=addValue(layer$aes,"stat")
                        layer$var=addValue(layer$var,myvar)
                    }
                }
                temp=makeLayer()
                updateTextAreaInput(session,"layer",value=temp)
            })
            
            observeEvent(input$facetwrap,{
                if(input$facetwrap!=""){
                layer$type="setting"
                layer$aes="facets"
                layer$var=paste0("~",str_c(input$facetwrap,collapse="+"))
                }
                
            })
            
            observe({
                if(!is.null(input$geoms)){
                    if(input$geoms=="facet_grid"){
                if((!is.null(input$facetrow))&(!is.null(input$facetcol))){
                
                    layer$type="setting"
                    layer$aes="facets"
                    layer$var=paste0(str_c(input$facetrow,collapse="+"),
                                     "~",str_c(input$facetcol,collapse="+"))
                
                } else if(!is.null(input$facetcol)){
                    layer$type="setting"
                    layer$aes="facets"
                    layer$var=paste0("~",str_c(input$facetcol,collapse="+"))
                } else if(!is.null(input$facetrow)){
                    layer$type="setting"
                    layer$aes="facets"
                    layer$var=paste0(str_c(input$facetrow,collapse="+"),
                                     "~.")
                } else{
                    layer$type=NULL
                    layer$aes=NULL
                    layer$var=NULL
                }
                    }
                }
            })
            
            
            observeEvent(input$var,{
                layer$type=addValue(layer$type,input$type)
                layer$aes=addValue(layer$aes,input$aes)
                layer$var=addValue(layer$var,input$var)
                
            })
            
            observeEvent(input$varset,{
                if(input$varset!=""){
                if(!is.null(input$aes)){
                    myvar=input$varset
                    if((input$type %in% layer$type)&(input$aes %in% layer$aes)){
                        pos1=str_detect(layer$type,input$type)
                        pos2=str_detect(layer$aes,input$aes)
                        pos=pos1 & pos2
                        layer$var[pos]=myvar
                    } else{
                        layer$type=addValue(layer$type,input$type)
                        layer$aes=addValue(layer$aes,input$aes)
                        layer$var=addValue(layer$var,myvar)
                    }
                }
                }
            })
            
            observeEvent(input$guideaes,{
                # if(!is.null(input$aes)){
                # pos=grep(input$aes,layer$aes)
                # if(length(pos)>0){
                #     layer$type=layer$type[-pos]
                #     layer$aes=layer$aes[-pos]
                #     layer$var=layer$var[-pos]
                # }
                # }
               
                    # layer$type=addValue(layer$type,"setting")
                    # layer$aes=addValue(layer$aes,input$aes)
                    # layer$var=addValue(layer$var,"guide_legend()")
                   
                if(input$guideaes %in% c("legend","colorbar","none")){
                    updateTextInput(session,"guideLegend",value=paste0("'",input$guideaes,"'"))
                 # layer$type=addValue(layer$type,"setting")
                 # layer$aes=addValue(layer$aes,input$aes)
                 # layer$var=addValue(layer$var,paste0("'",input$guideaes,"'"))
             } 
            })
            
            observeEvent(input$guideLegend,{
                if(!is.null(input$geoms)){
                if(input$geoms=="guides"){
                    if(!is.null(input$aes)){
                    pos=grep(input$aes,layer$aes)
                    if(length(pos)>0){
                        layer$type=layer$type[-pos]
                        layer$aes=layer$aes[-pos]
                        layer$var=layer$var[-pos]
                    }
                    layer$type=addValue(layer$type,"setting")
                    layer$aes=addValue(layer$aes,input$aes)
                    layer$var=addValue(layer$var,input$guideLegend)
                    }
                }
                }
            })
            
            makeGuideLegend=reactive({
                
                    if(input$guideaes %in% c("none","colorbar","legend")){
                        temp=paste0("'",input$guideaes,"'")
                    } else{
               
                        temp=paste0(input$guideaes,"(")
                        # layer<-NULL
                        # if(file.exists("layer.csv")) layer=read.csv("layer.csv")
                        
                        selected=settingData[str_detect(settingData$geom,input$guideaes),]
                        count=nrow(selected)
                        
                        if(count>0){
                            for(i in 1:count){
                                tempvar=selected$setting[i]
                                value=selected$value[i]
                                valuechoice=unlist(strsplit(value,","))
                                valuechoice=str_trim(valuechoice)
                                if(selected$input[i]=="select") {
                                    if(!is.null(input[[tempvar]])){
                                        if(input[[tempvar]]!=valuechoice[1]) {
                                            temp=mypaste0(temp,tempvar,"='",input[[tempvar]],"'")
                                        }
                                    }
                                } else if(selected$input[i]=="numeric"){
                                    if(!is.null(input[[tempvar]])){
                                        
                                        if(input[[tempvar]]!=value) 
                                            temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                    }
                                } else if(selected$input[i]=="text"){
                                    if(!is.null(input[[tempvar]])){
                                        if(input[[tempvar]]!=value) {
                                            if(selected$quoted[i]==TRUE){
                                                temp=mypaste0(temp,tempvar,"='",input[[tempvar]],"'")
                                            } else{
                                                temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                            }
                                        }
                                    }
                                } else if(selected$input[i]=="checkbox"){
                                    if(!is.null(input[[tempvar]])){
                                        if(input[[tempvar]]!=value) 
                                            temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                    }
                                }
                            }
                        }
                        
                        temp=paste0(temp,")")
                    }
                    updateTextInput(session,"guideLegend",value=temp)
                    
                    temp
            })
            
            # observeEvent(input$addmap,{
            # 
            #   
            #         if(!is.null(input$aes)){
            #             myvar=ifelse(input$type=="mapping",input$var,input$varset)
            #             if((input$type %in% layer$type)&(input$aes %in% layer$aes)){
            #                   pos1=str_detect(layer$type,input$type)
            #                   pos2=str_detect(layer$aes,input$aes)
            #                   pos=pos1 & pos2
            #                   layer$var[pos]=myvar
            #             } else{
            #                 layer$type=addValue(layer$type,input$type)
            #                 layer$aes=addValue(layer$aes,input$aes)
            #                 layer$var=addValue(layer$var,myvar)
            #             }
            #         }
            #   
            #     updateTextInput(session,"varset",value="")
            #     updateSelectInput(session,"var",selected="")
            #     
            # })
            
            observeEvent(input$aes,{
                if(input$type=="mapping") updateSelectInput(session,"var",selected="")
                updateTextInput(session,"varset",label=input$aes,value="")
                if(!is.null(input$geoms)){
                if(input$geoms=="guides"){
        
                        updateSelectInput(session,"guideaes",label=input$aes,
                                    choices=c("legend","guide_legend","colorbar","guide_colorbar","none"))
                 }}
            })

            observe({
                input$var
                input$varset
                # input$addmap
                
                temp=makeLayer()
                updateTextAreaInput(session,"layer",value=temp)
            })
            
            
            observeEvent(input$varmain,{
                
                if(!is.null(input$aesmain)){
                main$type=addValue(main$type,"mapping")
                main$aes=addValue(main$aes,input$aesmain)
                main$var=addValue(main$var,input$varmain)
                
                temp=makeMain()
                if(refreshMaincode) updateTextAreaInput(session,"maincode",value=temp)
                updateSelectInput(session,"varmain",selected="")
                updateSelectInput(session,"aesmain",selected="")
                } else{
                    session$sendCustomMessage(type = 'testmessage',
                                              message = list("Please select aes first",
                                                             controller = input$controller))
                }
        #         if(length(setdiff(main$aes,c("x","y")))>0){
        #             updateRadioGroupButtons(session,"selectedLayer","Select", 
        # choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale","annotate"),status="success")
        #         }
                
            })
            observeEvent(input$resetmap,{
                
                resetLayer()
                updateTextInput(session,"varset",label="varset")
                # updateSelectInput(session,"geoms",selected="")
                updateSelectInput(session,"var",selected="")
                #updateAceEditor(session,"layer",value="")
                updateSelectInput(session,"aes",selected="")
                updateTextInput(session,"argresult",value="") 
                updateTextInput(session,"argresult2",value="") 
            })
            
            observeEvent(input$resetmain,{
                resetMain()
                # updateRadioGroupButtons(session,"selectedLayer","Select", choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale","annotate"),status="success")
            })
            
            
            resetLayer=function(){
                # if(file.exists("layer.csv")) {
                #     file.remove("layer.csv")
                # } 
                layer$type <-c()
                layer$aes <-c()
                layer$var <-c()
                
                updateSelectInput(session,"geoms",selected="")
                updateSelectInput(session,"var",selected="")
                updateTextInput(session,"varset",value="")
                updateTextAreaInput(session,"layer",value="")
                updateTextAreaInput(session,"argresult3",value="")
               
            }
            resetMain=function(){
                # if(file.exists("main.csv")) {
                #     file.remove("main.csv")
                # } 
                main$type <-c()
                main$aes <-c()
                main$var <-c()
                
                updateSelectInput(session,"aesmain",selected="")
                updateSelectInput(session,"varmain",selected="")
            }
            makeLayer=function(){
                
                temp=""
                if(!is.null(input$geoms)){
                    if(input$geoms!="theme"){
                temp=paste0(input$geoms,"(")
                # layer<-NULL
                # if(file.exists("layer.csv")) layer=read.csv("layer.csv")
                
                findob<-input$geoms
                #find exact geom(not ...2, or ...n)
                findob<-paste0(findob,"^2n|",findob,",|",findob,"$")
                
                selected=settingData[str_detect(settingData$geom,findob),]
                count=nrow(selected)
                
                if(!is.null(layer)){
                    mylayer=data.frame(type=layer$type,aes=layer$aes,var=layer$var)
                    map=mylayer[mylayer$type=="mapping",]
                    set=mylayer[mylayer$type=="setting",]
                    set=set[!(set$aes %in% selected$setting),]
                    nmap=nrow(map)
                    nset=nrow(set)
                    if(nmap>0) {
                        temp=paste0(temp,"aes(")
                        for(i in 1:nrow(map)){
                            if(i>1) temp=paste0(temp,",")
                            temp=paste0(temp,map$aes[i],"=",map$var[i])
                        }
                        temp=paste0(temp,")")
                    }
                    if(nset>0) {
                        if(nmap>0) temp=paste0(temp,",")
                        for(i in 1:nrow(set)){
                            if(!is.null(input$geoms)){
                            
                                if(i>1) temp=paste0(temp,",")
                                if(input$geoms=="labs") {
                                    temp=paste0(temp,set$aes[i],"='",set$var[i],"'")
                                } else{
                                    temp=paste0(temp,set$aes[i],"=",set$var[i])
                                }
                            
                            }
                        }
                        
                    }
                }
                if(input$geoms %in% c("labs","lims","expand_limits")){
                    maincount=length(main$aes)
                    if(maincount>0){
                        for(i in 1:maincount){
                            tempvar=main$aes[i]
                            if(!is.null(input[[tempvar]])){
                            if(input[[tempvar]]!="") {
                                if(input$geoms=="labs"){
                                   temp=mypaste0(temp,tempvar,"='",input[[tempvar]],"'")
                                } else{
                                    temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                }
                            }
                            }
                        }
                    }
                }
                if(count>0){
                    for(i in 1:count){
                        tempvar=selected$setting[i]
                        if(tempvar=="position") tempvar="position2"
                        if(tempvar=="type") tempvar="type2"
                        value=selected$value[i]
                        valuechoice=unlist(strsplit(value,","))
                        valuechoice=str_trim(valuechoice)
                        if(selected$input[i]=="select") {
                            if(!is.null(input[[tempvar]])){
                                if(input[[tempvar]]!=valuechoice[1]) {
                                    if(selected$quoted[i]==TRUE){
                                        tempvarvalue=paste0("'",input[[tempvar]],"'")
                                    } else{
                                        if(!is.na(as.numeric(input[[tempvar]]))) tempvarvalue=input[[tempvar]]
                                        else if(!is.na(as.logical(input[[tempvar]]))) tempvarvalue=input[[tempvar]]
                                        else tempvarvalue=paste0(input[[tempvar]])
                                    }
                                    if(tempvar=="position2"){
                                        temp=mypaste0(temp,"position=",tempvarvalue)
                                    } else if(tempvar=="type2"){
                                        temp=mypaste0(temp,"type=",tempvarvalue)
                                    } else{
                                      temp=mypaste0(temp,tempvar,"=",tempvarvalue)
                                    }
                                }
                            }
                        } else if(selected$input[i]=="numeric"){
                            if(!is.null(input[[tempvar]])){
                                if(input[[tempvar]]!=value) {
                                    temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                }
                            }
                        } else if(selected$input[i]=="text"){
                            if(!is.null(input[[tempvar]])){
                                if(input[[tempvar]]!=value) {
                                    if(selected$quoted[i]==TRUE){
                                        if(str_detect(input[[tempvar]],"\\(")) temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                        else temp=mypaste0(temp,tempvar,"='",input[[tempvar]],"'")
                                    } else{
                                        temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                                    }
                                }
                            }
                        } else if(selected$input[i]=="checkbox"){
                            if(!is.null(input[[tempvar]])){
                            if(input[[tempvar]]!=value) 
                            temp=mypaste0(temp,tempvar,"=",input[[tempvar]])
                            }
                        }
                    }
                }
                if(input$geomdata!="") {
                    if(str_locate(temp,"\\(")[1,1]!=str_length(temp)) temp=paste0(temp,",")
                    temp=paste0(temp,"data=",input$geomdata)
                }
                if(input$geoms=="facet_grid"){
                    if(!is.null(input$labeller)){
                    if(input$labeller=="label_value"){
                    varnames=setdiff(c(input$facetrow,input$facetcol),".")
                    varnames<-c(".default",varnames,".rows",".cols")
                    count=length(varnames)
                    mylist=list()
                    labeltemp<-""
                    for(i in 1:count){
                        if(!is.null(input[[varnames[i]]])){
                        if(input[[varnames[i]]]!="NULL"){
                            if(labeltemp!="") labeltemp=paste0(labeltemp,",")
                            labeltemp=paste0(labeltemp,varnames[i],"=",input[[varnames[i]]])     
                        }
                        }
                    }
                    if(!is.null(input[[".multi_line"]])){
                    if(input[[".multi_line"]]==FALSE) {
                            if(labeltemp!="") labeltemp=paste0(labeltemp,",")
                            labeltemp<-paste0(labeltemp,".multi_line=FALSE")
                    }
                    }
                    if(labeltemp!="") temp=paste0(temp,",labeller=labeller(",labeltemp,")")
                    }
                    }
                }
                temp=paste0(temp,")")
                    
                    }
                
                
                }
                temp
            }
            
            mypaste0=function(temp,...){
                if(!str_detect(temp,"\\($")) {
                    temp=paste0(temp,",")
                } 
                paste0(temp,...)  
            }
            
            makeMain=function(){
                temp=paste0("ggplot(",input$mydata)
                #main<-NULL
                #if(file.exists("main.csv")) main=read.csv("main.csv")
                
                mymain=data.frame(type=main$type,aes=main$aes,var=main$var)
                map=mymain[mymain$type=="mapping",]
                    
                nmap=nrow(map)
                if(nmap>0) {
                        temp=paste0(temp,",aes(")
                        for(i in 1:nrow(map)){
                            if(i>1) temp=paste0(temp,",")
                            temp=paste0(temp,map$aes[i],"=",map$var[i])
                        }
                        temp=paste0(temp,")")
                }
                    
                 
                temp=paste0(temp,")")
                temp
            }
            
            observeEvent(input$resetmain,{
                
                temp=makeMain()
                
                updateTextAreaInput(session,"maincode",value=temp)
            })
            
           
            
            # output$mainPlot=renderPlot({
            #     #input$addmain
            #     input$resetmain
            #     
            #     if(input$doPreprocessing){
            #         eval(parse(text=input$preprocessing))
            #     }
            #     p<-eval(parse(text=input$maincode))
            #     p
            # })
            
            # observe({
            #     
            #     input$addmap
            #     input$resetmap
            #     input$addlayer
            #     input$geoms
            #     
            #     temp=makeLayer()
            #     
            #     updateAceEditor(session,"layer",value=temp)
            #     
            # })
            observe({
                input$code
                input$layer
                
                
                temp=input$code
                if(input$layer!="") temp=paste0(input$code," +\n",input$layer)
                
                updateTextAreaInput(session,"CodeUC",value=code2ggplot(temp))
            })
            
            observe({
                if(input$addquote) {
                    updateTextInput(session,"varset",value=paste0("'",input$varset,"'"))
                    updateCheckboxInput(session,"addquote",value=FALSE)
                }
                # if(input$addquotemain) {
                #     updateTextInput(session,"varsetmain",value=paste0("'",input$varsetmain,"'"))
                #     updateCheckboxInput(session,"addquotemain",value=FALSE)
                # }
            })
            
            observeEvent(input$addlayer,{
                temp<-input$layer
                
                layers$layer<-addValue(layers$layer,temp)
                # if(input$layerno==0) {
                #     layers<-data.frame(layer=temp,stringsAsFactors = FALSE)
                #     write.csv(layers,"layers.csv",row.names=FALSE)
                # } else {
                #     layers=read.csv("layers.csv")
                #     temp1=data.frame(layer=temp)
                #     temp1=rbind(layers,temp1)
                #     write.csv(temp1,"layers.csv",row.names=FALSE)
                #     layers<-read.csv("layers.csv",stringsAsFactors = FALSE)
                # }      
                #print(layers)
                #print(layers$layer)
                updateNumericInput(session,"layerno",value=input$layerno+1)
                updateSelectInput(session,"layers",choices=layers$layer)
                updateSelectInput(session,"geoms",selected="")
                updateSelectInput(session,"var",selected="")
                updateSelectInput(session,"aes",selected="")
                updateTextInput(session,"varset",label="varset",value="")
                updateTextAreaInput(session,"layer",value="")
                layer$type<-layer$aes<-layer$var<-c()
                resetLayer()
                
                
                
            })
            
            
            observeEvent(input$dellayer,{
                # layers<-read.csv("layers.csv",stringsAsFactors = FALSE)
                # str(layers)
                # print(input$layers)
                
                layers$layer <- layers$layer[layers$layer!=input$layers]
                
                # temp<-layers[layers$layer!=input$layers,]
                # layers=data.frame(layer=temp,stringsAsFactors = FALSE)
                # str(layers)
                # print(layers)
                # print(layers$layer)
                # write.csv(layers,"layers.csv",row.names=FALSE)
                updateSelectInput(session,"layers",choices=layers$layer)
                updateNumericInput(session,"layerno",value=input$layerno-1)
                
            })
            
            observeEvent(input$code,{
                (code=code2ggplot(input$code))
                (codes=separateCode(code))
                updateSliderInput(session,"no",min=1,max=length(codes),step=1)
            
            })
            
            observe({
                input$resetmain
                #input$addmain
                input$addlayer
                input$dellayer
                input$maincode
                
                #text=makeMain()
                text=input$maincode
                if(!is.null(layers$layer)){
                    # if(file.exists("layers.csv")) {
                    #     layers<-read.csv("layers.csv",stringsAsFactors = FALSE)
                    # 
                    count=length(layers$layer)
                    if(count>0){
                        for(i in 1:count) {
                            text=paste0(text,"+\n\t",layers$layer[i])
                        }
                    }
                }
                updateAceEditor(session,"code",value=text)
                
            })
            output$plot=renderPlot({
                input$resetmain
                #input$addmain
                input$addlayer
                input$dellayer
                
                if(input$doPreprocessing){
                    try(eval(parse(text=input$preprocessing)))
                }
                p<-eval(parse(text=input$code))
                p
                
            })
            
            output$plot2=renderPlot({
                input$resetmain
                #input$addmain
                input$addlayer
                input$dellayer
                
                if(input$doPreprocessing){
                    eval(parse(text=input$preprocessing))
                }
                (code=code2ggplot(input$code))
                (codes=separateCode(code))
                p<-eval(parse(text=codes[input$no]))
                p
                
            })
            
            output$plot3=renderPlot({
                input$layers
                input$maincode
                input$resetmap
                #input$updatePreview
                
                if(input$doPreprocessing){
                    try(eval(parse(text=input$preprocessing)))
                }
                #p<-eval(parse(text=isolate(input$CodeUC)))
                p<-eval(parse(text=input$CodeUC))
                p
                
            })
            
            output$codes=renderPrint({
                (code=code2ggplot(input$code))
                (codes=separateCode(code))
                #print(codes[input$no])
                cat(codes[input$no])
            })
            
            observeEvent(input$done, {
                
                result=""
                if(input$doPreprocessing) result=paste0(input$preprocessing,"\n")
                result=paste0(result,input$code)
                if(nzchar(defaultData)) {
                    insertText(text=result)
                    stopApp()
                } else{
                    p<-eval(parse(text=result))
                    stopApp(p)
                }
                
            })
            
            observeEvent(input$cancel, {
                stopApp(NULL)
            })
            
            value2choices=function(value){
                unlist(strsplit(value,",",fixed=TRUE))
            }
            
            output$varsetUI=renderUI({
                
                if(!(is.null(input$geoms))){
                    if(input$doPreprocessing){
                        eval(parse(text=input$preprocessing))
                    }
                    df=eval(parse(text=input$mydata))
                    mylist=list()
                    no=1
                    if(input$geoms=="facet_grid") {
                        mylist[[no]]=selectInput3("facetrow","by rows",
                                                 choices=c("",colnames(df)),multiple=TRUE)
                        no=no+1
                        mylist[[no]]=selectInput3("facetcol","by columns",
                                                 choices=c(".",colnames(df)),multiple=TRUE)
                        no=no+1
                    }
                    if(input$geoms=="facet_wrap") {
                        mylist[[no]]=selectInput3("facetwrap","wrap by",
                                                 choices=c("",colnames(df)),multiple=TRUE)
                        no=no+1
                    }
                    if(input$geoms %in% c("labs","lims","expand_limits")) {
                        count=length(main$aes)
                        if(count>0){
                            for(i in 1:count){
                            temp=main$aes[i]
                           mylist[[no]]=textInput3(temp,temp,value="",
                                                   placeholder=paste0(input$geoms," for ",temp),width=200)
                           no=no+1
                            }
                        }
                    }
                    
                    findob<-input$geoms
                    #find exact geom(not ...2, or ...n)
                    findob<-paste0(findob,"^2n|",findob,",|",findob,"$")
                    if(input$selectedLayer=="guides"){
                    if(input$guideaes %in% c("guide_legend","guide_colorbar")) {
                        findob<-input$guideaes
                    } else{
                        findob<-"!@#$%"
                    } 
                    }
                    selected=settingData[str_detect(settingData$geom,findob),]
                    count=nrow(selected)
                    if(count>0){
                        for(i in 1:count){
                            temp=selected$setting[i]
                            value=selected$value[i]
                            placeholder=selected$placeholder[i]
                            mywidth=min((((max(nchar(value),nchar(placeholder))*8)%/%100)+1)*100,200)
                        if(selected$input[i]=="select") {
                            tempid<-temp
                            if(temp=="position") tempid<-"position2"
                            if(temp=="type") tempid<-"type2"
                            mychoices=unlist(strsplit(value,",",fixed=TRUE))
                            if(tempid %in% c("colour","color","fill")){
                                mychoices=c(mychoices,colors()[!str_detect(colors(),mychoices)])
                            }
                            if(length(mychoices)>0)
                            mywidth=(((max(nchar(mychoices))*8)%/%100)+1)*100
                            else mywidth=100
                            mylist[[no]]= selectizeInput3(tempid,label=temp,
                                                 choices=mychoices,width=mywidth,
                                                 options=list(create=TRUE))
                        } else if(selected$input[i]=="numeric"){
                            mylist[[no]]= numericInput3(temp,label=temp,value=as.numeric(value))
                        } else if(selected$input[i]=="text"){
                            mylist[[no]]=textInput3(temp,label=temp,value=value,placeholder=placeholder,width=mywidth)
                        } else if(selected$input[i]=="checkbox"){
                            mylist[[no]]= checkboxInput3(temp,label=temp,value=as.logical(value))
                        }
                            no=no+1
                        }
                    }
                    if(input$geoms %in% c("scale_x_log10","scale_y_log10")){
                        mylist[[no]]=actionButton("addMathFormat","Add math_format")
                        no=no+1
                    }
                    if(input$geoms=="facet_grid"){
                        mylist[[no]]=uiOutput("labellerUI")
                        no=no+1
                    }
                    do.call(tagList,mylist)
               
                }
            })
            output$labellerUI=renderUI({
                 varnames=setdiff(c(input$facetrow,input$facetcol),".")
                 mychoices=c("NULL","label_value","label_both","label_bquote"
                             ,"label_context","label_parsed","label_wrap_gen")
                 varnames<-c(varnames,".rows",".cols",".default")
                 count=length(varnames)
                 mylist=list()
                 for(i in 1:count){
                     mylist[[i]]<-selectizeInput3(varnames[i],label=varnames[i],
                                                  choices=mychoices,width=200,
                                                  options=list(create=TRUE))
                 }
                 mylist[[count+1]]<-checkboxInput(".multi_line",".multi_line",value=TRUE)
                 do.call(tagList,mylist)
            })
            
           observeEvent(input$addMathFormat,{
               updateTextInput(session,"breaks",value="scales::trans_breaks('log10', function(x) 10^x)")
               updateTextInput(session,"labels",value="scales::math_format(10^.x)")
           })
            # observeEvent(input$args,{
            #     updateTextInput(session,"arg",label=input$args,value=themeData$value[themeData$setting==input$args])
            # })
            # 
            output$argsUI=renderUI({
                if(!is.null(input$args)){
                kind<-themeData$input[themeData$setting==input$args]
                value<-themeData$value[themeData$setting==input$args]
                mylist=list()
                if(kind %in% c("text","select")) {
                    if(kind=="text") mylist[[1]]<-textInput("arg",label=input$args,value=value)
                    if(kind=="select") mylist[[1]]<-selectInput("arg",label=input$args,choices=unlist(strsplit(value,",")))
                    mylist[[2]]=uiOutput("argsUI2")
                    
                }
                else if(kind=="checkbox") mylist[[1]]<-checkboxInput("arg",label=input$args,value=as.logical(value))
                
                
                do.call(tagList,mylist)
                }
            })
             
            output$argsUI2=renderUI({
                value=input$arg
                if(str_detect(value,"\\(")) {
                fname=unlist(strsplit(value,"\\("))[1]
                selected=settingData[str_detect(settingData$geom,fname),]
                count=nrow(selected)
                mylist=list()
                no=1
                if(count>0){
                    for(i in 1:count){
                        temp=selected$setting[i]
                        value=selected$value[i]
                        placeholder=selected$placeholder[i]
                        if(selected$input[i]=="select") {
                            tempid<-temp
                            if(temp=="position") tempid<-"position2"
                            if(temp=="type") tempid<-"type2"
                            mychoices=unlist(strsplit(value,",",fixed=TRUE))
                            if(tempid %in% c("colour","color","fill")){
                                mychoices=c(mychoices,colors())
                            }
                            mywidth=(((max(nchar(mychoices))*8)%/%100)+1)*100
                            mylist[[no]]= selectizeInput3(tempid,label=temp,
                                                          choices=mychoices,width=mywidth,
                                                          options=list(create=TRUE))
                        } else if(selected$input[i]=="numeric"){
                            mylist[[no]]= numericInput3(temp,label=temp,value=as.numeric(value))
                        } else if(selected$input[i]=="text"){
                            mywidth=min((((max(nchar(value),nchar(placeholder))*8)%/%100)+1)*100,200)
                            mylist[[no]]=textInput3(temp,label=temp,value=value,width=mywidth,placeholder=placeholder)
                        } else if(selected$input[i]=="checkbox"){
                            mylist[[no]]= checkboxInput3(temp,label=temp,value=as.logical(value))
                        }
                        no=no+1
                    }
                }
                do.call(tagList,mylist)
            }
            })
            
            observeEvent(input$arg,{
                updateTextInput(session,"argresult",value=input$arg)
            })
            
            makeTheme=reactive({
                result=""
                if(input$geoms=="theme"){
                    
                if(!is.null(input$args)){
                    kind<-themeData$input[themeData$setting==input$args]
                    value<-themeData$value[themeData$setting==input$args]
                    
                    if(kind %in% c("text","select")) {
                        if(str_detect(value,"\\(")) {
                            
                            result=unlist(strsplit(value,"\\)"))[1]
                            
                            fname=unlist(strsplit(value,"\\("))[1]
                            selected=settingData[str_detect(settingData$geom,fname),]
                            count=nrow(selected)
                            tempresult=c()
                            if(count>0){
                                for(i in 1:count){
                                    temp=selected$setting[i]
                                    tvalue=selected$value[i]
                                    kind2=selected$input[i]
                                    tempid<-temp
                                    if(temp=="position") tempid<-"position2"
                                    if(temp=="type") tempid<-"type2"
                                    if(!is.null(input[[tempid]])){
                                    if(input[[tempid]]!=tvalue){
                                        if(kind2=="select"){
                                            if(input[[tempid]]!=unlist(strsplit(tvalue,","))[1]){
                                          tempresult=c(tempresult,paste0(tempid,"='",input[[tempid]],"'"))    
                                            }
                                        } else{
                                            if(selected$quoted[i]){
                                            tempresult=c(tempresult,paste0(tempid,"='",input[[tempid]],"'"))    
                                            } else{
                                            tempresult=c(tempresult,paste0(tempid,"=",input[[tempid]]))
                                            }
                                        }
                                    }
                                    }
                                    
                                }
                            }
                            if(length(tempresult)>0) {
                                tempresult2=str_c(tempresult,collapse=",")
                            } else {
                                tempresult2=tempresult
                            }
                           
                            result=paste0(result,tempresult2)
                            result=paste0(result,")")
                        } else if(kind=="text"){
                            #result=paste0(result,input$args,"=",input$arg)
                            quoted=themeData$quoted[themeData$setting==input$args]
                            if(quoted){
                                result=paste0(result,"'",input$arg,"'")
                            } else{
                                result=paste0(result,input$arg)
                            }
                        } else if(kind=="select") {
                            if(!is.null(input$arg)){
                                if(input$arg!=unlist(strsplit(value,","))[1]){
                                    quoted=themeData$quoted[themeData$setting==input$args]
                                    if(quoted){
                                    result=paste0(result,"'",input$arg,"'")
                                    } else{
                                        result=paste0(result,input$arg) 
                                    }
                                }
                            } else{
                                result=""
                            }
                        }
                    }
                    else if(kind=="checkbox") {
                        if(!is.null(input$arg)){
                        if(input$arg!=as.logical(value)){
                          result=paste0(result,input$arg)
                        }
                        }
                    } 
                    
                    
                   
                }
                   
                }
                
                result
            })   
                
            observe({
                if(!is.null(input$geoms)){
                if(input$geoms=="theme"){
                    #print("observe1")
                    if(!is.null(input$args)){
                        #updateTextInput(session,"argresult2",value="")
                        updateTextInput(session,"argresult",value=makeTheme())
                        #updateTextInput(session,"arg",value=makeTheme())
                        
                    }
                    
                }
                }
            })
            
            observeEvent(input$args,{
                updateTextInput(session,"argresult2",value="")
            })
            observeEvent(input$argresult,{
                #print("observeEvent input$argresult")
                value<-themeData$value[themeData$setting==input$args]
                if(input$argresult==""){
                    updateTextInput(session,"argresult2",value="")
          
                } else if(input$argresult!=value ){
                updateTextInput(session,"argresult2",value=paste0(input$args,"=",input$argresult))
                   
                }
                
            })
            
            observeEvent(input$argresult2,{
                if(!is.null(input$geoms)){
                    if(input$geoms=="theme"){
                        updateTextAreaInput(session,"argresult3",value=makeTheme2())
                    }
                }
            })
            
            observeEvent(input$argresult3,{
                if(!is.null(input$geoms)){
                    if(input$geoms=="theme"){
                updateTextAreaInput(session,"layer",value=input$argresult3)
                    }
                }
            })
            
            class(eval(parse(text="theme()")))
            
            makeTheme2=reactive({
                
                
                if(input$argresult3=="") result="theme()"
                else result=input$argresult3
                   # cat("result=",result,"\n")
                    #result="theme(legend.background=element_rect(fill='red'),legend.position='element_rect()')"
                    
                    value<-themeData$value[themeData$setting==input$args]
                    (args<-extractArgs(result))
                    #cat("args=",args,"\n")
                    (args1<-setdiff2(args,input$args))
                    #cat("args1=",args1,"\n")
                    args2<-args1    
                    if(input$argresult2=="") {
                        args2<-args1
                    } else if(input$argresult!=value) {
                        args2<-c(args1,input$argresult2)
                    }
                    #cat("args2=",args2,"\n")
                    result=paste0("theme(",str_c(args2,collapse=","),")")
                    
                    result
            })
            
            separateCode=function(code){
                result=""
                if(!is.null(code)){
                    (temp=unlist(strsplit(code,"+",fixed=TRUE)))
                    temp=temp[nchar(temp)>0]
                    count=length(temp)
                    result=c()
                    if(count>0) {
                        for(i in 1:count){
                            result=c(result,str_c(temp[1:i],collapse="+\n"))
                        }
                    }
                }
                result
            } 
            
         
            splitCode<-function(code){
                (codes=stringr::str_replace_all(code,"\n",""))
                (temp=unlist(strsplit(codes,"+",fixed=TRUE)))
                temp=temp[nchar(temp)>0]
                temp
            }
            
           
            code2ggplot=function(code){
                codes=splitCode(code)
                if(length(codes)>0) selectMaxValid(codes)
            }
            
           
            selectMaxValid=function(codes){
                
                if(input$doPreprocessing){
                    try(eval(parse(text=input$preprocessing)))
                }
                
                count=length(codes)
                i=count
                for(i in count:1){
                    codes=codes[1:i]
                    temp=str_c(codes,collapse="+")
                    result<-c()
                    result<-tryCatch(eval(parse(text=temp)),
                                     error=function(e) return("error"),
                                     warning=function(w) return("warning"))
                    class(result)
                    if("ggplot" %in% class(result)){
                        return(temp)
                    }
                }
                return(NULL)
            }
        }
               #))
        if(viewer=="dialog") myviewer <- dialogViewer("ggplotAssist", width = 1000, height = 800)
        else if(viewer=="browser") myviewer <- browserViewer()
        else myviewer <- paneViewer()
        runGadget(ui, server, viewer = myviewer)

}


#' side-by-side selectizeInput
#'
#' @param ... Further arguments to be passed to selectizeInput
#' @param width Input width in pixel
#' @importFrom shiny selectizeInput
#' @export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          selectizeInput3("color", "color", choices=colors())
#'     )
#'     server <- function(input, output) {
#'
#'     }
#'     shinyApp(ui, server)
#'}
selectizeInput3=function (..., width = 100)
{
    mywidth = paste(width, "px", sep = "")
    div(style = "display:inline-block;", selectizeInput(..., width = mywidth))
}


