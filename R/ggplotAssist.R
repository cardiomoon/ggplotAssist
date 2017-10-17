#' A shiny app for learn dplyr

#' @param df A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#'
#' @return A manipulated tibble or NULL
#' @importFrom shiny div selectInput runApp fluidPage tags HTML titlePanel hr fluidRow column
#' @importFrom shiny textInput checkboxInput numericInput conditionalPanel verbatimTextOutput uiOutput h3 actionButton showModal modalDialog modalButton
#' @importFrom shiny validate need renderPrint updateTextInput updateCheckboxInput reactive renderPlot 
#' @importFrom shiny updateSelectizeInput renderUI htmlOutput tagList updateNumericInput updateSelectInput imageOutput textAreaInput updateTextAreaInput
#' @importFrom shiny observe br observeEvent renderImage stopApp plotOutput runGadget dialogViewer paneViewer h4 radioButtons sliderInput reactiveValues updateSliderInput browserViewer
#' @importFrom shinyWidgets radioGroupButtons materialSwitch pickerInput updateMaterialSwitch updateRadioGroupButtons
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom utils capture.output
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select 
#' @importFrom scales muted
#' @importFrom magrittr "%>%"
#' @importFrom editData checkboxInput3 numericInput3 selectInput3 textInput3
#' @importFrom ggplot2 map_data
#' @importFrom stringr str_detect str_trim str_length str_locate str_c
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
#'
#' \dontrun{
#' ggplotAssist(mtcars)
#' result<-ggplot(iris)
#' cat(attr(result,"code"))
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
    geomsall<-sort(unique(c(geomData$geom,unlist(strsplit(settingData$geom,",")))))
    aeses<-c("x","y","z","group","colour","fill","label","alpha","linetype","size","shape","xmin","xmax","ymin","ymax","sample")
    types<-c("mapping","setting")
    data<-get(df)
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
    
    splitData=function(df,colname){
        
        if(nrow(df)==0){
            result=df
        } else{
            result=c()
            for(i in 1:nrow(df)){
                
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
                       textInput("mydata","Enter data name",value=df),
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
                       radioGroupButtons("selectedLayer","Select", choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale"),status="success"),
                       selectInput("geoms",NA,choices=geomsall,selectize=FALSE,size=23,selected=""),
                       actionButton("showEx","Show Example")
                ),
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
                       
                        
                ),
                column(4,
                       h4("Layer under construction"),
                       textAreaInput("layer",NULL,value="",rows=2),
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
                  
                   sliderInput("no","layer",min=1,max=1,value=1,step=1,animate=TRUE),
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
           
            refreshMaincode=TRUE
           
            addValue=function(X,A){
                if(is.null(X)) X<-A
                else X=c(X,A)
                X
            }
            
            # output$mainText=renderPrint({
            #     temp=makeGuideLegend()
            #     temp
            # })
            
            #observe(makeGuideLegend())
            
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
                } else{
                    #geoms<-sort(geomData$geom)
                   mychoices=geomsall[str_detect(geomsall,paste0(input$selectedLayer,"_"))]
                }
                updateSelectInput(session,"geoms",choices=mychoices)
            })
            
            observeEvent(input$geoms,{
          
                if(!is.null(input$geoms)){
                if(input$geoms=="guides"){
                    choices<-setdiff(main$aes,c("x","y"))
                } else if(input$geoms=="labs"){
                    choices<-c(main$aes,"title","subtitle","caption")
                } else{
                    temp=geomData[geomData$geom==input$geoms,"aes"]
                    choices<-unlist(strsplit(temp,","))
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
                temp=makeLayer()
                updateTextAreaInput(session,"layer",value=temp)
                }
                # if(str_detect(input$geoms,"stat")){
                #     updateSelectInput(session,"geom",selected=getDefault(defaultData,geoms,"stat"))
                # }
                
                
            })
            
            observeEvent(input$asFactor,{
                
                result=c()
                df=get(input$mydata)
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
                df=get(input$mydata)
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
                data1<-get(input$mydata)
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
            
            observeEvent(input$facetcol,{
                if(!is.null(input$facetrow)){
                if((length(input$facetrow)>0)&(length(input$facetcol)>0)){
                    layer$type="setting"
                    layer$aes="facets"
                    layer$var=paste0(str_c(input$facetrow,collapse="+"),
                                     "~",str_c(input$facetcol,collapse="+"))
                }
                }
            })
            observeEvent(input$facetrow,{
                if(!is.null(input$facetcol)){
                 if((length(input$facetrow)>0)&(length(input$facetcol)>0)){
                    layer$type="setting"
                    layer$aes="facets"
                    layer$var=paste0(str_c(input$facetrow,collapse="+"),
                                     "~",str_c(input$facetcol,collapse="+"))
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
                if(length(setdiff(main$aes,c("x","y")))>0){
                    updateRadioGroupButtons(session,"selectedLayer","Select", choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale"),status="success")
                }
                
            })
            observeEvent(input$resetmap,{
                
                resetLayer()
                updateTextInput(session,"varset",label="varset")
                # updateSelectInput(session,"geoms",selected="")
                # updateSelectInput(session,"var",selected="")
                # updateAceEditor(session,"layer",value="")
                # 
            })
            
            observeEvent(input$resetmain,{
                resetMain()
                updateRadioGroupButtons(session,"selectedLayer","Select", choices = c("geom", "stat", "coord", "theme","facet","labs","guides","scale"),status="success")
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
                                        else tempvarvalue=paste0("'",input[[tempvar]],"'")
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
                temp=paste0(temp,")")
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
                resetLayer()
                layer$type<-layer$aes<-layer$var<-c()
                updateTextAreaInput(session,"layer",value="")
                
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
                input$layer
                input$maincode
                
                if(input$doPreprocessing){
                    try(eval(parse(text=input$preprocessing)))
                }
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
                
                    stopApp(result)
                }
                
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
                        if(selected$input[i]=="select") {
                            tempid<-temp
                            if(temp=="position") tempid<-"position2"
                            if(temp=="type") tempid<-"type2"
                            mylist[[no]]= selectInput3(tempid,label=temp,
                                                 choices=unlist(strsplit(value,",",fixed=TRUE)))
                        } else if(selected$input[i]=="numeric"){
                            mylist[[no]]= numericInput3(temp,label=temp,value=as.numeric(value))
                        } else if(selected$input[i]=="text"){
                            mylist[[no]]=textInput3(temp,label=temp,value=value)
                        } else if(selected$input[i]=="checkbox"){
                            mylist[[no]]= checkboxInput3(temp,label=temp,value=as.logical(value))
                        }
                            no=no+1
                        }
                    }
                    
                    do.call(tagList,mylist)
               
                }
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




