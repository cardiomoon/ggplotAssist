#' Create side-by side uiOutput
#' 
#' @param ... arguments to be passed to uiOutput
#' @export
#' @examples 
#' library(shiny)
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'      ui <- fluidPage(
#'           textInput4("name","name",""),
#'           uiOutput3("test")
#'      )
#'      server <- function(input, output) {
#'           
#'      }
#'      shinyApp(ui, server)
#' }  
uiOutput3=function (...) {
     div(style = "display:inline-block;", uiOutput(...))
}


#' Create side-by side textInput with disabled spell check
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param width The width of the input in pixel
#' @param bg backgroung color
#' @param ... arguments to be passed to textInput
#' @export
#' @examples 
#' library(shiny)
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'      ui <- fluidPage(
#'           textInput4("id", "id", ""),
#'           textInput4("name","name","")
#'      )
#'      server <- function(input, output) {
#'           
#'      }
#'      shinyApp(ui, server)
#' }  
textInput4=function (inputId, label, value = "",width=100,bg=NULL,...) 
{
     style=paste0("width: ",width,"px;")
     if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
     div(style="display:inline-block;",
         if(label!="") tags$label(label, `for` = inputId),
         tags$input(id = inputId, type = "text", class="form-control",value = value,
                    style=style,spellcheck="false",autocorrect="off",...))
}

#' Create side-by side textAreaInput with disabled spell check
#' 
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param width The width of the input in pixel
#' @param bg backgroung color
#' @param ... arguments to be passed to textInput
#' @export
#' @examples 
#' library(shiny)
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'      ui <- fluidPage(
#'           textAreaInput4("Code","Code","")
#'      )
#'      server <- function(input, output) {
#'           
#'      }
#'      shinyApp(ui, server)
#' }  
textAreaInput4=function (inputId, label, value = "",bg=NULL,width="100%",...)
{
    style=paste0("width: ",width,";")
    if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
    div(class="form-group shiny-input-container",
        tags$style(type="text/css", "textarea {width:100%}"),
        if(!is.null(label)) tags$label(label, `for` = inputId),
        tags$textarea(id = inputId, value = value,
                   style=style,spellcheck="false",autocorrect="off",...))
}

# textareaInput<-function(inputId, label="",value="", rows=8, width=100){
#     div(class="form-group shiny-input-container",
#         tags$style(type="text/css", "textarea {width:100%}"),
#         tags$textarea(id = inputId, placeholder = label, rows = rows, value=value,
#                       style=paste("width: ",width,"px; display:inline-block;",sep=""))
#     )    
# }



#'Elongate data.frame with column split by comma
#'
#' @param df a data.frame
#' @param colname column name
#' @export
#' @return An elongated data.frame        
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


#' UI of textFunction shiny module
#' @param id A string
#' @importFrom shiny NS
#' @export
#' @examples
#' library(ggplotAssist)
#' library(shiny)
#'# Only run examples in interactive R sessions
#'if(interactive()){
#' ui=fluidPage(
#'    textFunctionInput("text"),
#'    textOutput("text")
#')
#'server=function(input,output,session){
#'    rv=reactiveValues()
#'    rawData=read.csv("data-raw/setting.csv",stringsAsFactors = FALSE)
#'    settingData=splitData(rawData,"setting")
#'    rv$argList<-list(label="text",mode="text",value="element_text()",choices=NULL,width=200,
#'                     bg="lightcyan",placeholder="")
#'    result=callModule(textFunction,"text",argList=reactive(rv$argList),
#'                      editCode=reactive(TRUE),settingData=reactive(settingData))
#'    output$text=renderText({
#'        result()
#'    })
#'}
#'shinyApp(ui,server)
#'}
textFunctionInput=function(id){ 
    ns<-NS(id)
    
    tagList(
        uiOutput3(ns("functionInput"))
       
    )
}

#' Server function of textFunction shiny module
#' 
#' @param input input
#' @param output output
#' @param session session
#' @param argList A list containing options
#' @param editCode Logical. Wheter or not edit initial R code
#' @param settingData A data.frame contains information about functions
#' @importFrom shiny callModule
#' @importFrom stringr str_extract
#' @export
textFunction=function(input,output,session,argList=reactive(argList),
                      editCode=reactive(TRUE),settingData=reactive(NULL)){
    
    rv=reactiveValues()
   
    rv$myArgs<-reactive(argList()) 
   
    
   selectedData=reactive({
        myOptions<-rv$myArgs()
        
        result<-NULL
        findob<-NULL
        
        if(editCode()){      
            
             if(!is.null(input$text)){
                  if(str_detect(input$text,"[^\\(]*\\(\\)")){
                      findob<-str_extract(input$text,"[^\\(]*\\(")
                      findob
                      findob<-unlist(strsplit(findob,"\\("))[1]
                  }
             } 
        } else {
                  findob <-argList()$value
        }
        
            
        if(!is.null(findob)){
            #find exact geom(not ...2, or ...n)
                    findob<-paste0("^",findob) 
                    findob<-paste0(findob,"^2n|",findob,",|",findob,"$")
                    result<-settingData()[str_detect(settingData()$geom,findob),]
                    if(nrow(result)==0) result<-NULL
        }
         
        result
    })
    
    
    output$functionInput=renderUI({
         ns <- session$ns
       
         myOptions<-rv$myArgs() 
        # cat("ns('text')=",ns("text"),"\n")
       tagList(
            if(myOptions$mode=="text") 
               if(editCode())
                 textInput4(ns("text"),label=myOptions$label,value=myOptions$value,
                            width=myOptions$width,bg=myOptions$bg,
                            placeholder=myOptions$placeholder),
            if(myOptions$mode=="select")
                 selectizeInput3(ns("text"),label=myOptions$label,choices=myOptions$choices,
                                 selected=myOptions$selected,
                              width=myOptions$width,options=list(create=TRUE)),
            uiOutput3(ns("functionInput2"))
       )
       
    
    })
    
    output$functionInput2=renderUI({
         ns <- session$ns
         count=0
         mylist=list()
         
         #myOptions<-rv$myArgs()
         no=1
         selected<-selectedData()
         if(!is.null(selected)) {
             count=nrow(selected)
         }
         if(count>0){
                 for(i in 1:count){
                     temp=selected$setting[i]
                     value=selected$value[i]
                     placeholder=selected$placeholder[i]
                     mywidth=min((((max(nchar(value),nchar(placeholder))*8)%/%100)+1)*100,200)
                     if(selected$input[i] %in% c("select","text")) {
                         mylist[[no]]=textFunctionInput(ns(temp))
                     } else if(selected$input[i]=="numeric"){
                         mylist[[no]]= numericInput3(ns(temp),label=temp,value=as.numeric(value))
                     } else if(selected$input[i]=="checkbox"){
                         mylist[[no]]= checkboxInput3(ns(temp),label=temp,value=as.logical(value))
                     }
                     #cat("ns(temp)=",ns(temp),"\n")
                     no=no+1
                 }

         }
         do.call(tagList,mylist)
    })
    
    observeEvent(selectedData(),{
          count=0
          selected<-selectedData()
    
          if(!is.null(selected)) {
               count=nrow(selected)
          }
          rv$result=list()
          if(count>0){
               for(i in 1:count){
                    local({
                         j<-i
                         temp=selected$setting[j]
                         value=selected$value[j]
                         placeholder=selected$placeholder[j]
                         mywidth=min((((max(nchar(value),nchar(placeholder))*8)%/%100)+1)*100,200)
                    rv$result[[j]]<-""
                    temp=selected$setting[j]
                    if(!is.null(selected$input[j])){
                    if(selected$input[j]=="text") {
                         argList=list(label=temp,mode="text",value=value,width=mywidth,
                                      bg="lightcyan",placeholder=placeholder)
                         rv$result[[j]]<-callModule(textFunction,temp,argList=reactive(argList),
                                                    editCode=reactive(TRUE),settingData=reactive(settingData()))
                    } else if(selected$input[j]=="select"){
                         mychoices=unlist(strsplit(value,",",fixed=TRUE))
                         if(temp %in% c("colour","color","fill")){
                              mychoices=c(mychoices,colors()[!str_detect(colors(),mychoices)])
                         }
                         if(length(mychoices)>0)
                              mywidth=(((max(nchar(mychoices))*8)%/%100)+1)*100
                         else mywidth=100
                         argList=list(label=temp,mode="select",choices=mychoices,
                                      value=NULL,width=mywidth)
                         rv$result[[j]]<-callModule(textFunction,temp,argList=reactive(argList),
                                                    editCode=reactive(TRUE),settingData=reactive(settingData()))
                    }
                         
                    }
                    })
               }
          }
          #str(rv$result)

    })
    
     myfunction=reactive({
        count=0
        code<-""
        if(editCode()) code<-input$text
        myOptions<-rv$myArgs() 
        
        
        selected<-selectedData()
        if(!is.null(selected)) {
            count=nrow(selected)
        }
        if(count>0){
            
            tempcode=""
            result <- vector(mode = "list", length = count)
            for(i in 1:count){
                temp=selected$setting[i]
                value=selected$value[i]
                valuechoice=unlist(strsplit(value,","))
                valuechoice=str_trim(valuechoice)
              

                if(selected$input[i] %in% c("select","text")){
                    
                     # cat(paste0("\nrv$result[[",i,"]]()="))
                     # str(rv$result[[i]]())
                     #cat("class(rv$result[[i]])=",class(rv$result[[i]]),"\n")
                     resultCode<-NULL
                     resultCode<-tryCatch(rv$result[[i]](),error=function(e) "error")
                     #cat("resultCode=",resultCode,"\n")
                     if(selected$input[i]=="select") defaultValue=valuechoice[1]
                     else defaultValue=value
                     if(!is.null(resultCode)){
                         # cat("resultCode=",resultCode,"\n")
                         # cat("defaultValue=",defaultValue,"\n")
                     if(!identical(resultCode,defaultValue)){
                        if(tempcode!="") tempcode=paste0(tempcode,",")
                        if(selected$quoted[i]) tempcode=paste0(tempcode,temp,"='",resultCode,"'")
                        else tempcode=paste0(tempcode,temp,"=",resultCode)
                     }
                    }
                } else{
                     if(!is.null(input[[temp]])){
                    if(input[[temp]]!=value){
                        if(tempcode!="") tempcode=paste0(tempcode,",")
                        if(selected$quoted[i]) tempcode=paste0(tempcode,temp,"='",input[[temp]],"'")
                        else tempcode=paste0(tempcode,temp,"=",input[[temp]])
                    }
                     }
                }
            }
            
            if(editCode()) {
               code=str_extract(input$text,"[^\\(]*\\(")
               code=paste0(code,tempcode,")")
            } else{
                code<-tempcode 
            }
                 
            
        }
        code
    })
    return(myfunction)
}


