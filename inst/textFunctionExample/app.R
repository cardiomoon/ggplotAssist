 library(ggplotAssist)
 library(shiny)
# Only run examples in interactive R sessions
if(interactive()){
 ui=fluidPage(
    textFunctionInput("text"),
    textOutput("text")
)
server=function(input,output,session){
    rv=reactiveValues()
    
    filename <- paste0(system.file(package="ggplotAssist"),"/textFunctionExample/setting.csv")
    rawData=read.csv(filename,stringsAsFactors = FALSE)
    settingData=splitData(rawData,"setting")
    settingData=splitData(settingData,"geom")
    rv$argList<-list(label="text",mode="text",value="",choices=NULL,width=200,
                     bg="lightcyan",placeholder="element_text()")
    result=callModule(textFunction,"text",argList=reactive(rv$argList),
                      editCode=reactive(TRUE),settingData=reactive(settingData))
    output$text=renderText({
        result()
    })
}
shinyApp(ui,server)
}
 
 