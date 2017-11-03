library(ggplotAssist)
library(shiny)
# Only run examples in interactive R sessions
if(interactive()){
    ui=fluidPage(
        textFunctionInput("select"),
        textOutput("text")
    )
    server=function(input,output,session){
        rv=reactiveValues()
        
        filename <- paste0(system.file(package="ggplotAssist"),"/textFunctionExample/setting.csv")
        rawData=read.csv(filename,stringsAsFactors = FALSE)
        settingData=splitData(rawData,"setting")
        settingData=splitData(settingData,"geom")
        rv$argList<-list(label="Select fucntion",mode="select",
                         choices=c("element_text()","element_line()","guide_colorbar()","guide_legend()"),width=200
                         )
        result=callModule(textFunction,"select",argList=reactive(rv$argList),
                          editCode=reactive(TRUE),settingData=reactive(settingData))
        output$text=renderText({
            result()
        })
    }
    shinyApp(ui,server)
}
