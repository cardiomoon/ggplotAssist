library(ggplotAssist)
library(shiny)
library(markdown)
library(stringr)
# Only run examples in interactive R sessions
if(interactive()){
    ui=fluidPage(
        h3("Recursive Shiny Module for Functionals"), 
        hr(),
        HTML(markdownToHTML(fragment.only=TRUE, 
                            text="There are many functions that takes a function as an input : `Functionals`. 
                            To handle a functional in a shiny app, you have to make a shiny module that allows `recursive` call.
                            I have included an recursive shiny module `textFunction` in my package ggplotAssist. 
                            The UI of textFunction shiny module is `textFunctionInput` and the server function is `textFunction`.
                            Please try to select one of the functions in the following selectInput.")),
        hr(),
        textFunctionInput("select"),
        hr(),
        textOutput("text")
    )
    server=function(input,output,session){
        rv=reactiveValues()
        
        filename <- paste0(system.file(package="ggplotAssist"),"/textFunctionExample/setting.csv")
        rawData=read.csv(filename,stringsAsFactors = FALSE)
        settingData=splitData(rawData,"setting")
        settingData=splitData(settingData,"geom")
        rv$argList<-list(label="Select function",mode="select",
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
