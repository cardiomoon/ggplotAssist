---
title: "Recursive Shiny Module for Functionals"
author: "Keon-Woong Moon"
date: "2017-11-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{textFunctinoInput}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




There are many functions that takes a function as an input : **Functionals**. To handle a functional in a shiny app, you have to make a shiny module that allows **recursive** call. I have included an recursive shiny module `textFunction` in my package ggplotAssist. The UI of textFunction shiny module is `textFunctionInput` and the server function is `textFunction`. I also included two toy shiny app to demonstrate the recursive shiny module.


## Prerequisite

You have to install the developmental version of R package `editData` from github.


```r
#install.packages("devtools")
devtools::install_github("cardiomoon/editData")
```

## Install package

You have to install the developmental version of `ggplotAssist` package from github. 


```r
devtools::install_github("cardiomoon/ggplotAssist")
```

## Example 1

After install the package `ggplotAssist`, you can run the first example app by the following R code.



```r
shiny::runApp(system.file('textFunctionExample',package='ggplotAssist'))
```

Enter `element_text()` in the textInput(1).

<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="90%" style="display: block; margin: auto;" />


You can select font family(2) or colour(3). You can enter size(4). To adjust margin, enter `margin()` at the margin textInput(5). 

<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/2.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="90%" style="display: block; margin: auto;" />

You can adjust dimensions of each margin(6,7) or ajdust default units of dimension(8). You can see the resultant R code for this function(arrow).


<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/3.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="90%" style="display: block; margin: auto;" />


## Example 2

You can use textFunctionInput as a selectInput. Please run the second example app by the following R code.



```r
shiny::runApp(system.file('textFunctionExample2',package='ggplotAssist'))
```
Select `guide_colorbar()` among the selectInput(arrow).


<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/4.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="90%" style="display: block; margin: auto;" />

You can enter title(9) or select title.position(10). You can set the title.theme by enter `element_text()` in the textInput(11).

<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/5.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="90%" style="display: block; margin: auto;" />

You can adjust font family(12) or font face(13). Also you can set the margin by entering `margin()` in the textInput(14).

<img src="https://raw.githubusercontent.com/cardiomoon/ggplotAssistFigures/master/textFunction/6.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" width="90%" style="display: block; margin: auto;" />


You can see the source R code at the github page of package ggplotAssist: [https://github.com/cardiomoon/ggplotAssist](https://github.com/cardiomoon/ggplotAssist).
