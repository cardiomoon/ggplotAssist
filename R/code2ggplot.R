#' Separate Code
#' 
#' @param code R code to separate 
#' 
#' @importFrom stringr str_c
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

#' Split Code
#' 
#' @param code R code to split
#' 
#' @importFrom stringr str_replace_all
splitCode<-function(code){
    (codes=stringr::str_replace_all(code,"\n",""))
    (temp=unlist(strsplit(codes,"+",fixed=TRUE)))
    temp=temp[nchar(temp)>0]
    temp
}

#' Make ggplot2 code
#' 
#' @param code R code to make ggplot
code2ggplot=function(code){
    codes=splitCode(code)
    if(length(codes)>0) selectMaxValid(codes)
}

#' Select maximally valid R code
#' @param codes R codes to select 
selectMaxValid=function(codes){
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

