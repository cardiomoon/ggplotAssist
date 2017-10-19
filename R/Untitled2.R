x="theme(legend.background=element_rect(fill='red',size=0),length=2,legend.position='top',z=unit(0.25,'mm'))"
x

extractArgs=function(x){
    (x1=str_replace(x,"\\)$",""))
    (x2=str_replace(x1,"[:alpha:]*\\(",""))
    str_view(x2,".*\\(.*\\),")
    str_view_all(x2,"[^(.*\\(.*)],")
    3=unlist(str_extract_all(x2,".*\\(.*\\),|.+$"))
    str_replace_all(x3,",$","")
}
extractArgs(x)
setdiff2=function(args,x){
    result=args
    if(length(args)>0){
        args2=str_trim(str_replace_all(args,"=.*",""),side="both")    
        pos=c()
        for(i in 1:length(args2)){
            if(args2[i]==x) pos=c(pos,i)
        }
        pos
        if(!is.null(pos)) result=args[-pos]
    }
    result
}
