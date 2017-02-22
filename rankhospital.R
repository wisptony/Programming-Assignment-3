rankhospital<-function(State,outcome,num="best"){
        ##read outcome data
        data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",
                       stringsAsFactors = FALSE)
        newdata<-cbind.data.frame(data[,2],
                                  data[,7],
                                  data[,11],
                                  data[,17],
                                  data[,23])
        names<-c("hospital name","state","heart attack","heart failure",
                 "pneumonia")
        colnames(newdata)<-names
        ##check that state&outcome are valid
        if(!State %in% newdata[,"state"]){
                stop("invalid state", call. = FALSE)
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome", call. = FALSE)
        }   
        ##Return hospital name in that state with the given rank
        ##30-day death rate
        selecteddata<-subset(newdata,state==State)
        ordereddata<-selecteddata[order(selecteddata[,outcome],
                                        selecteddata[,"hospital name"],na.last=NA),]
        if(num=="best"){
                print(as.character(ordereddata[1,"hospital name"]))      
        }else if(num=="worst"){
                print(as.character(ordereddata[length(ordereddata[,outcome]),"hospital name"])) 
        }else{
                print(as.character(ordereddata[num,"hospital name"]))
        }
}