rankall<-function(outcome,num="best"){
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
        factor_vector<-newdata$state
        ##check that state&outcome are valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome", call. = FALSE)
        }
        ##For each state, find the hospital names and the given rank
        order_outcome<-function(df){
                df[order(df[,outcome],df[,"hospital name"],na.last=NA),]}
        sort_outcome<-function(df){
                if(num=="best"){
                        df[1,c("hospital name","state")]
                }else if(num=="worst"){
                        df[length(df[,"hospital name"]),c("hospital name","state")]
                }else{
                        df[num,c("hospital name","state")]
                }
        }
        splitdata<-split(newdata,factor_vector)
        orderlist<-lapply(splitdata,order_outcome)
        sort_list<-lapply(orderlist,sort_outcome)
        result<-as.data.frame(do.call(rbind,sort_list))
        return(result)
        
}