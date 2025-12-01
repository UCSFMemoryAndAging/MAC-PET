#Prepare data by removing duplicates and ids with no cl available-----
prep_baseline_data<-function(data){
  data<-data%>%
    group_by(id)%>%
    slice(1)%>%
    ungroup()%>%
    subset(is.na(cl)==F)%>%
    distinct()

  #check male variable-----
  if (nrow(subset(data,is.na(data$male)==T))>0){
    cat("Sex info is missing!\n")
    print(data[is.na(data$male)==T, ])
    stop("Please check sex variable for missing data.")
  }
  valid<-all(data$male %in% c(0,1))
  if (valid==F) {
    cat("Some values in column male are outside the range\n")
    print(data[!(data$male %in% c(0, 1)), ])
    stop("Please check values in sex variable.")
  }
  
  #check age variable----
  if (nrow(subset(data,is.na(data$age)==T))>0){
    cat("Age info is missing!\n")
    print(data[is.na(data$age)==T, ])
    stop("Please check age variable for missing data.")
  }
  valid<-all(data$age >= 0 & data$age <= 120)
  if (valid==F) {
    cat("Some values in column age are outside the range\n")
    print(data[!data$age >= 0 | !data$age <= 120, ])
    stop("Please check age range.")
  }
  
  #check mmse variable----
  if ("mmse" %in% colnames(data)){
    valid<-all(data$mmse >= 0 & data$mmse <= 30 | is.na(data$mmse))
    if (valid==F) {
      cat("Some values in column mmse are outside the range\n")
      print(data[!data$mmse >= 0 | !data$mmse <= 30, ])
      stop("Please check mmse range.")
    }
  }
  
  #check cdr variable----
  if ("cdr" %in% colnames(data)){
    valid<-all(data$cdr %in% c(0,0.5,1,2,3) | is.na(data$cdr))
    if (valid==F) {
      cat("Some values in column cdr are outside the range\n")
      print(data[!(data$cdr %in% c(0,0.5,1,2,3)), ])
      stop("Please check cdr range.")
    }
  }
  
  #harmonize tracer names
  if (nrow(subset(data,is.na(data$tracer)==T))>0){
    cat("Tracer info is missing!\n")
    print(data[is.na(data$tracer)==T, ])
    stop("Please check tracer variale for missing values.")
  }
  
  tracer_mapping<-read.csv("metadata/tracers.csv")
  lookup <- setNames(tracer_mapping$name_out, tracer_mapping$name_in)
  data$tracer_lower <- tolower(data$tracer)
  data$tracer <- lookup[data$tracer_lower]
  if (nrow(subset(data,is.na(data$tracer)==T))>0){
    cat("Couldn't match tracer name to existing convention.\n")
    print(data[is.na(data$tracer)==T, ])
    stop("Please check tracer naming convention.")
  }
  
  #harmonize apoe4 carriers
  if ("apoe" %in% colnames(data)){
    apoe_mapping<-read.csv("metadata/apoe4.csv")
    lookup <- setNames(apoe_mapping$name_out, apoe_mapping$name_in)
    data$apoe4_status <- tolower(data$apoe)
    data$apoe4_status <- lookup[data$apoe4_status]
  }
  
  #harmonize visual reads
  if ("visual_read" %in% colnames(data)){
    vr_mapping<-read.csv("metadata/visual_read.csv")
    lookup <- setNames(vr_mapping$name_out, vr_mapping$name_in)
    data$visual_read <- tolower(data$visual_read)
    data$visual_read <- lookup[data$visual_read]
  }
  
  #harmonize dx
  if ("dx" %in% colnames(data)){
    dx_mapping<-read.csv("metadata/dx.csv")
    lookup <- setNames(dx_mapping$name_out, dx_mapping$name_in)
    data$dx <- tolower(data$dx)
    data$dx <- lookup[data$dx]
  }
  dataset_clean<-subset(data, select = -c(tracer_lower))
}






    
