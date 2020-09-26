rm(list = ls()) # removes all variables in r
library("plyr")

setwd("/Users/noahferrel/Desktop")
codes = read.csv("OpioidCodesOnly.csv", header = T)
load("RFV_codes.RDA")
load("ICD9_codes.RDA")
load("testdf.Rda")
a = df_char
b = df_fac
c = df_int
d = df_num

#B.)
op_codes = as.vector(codes)[,1]
op_codes = op_codes[!op_codes %in% c("d04766","a11242","d03826","n09045","n11008")]
op_codes = unique(op_codes) 

setwd("/Users/noahferrel/Desktop/Fall 2018/Math 370-R/CDC 10 Years of Data")
file_names = list.files()

E = lapply(list.files(),function(x) read.csv(x,header = T))
e = E

w = list(
  lapply(E,function(x) names(x)[grep("DRUGID", names(x))]),
  lapply(E,function(x) names(x)[grep("CONTSUB", names(x))]),
  lapply(1:10,function(x) if(x<=8){c("DIAG1","DIAG2","DIAG3")}else{c("DIAG1","DIAG2","DIAG3","DIAG4","DIAG5")} ),
  lapply(1:10,function(x) if(x<=8){c("RFV1","RFV2","RFV3")}else{c("RFV1","RFV2","RFV3","RFV4","RFV5")} )
)

combine_columns = function(df_L,col_L){
  sub_df_L = mapply(function(x,y) x[,y] ,df_L,col_L ) 
  new_df = do.call(rbind.fill,sub_df_L)
  return(new_df)
}

newDF = do.call(cbind, lapply(w,function(x) combine_columns(e,x) ))

whitespace2NA = function(vec){
  myclass = class(vec)
  vec =  as.character(vec)
  vec[trimws(vec)==""] = NA
  x = get(paste0("as.",myclass))
  vec = x(vec)
  return(vec)
}

newDF = as.data.frame(lapply(newDF,function(x)   whitespace2NA(x) ))

setwd("/Users/noahferrel/Desktop")
load("new_names.Rda")

names(newDF) = new_names

# a)
dash_to_0 = function(v){
  g = class(v)
  v = as.character(v)
  v = gsub("-", "0", v)
  x = get(paste0("as.",g))
  v= x(v)
  return (v)
}

newDF = as.data.frame(lapply(newDF,function(x)  dash_to_0(x) ))

setwd("/Users/noahferrel/Desktop")
save(newDF,file = "newDF.Rda")

diabetes = Diabetes_ICD9[[1]]
D = newDF[,c("DIAG1","DIAG2","DIAG3","DIAG4","DIAG5")]

#scratch work
C = as.data.frame(sapply(D, function(x) (x %in% diabetes)*1))
Diabetes = (rowSums(C)>0)*1
#the function of work for any df and vec

bicol = function(df,myvec){
  binat = sapply(df, function(x) (x %in% myvec)*1 )
  return ((rowSums(binat)>0)*1)
}

bicol(D,diabetes)
as = bicol(a,c(1,2))
af = bicol(b,c(1,2))
ty = bicol(c,c(1,2))
hj = bicol(d,c(1,2))
dfpo = as.data.frame(cbind(as,af,ty,hj))
as.data.frame(cbind(a,b,c,d))

ab = bicols(a,list(c(1,2)))

all(as == ab)

# 



bicols = function(myDF, mylist){
  bicol = function(df,myvec){
    binat = sapply(df, function(x) (x %in% myvec)*1 )
    return ((rowSums(binat)>0)*1)
  }
  biDF = sapply(mylist, function(x) bicol(myDF, x))
  
  return(biDF)
}

#d.)
newDF_DIAG = newDF[names(newDF)[grep("DIAG",names(newDF))]]
p_new = newDF[names(newDF)[grep("DIAG",names(newDF))]]
Diabetes_ICD9 = as.data.frame(bicols(newDF_DIAG,Diabetes_ICD9))
Alcohol_df = as.data.frame(bicols(newDF_DIAG,Alcohol_ICD9))
pain_df = as.data.frame(bicols(newDF_DIAG,Pain_ICD9))
Mental_df = as.data.frame(bicols(newDF_DIAG,Mental_ICD9))

BIDATA = cbind(Alcohol_df,pain_df,Mental_df)


kool = as.data.frame(bicols(p_new,Pain_ICD9))

#e.)
newDF_RFV = newDF[names(newDF)[grep("RFV",names(newDF))]]
codes_RFV = as.data.frame(bicols(newDF_RFV,codes_RFV))



#f.)

lOP =  list(OP=op_codes)
DrugIdDf = newDF[names(newDF)[grep("DRUGID",names(newDF))]]
OP = bicol(DrugIdDf,op_codes)
OP1 = bicols(DrugIdDf,lOP)
OP = as.data.frame(OP)

#g.)

list(s2=2,s3=3,s4=4,s5=5,s6=6)
