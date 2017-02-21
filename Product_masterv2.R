library(data.table)

setwd("/mnt/s3/Global_Forecasting/Affine/product_master_revised")

file_list<- c("Style_CEE_2014-2016.txt",
              "Style_NA_2014-2016.txt",
              "Style_JP_2014-2016.txt",
              "Style_EM_2014-2016.txt",
              "Style_GC_2014-2016.txt")

booking <- fread("//mnt//s3//Global_Forecasting//WHLSL_2014_2017.txt"
  ,select = c("StylCd","StylNm","Global_Category","Product_Engine","Gender_Desc","IconAndFrnchs",
                                      "SilhDesc", "SilhTypeDesc","SubClas","MSRP_USD","AAF_Qty")
                                      ,sep="~",header = T)
head(booking)
setnames(booking, "StylCd", "Style_display_code")
setnames(booking, "StylNm", "StyleName")
setnames(booking, "AAF_Qty", "NetSlsUnts_WTD")
setnames(booking, "Global_Category", "Global_Categoty")

Final_Style_sales <- NULL
Final_bad_styles <- NULL
Final_Style_franchise <- NULL
Final_Style_silhoute_desc <- NULL
Final_Style_silhoute_type_desc <- NULL
Final_Style_gender <- NULL
Final_Style_subclass <- NULL
Final_Style_global_category <- NULL
Final_Style_stylename <- NULL

i <- 0
for(i in 0:length(file_list))
{
  
  path <- paste("/mnt/s3/Global_Forecasting/",file_list[i],sep = "")
  ifelse(i==0, 
    POS <-  booking[,c("Style_display_code","Product_Engine","Global_Categoty", 
      "Gender_Desc","IconAndFrnchs","StyleName", "SilhDesc", "SilhTypeDesc","SubClas","MSRP_USD","NetSlsUnts_WTD")]
    ,POS <-  fread(path,select=c("Style_display_code","Product_Engine","Global_Categoty", 
      "Gender_Desc","IconAndFrnchs","StyleName", "SilhDesc", 
      "SilhTypeDesc","SubClas","MSRP_USD","NetSlsUnts_WTD"),header = T, sep = "~"))

#PRIMARY DATA TREATMENT#
POS<- POS[, NetSlsUnts_WTD:=as.numeric(NetSlsUnts_WTD)]
#POS<- POS[, OHInvUnts_WTD:=as.numeric(OHInvUnts_WTD)]
POS[is.na(NetSlsUnts_WTD), NetSlsUnts_WTD :=0]
#POS[is.na(OHInvUnts_WTD), OHInvUnts_WTD :=0]
POS[NetSlsUnts_WTD < 0, NetSlsUnts_WTD :=0]
#POS[OHInvUnts_WTD < 0, OHInvUnts_WTD :=0]


#########
POS <- POS[,.(NetSlsUnts_WTD = sum(NetSlsUnts_WTD)), by = .(Style_display_code,
  Product_Engine,Global_Categoty, Gender_Desc,
  IconAndFrnchs,StyleName, SilhDesc, SilhTypeDesc,SubClas)]


#####
POS$Style_display_code <- gsub(pattern = "[[:space:]]", replacement = "",POS$Style_display_code)

POS$Product_Engine <- toupper(POS$Product_Engine)
POS$IconAndFrnchs <- toupper(POS$IconAndFrnchs)
POS$SilhDesc <- toupper(POS$SilhDesc)
POS$SilhTypeDesc <- toupper(POS$SilhTypeDesc)
POS$Gender_Desc <- toupper(POS$Gender_Desc)
POS$SubClas <- toupper(POS$SubClas)
POS$Global_Categoty <- toupper(POS$Global_Categoty)
POS$StyleName <- toupper(POS$StyleName)

POS$Global_Categoty  <- gsub(","," ",POS$Global_Categoty)
POS$StyleName  <- gsub(","," ",POS$StyleName)


POS$IconAndFrnchs <- gsub(pattern = "[[:punct:]]", replacement = "",POS$IconAndFrnchs)
POS$SilhDesc <- gsub(pattern = "[[:punct:]]", replacement = "",POS$SilhDesc)
POS$SilhTypeDesc <- gsub(pattern = "[[:punct:]]", replacement = "",POS$SilhTypeDesc)
POS$Gender_Desc <- gsub(pattern = "[[:punct:]]", replacement = "",POS$Gender_Desc)
POS$SubClas <- gsub(pattern = "[[:punct:]]", replacement = "",POS$SubClas)
POS$Global_Categoty  <- gsub(pattern = "[[:punct:]]", replacement = "",POS$Global_Categoty)
POS$StyleName  <- gsub(pattern = "[[:punct:]]", replacement = "",POS$StyleName)

#####calculating bad styles###

abc <- POS[!(nchar(Style_display_code) == 6)]
Final_bad_styles <- rbind(Final_bad_styles,abc)
#POS<-POS[!(POS$Style_display_code=="*UNK*")]
POS <- POS[nchar(Style_display_code) == 6]
###################################

##################### PE and sales
Style_sales <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","Product_Engine")]

Final_Style_sales=rbind(Final_Style_sales,Style_sales)
Final_Style_sales <- Final_Style_sales[,j=list(sales = sum(sales)),
  by = c("Style_display_code","Product_Engine")]

#####franchise
Style_franchise <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","IconAndFrnchs")]
Final_Style_franchise <- rbind(Final_Style_franchise, Style_franchise)
Final_Style_franchise <- Final_Style_franchise[,j=list(sales = sum(sales)),
  by = c("Style_display_code","IconAndFrnchs")]

##silhoute desc
Style_silhoute_desc <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","SilhDesc")]
Final_Style_silhoute_desc <- rbind(Final_Style_silhoute_desc, Style_silhoute_desc)
Final_Style_silhoute_desc <- Final_Style_silhoute_desc[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SilhDesc")]

### silhoute type desc
Style_silhoute_type_desc <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","SilhTypeDesc")]
Final_Style_silhoute_type_desc <- rbind(Final_Style_silhoute_type_desc, Style_silhoute_type_desc)
Final_Style_silhoute_type_desc <- Final_Style_silhoute_type_desc[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SilhTypeDesc")]

## gender
Style_gender <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","Gender_Desc")]
Final_Style_gender <- rbind(Final_Style_gender, Style_gender)
Final_Style_gender <- Final_Style_gender[,j=list(sales = sum(sales)),
  by = c("Style_display_code","Gender_Desc")]

## sub class
Style_subclass <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","SubClas")]
Final_Style_subclass <- rbind(Final_Style_subclass, Style_subclass)
Final_Style_subclass <- Final_Style_subclass[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SubClas")]


## global category
Style_global_category <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","Global_Categoty")]
Final_Style_global_category <- rbind(Final_Style_global_category, Style_global_category)
Final_Style_global_category <- Final_Style_global_category[,j=list(sales = sum(sales)),
  by = c("Style_display_code","Global_Categoty")]

## style name
Style_stylename <- POS[,j=list(sales = sum(NetSlsUnts_WTD)),
  by = c("Style_display_code","StyleName")]
Final_Style_stylename <- rbind(Final_Style_stylename, Style_stylename)
Final_Style_stylename <- Final_Style_stylename[,j=list(sales = sum(sales)),
  by = c("Style_display_code","StyleName")]

#write.table(POS,file_list[i],sep=",",row.names=FALSE,quote=FALSE)
rm(POS, abc)
}
#warnings()
setwd("/mnt/s3/Global_Forecasting/Affine/product_master_revised")

write.table(Final_bad_styles,"Final_bad_styles.csv",sep=",",row.names=FALSE,quote=FALSE)

rm(Style_sales,Style_franchise,Style_silhoute_desc,Style_silhoute_type_desc,Style_gender,Style_subclass,Style_global_category,Style_stylename)
rm(booking)
Product_Master <- Final_Style_sales
#Final_Style_sales <- NULL


#Franchise
Final_Style_franchise<-Final_Style_franchise[!(IconAndFrnchs=="UNK")]
Final_Style_franchise <- Final_Style_franchise[,j=list(sales = sum(sales)),by = c("Style_display_code","IconAndFrnchs")]
Final_Style_franchise <- Final_Style_franchise[,count := .N, by =Style_display_code ]
duplicate_franchise <- Final_Style_franchise[count >1]
Final_Style_franchise <- Final_Style_franchise[count == 1]
Final_Style_franchise <-Final_Style_franchise[,count:=NULL]
Final_Style_franchise <-Final_Style_franchise[,sales:=NULL]
#write.table(duplicate_franchise,"duplicate_franchise.csv",sep=",",row.names=FALSE,quote=FALSE)

#gender
#head(Final_Style_gender)
Final_Style_gender<-Final_Style_gender[!(Gender_Desc=="UNK")]
Final_Style_gender <- Final_Style_gender[,j=list(sales = sum(sales)),by = c("Style_display_code","Gender_Desc")]
Final_Style_gender <- Final_Style_gender[,count := .N, by =Style_display_code ]
duplicate_gender <- Final_Style_gender[count > 1]
Final_Style_gender <- Final_Style_gender[count == 1]
Final_Style_gender <-Final_Style_gender[,count:=NULL]
Final_Style_gender <-Final_Style_gender[,sales:=NULL]
#write.table(duplicate_gender,"duplicate_gender.csv",sep=",",row.names=FALSE,quote=FALSE)

#globalcategory
#head(Final_Style_global_category)
Final_Style_global_category<-Final_Style_global_category[!(Global_Categoty=="UNK")]
Final_Style_global_category <- Final_Style_global_category[,j=list(sales = sum(sales)),
  by = c("Style_display_code","Global_Categoty")]
Final_Style_global_category <- Final_Style_global_category[,count := .N, by =Style_display_code ]
duplicate_global_category <- Final_Style_global_category[count > 1]
Final_Style_global_category <- Final_Style_global_category[count == 1]
Final_Style_global_category <-Final_Style_global_category[,count:=NULL]
Final_Style_global_category <-Final_Style_global_category[,sales:=NULL]
#write.table(duplicate_global_category,"duplicate_global_category.csv",sep=",",row.names=FALSE,quote=FALSE)

#silhoute
#head(Final_Style_silhoute_desc)
Final_Style_silhoute_desc<-Final_Style_silhoute_desc[!(SilhDesc=="UNK")]
Final_Style_silhoute_desc <- Final_Style_silhoute_desc[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SilhDesc")]
Final_Style_silhoute_desc <- Final_Style_silhoute_desc[,count := .N, by =Style_display_code ]
duplicate_silhoute_desc <- Final_Style_silhoute_desc[count > 1]
Final_Style_silhoute_desc <- Final_Style_silhoute_desc[count == 1]
Final_Style_silhoute_desc <-Final_Style_silhoute_desc[,count:=NULL]
Final_Style_silhoute_desc <-Final_Style_silhoute_desc[,sales:=NULL]
#write.table(duplicate_silhoute_desc,"duplicate_silhoute_desc.csv",sep=",",row.names=FALSE,quote=FALSE)

#silhoute_type_desc
#head(Final_Style_silhoute_type_desc)
Final_Style_silhoute_type_desc<-Final_Style_silhoute_type_desc[!(SilhTypeDesc=="UNK")]
Final_Style_silhoute_type_desc <- Final_Style_silhoute_type_desc[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SilhTypeDesc")]
Final_Style_silhoute_type_desc <- Final_Style_silhoute_type_desc[,count := .N, by =Style_display_code ]
duplicate_silhoute_type_desc <- Final_Style_silhoute_type_desc[count > 1]
Final_Style_silhoute_type_desc <- Final_Style_silhoute_type_desc[count == 1]
Final_Style_silhoute_type_desc <-Final_Style_silhoute_type_desc[,count:=NULL]
Final_Style_silhoute_type_desc <-Final_Style_silhoute_type_desc[,sales:=NULL]
#write.table(duplicate_silhoute_type_desc,"duplicate_silhoute_type_desc.csv",sep=",",row.names=FALSE,quote=FALSE)

#subclass
#head(Final_Style_subclass)
Final_Style_subclass<-Final_Style_subclass[!(SubClas=="UNK")]
Final_Style_subclass <- Final_Style_subclass[,j=list(sales = sum(sales)),
  by = c("Style_display_code","SubClas")]
Final_Style_subclass <- Final_Style_subclass[,count := .N, by =Style_display_code ]
duplicate_subclass <- Final_Style_subclass[count > 1]
Final_Style_subclass <- Final_Style_subclass[count == 1]
Final_Style_subclass <-Final_Style_subclass[,count:=NULL]
Final_Style_subclass <-Final_Style_subclass[,sales:=NULL]
#write.table(duplicate_subclass,"duplicate_subclass.csv",sep=",",row.names=FALSE,quote=FALSE)

#stylename
#head(Final_Style_stylename)
Final_Style_stylename<-Final_Style_stylename[!(StyleName=="UNK")]
Final_Style_stylename <- Final_Style_stylename[,j=list(sales = sum(sales)),
  by = c("Style_display_code","StyleName")]
Final_Style_stylename <- Final_Style_stylename[,count := .N, by =Style_display_code ]
duplicate_stylename <- Final_Style_stylename[count > 1]
Final_Style_stylename <- Final_Style_stylename[count == 1]
Final_Style_stylename <-Final_Style_stylename[,count:=NULL]
Final_Style_stylename <-Final_Style_stylename[,sales:=NULL]
#write.table(duplicate_stylename,"duplicate_stylename.csv",sep=",",row.names=FALSE,quote=FALSE)

setorder(duplicate_franchise, Style_display_code, -sales)
abc <- duplicate_franchise[, .I[which.max(sales)], by=Style_display_code]
correct_franchise <- duplicate_franchise[abc$V1]
correct_franchise <-correct_franchise[,count:=NULL]
correct_franchise <-correct_franchise[,sales:=NULL]


setorder(duplicate_global_category, Style_display_code, -sales)
abc <- duplicate_global_category[, .I[which.max(sales)], by=Style_display_code]
correct_global_category <- duplicate_global_category[abc$V1]
correct_global_category <- correct_global_category[,count:=NULL]
correct_global_category <- correct_global_category[,sales:=NULL]

setorder(duplicate_stylename, Style_display_code, -sales)
abc <- duplicate_stylename[, .I[which.max(sales)], by=Style_display_code]
correct_stylename <- duplicate_stylename[abc$V1]
correct_stylename <- correct_stylename[,count:=NULL]
correct_stylename <- correct_stylename[,sales:=NULL]

# write.table(correct_stylename,"correct_stylename.csv",sep=",",row.names=FALSE,quote=FALSE)
# write.table(correct_franchise,"correct_franchise.csv",sep=",",row.names=FALSE,quote=FALSE)
# write.table(correct_global_category,"correct_global_category.csv",sep=",",row.names=FALSE,quote=FALSE)

Final_Style_franchise <- rbind(Final_Style_franchise, correct_franchise)
Final_Style_global_category <- rbind(Final_Style_global_category, correct_global_category)
Final_Style_stylename <- rbind(Final_Style_stylename, correct_stylename)


setkey(Product_Master,Style_display_code)
setkey(Final_Style_franchise, Style_display_code)
setkey(Final_Style_silhoute_desc, Style_display_code)
setkey(Final_Style_silhoute_type_desc, Style_display_code)
setkey(Final_Style_gender, Style_display_code)
setkey(Final_Style_subclass, Style_display_code)
setkey(Final_Style_global_category, Style_display_code)
setkey(Final_Style_stylename, Style_display_code)

Product_Master <- Final_Style_franchise[Product_Master]
Product_Master <- Final_Style_silhoute_desc[Product_Master]
Product_Master <- Final_Style_silhoute_type_desc[Product_Master]
Product_Master <- Final_Style_global_category[Product_Master]
Product_Master <- Final_Style_gender[Product_Master]
Product_Master <- Final_Style_subclass[Product_Master]
Product_Master <- Final_Style_stylename[Product_Master]

rm(list=setdiff(ls(), c("Product_Master","Final_Style_stylename", "Final_bad_styles")))
write.table(Final_Style_stylename,"Final_Style_stylename.csv",sep=",",row.names=FALSE,quote=FALSE)
Product_Master <- Product_Master[,sales:=NULL]

###### AA and launch styles
AA_launch_styles <- fread("/mnt/s3/Global_Forecasting/Product_Master/AA_launch_styles.csv",header = T,sep=",")
setkey(Product_Master, Style_display_code)
setkey(AA_launch_styles, Style)
Product_Master <- AA_launch_styles[Product_Master]
Product_Master[is.na(Type), Type := "Regular"]


write.table(Product_Master,"Product_Master.csv",sep=",",row.names=FALSE,quote=FALSE)

#abc <- fread("/mnt/s3/Global_Forecasting/Affine/product_master_revised/Product_Master.csv",header = T,sep=",")

# ####Style name cleaning############
# library(stringr)
# 
# Final_Style_stylename <- Final_Style_stylename[, split:=strsplit(StyleName, " ")]
# 
# x <- Final_Style_stylename$split
# frequency <- as.data.table(table(unlist(x)))
# setorder(frequency, -N)
# write.table(frequency,"Style_name_frequency.csv",sep=",",row.names=FALSE,quote=FALSE)
# 
# top_order <- frequency$V1[1:2]
# col_data <- ncol(Final_Style_stylename)
# for(i in 1:length(top_2_order)){
#   aa <- rep(0, nrow(Final_Style_stylename))
#   b <- str_detect(Final_Style_stylename$split,top_2_order[i])
#   aa[b] <- 1
#   Final_Style_stylename <- data.table(Final_Style_stylename, aa)
#   colnames(Final_Style_stylename)[i+col_data] <- paste(top_2_order[i], sep = '_')
# }

################################################# latest code update ##################
###hardcoding to remove duplicates as per the previous product master and replace them with new PM Values ####

# setwd("/mnt/s3/Global_Forecasting/Affine/product_master_revised")
# Product_Master<-fread("Product_Master.csv",header=T)
# Product_Master<-Product_Master[!duplicated(Product_Master), ]
# Product_Master <- Product_Master[,count := .N, by =Style ]
# pm_dup <- Product_Master[count == 2]
# Product_Master<-Product_Master[count == 1]
# 
# setwd("/mnt/s3/Global_Forecasting/Product_Master")
# pm<-fread("Product_Master.csv",header=T)
# pm$flag<-NULL
# pm_dup<-as.data.frame(pm_dup)
# pm<-as.data.frame(pm)
# xy<-merge(x=pm_dup,y=pm[,c("Style","Global_Categoty","IconAndFrnchs","flag")],by=c("Style","Global_Categoty","IconAndFrnchs"), all.x = TRUE)
# xy<-xy[is.na(xy$flag),]
# xy$flag<-NULL
# Product_Master<-rbind(Product_Master,xy)
# setwd("/mnt/s3/Global_Forecasting/Affine/product_master_revised")
# write.table(Product_Master,"Product_Master.csv",sep=",",row.names=FALSE,quote=FALSE)
# write.csv(pm,"Product_Master_old.csv")
# getwd()
