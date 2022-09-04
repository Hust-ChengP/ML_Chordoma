library(magrittr)
library(dplyr)
library(ggplot2)
#library(gridExtra)
#library(stringr)

get_data <- function(file_path = '../data/original.csv'){
  origin_data <- read.csv(file_path,check.names = F)
  data <- origin_data %>% 
    filter(`Primary Site` %in% c(410:414)) %>%
    filter(`First malignant primary indicator`== 'Yes') %>%
    # filter(`RX Summ--Surg Prim Site (1998+)` %in% c(30, 41, 40, 26, 0, 42, 25, 53, 50, 51, 52, 54, 19)) %>%
    #filter(!grepl('Blank|NA|UNK|IVNOS',`Derived AJCC Stage Group, 6th ed (2004-2015)`)) %>%
    #filter(!grepl('Paget',`ICD-O-3 Hist/behav`)) %>%
    #filter(!grepl('Blank|NA|UNK|unknow|^Surgery',`RX Summ--Systemic/Sur Seq`))%>%
    filter(`Survival months` != 'Unknown') %>%
    # filter(`Marital status at diagnosis`!='Unknown') %>%
    # filter(`Race recode (W, B, AI, API)`!='Unknown')%>%
    # filter(`Grade (thru 2017)` != 'Unknown') %>%
    # filter(`CS tumor size (2004-2015)`<=989 && `CS tumor size (2004-2015)`>0) %>%
    #filter(`CS mets at dx (2004-2015)` %in% c(0,30,35,40,50)) %>%
    filter(!grepl('Blank',`CS extension (2004-2015)`)) %>%
    #filter(!grepl('999',`CS extension (2004-2015)`)) %>%
    mutate(`Age recode with single ages and 100+`=gsub('years','',`Age recode with single ages and 100+`))%>%
    subset(select = c(
      `Age recode with single ages and 100+`,
      `Sex`,
      `Marital status at diagnosis`,
      `Race recode (White, Black, Other)`,
      `ICD-O-3 Hist/behav`,
      `Primary Site - labeled`,
      `Derived AJCC Stage Group, 6th ed (2004-2015)`,
      `Derived AJCC T, 6th ed (2004-2015)`,
      `Derived AJCC N, 6th ed (2004-2015)`,
      `Derived AJCC M, 6th ed (2004-2015)`,
      `Grade (thru 2017)`,
      `RX Summ--Surg Prim Site (1998+)`,
      `RX Summ--Systemic/Sur Seq`,
      `RX Summ--Surg/Rad Seq`,
      `Chemotherapy recode (yes, no/unk)`,
      `CS tumor size (2004-2015)`,
      `Total number of in situ/malignant tumors for patient`,
      `CS extension (2004-2015)`,
      `CS mets at dx (2004-2015)`,
      `Median household income inflation adj to 2019`,
      `Survival months`,
      `Vital status recode (study cutoff used)`
      )) %>%
    setNames(
      c(
        'Age','Gender','Marital status','Race','Histological type','Primary site','Stage','AJCC T','AJCC N','AJCC M','Grade','Surgery','Systemic therapy',
        'Radiotherapy','Chemotherapy','Tumor size','Number of tumors','Tumor extension','Distant metastasis','Median income',
        'Survival months','Status'
      )
    ) %>%
    mutate(
      `Tumor size` = replace(`Tumor size`,`Tumor size` %in% append(seq(989,999,1),'Blank(s)'),NA),
      Stage = replace(Stage,grepl('UNK|Blank',Stage),NA),
      `AJCC T` = replace(`AJCC T`,grepl('X',`AJCC T`),NA),
      `AJCC N` = replace(`AJCC N`,grepl('X',`AJCC N`),NA),
      `AJCC M` = replace(`AJCC M`,grepl('X',`AJCC M`),NA),
      Grade = replace(Grade,grepl('Unknown|Blank',Grade),NA),
      Surgery = replace(Surgery,grepl('90|99',Surgery),NA) ,
      `Tumor extension` = replace(`Tumor extension`,grepl('999|Blanks',`Tumor extension`),NA) ,
      `Distant metastasis` = replace(`Distant metastasis`,grepl('99|Blank',`Distant metastasis`),NA) 
    ) %>%
    mutate(
      Age = as.numeric(Age),
      Gender = factor(Gender,levels = c('Female', 'Male')),
      `Marital status` = grepl('^Married',`Marital status`) %>% factor(labels = c('Not married','Married')),
      Race = Race %>% factor(levels = c('White', 'Black', 'Other (American Indian/AK Native, Asian/Pacific Islander)'),
                             labels = c('White', 'Black', 'Other')),
      `Histological type` = `Histological type` %>% factor(levels = c("9370/3: Chordoma, NOS", 
                                                                      "9371/3: Chondroid chordoma", 
                                                                      "9372/3: Dedifferentiated chordoma"),
                                                           labels = c("Conventional chordoma", 
                                                             "Chondroid chordoma",
                                                             "Dedifferentiated chordoma"
                                                             )),
      `Primary site` = `Primary site` %>% factor(
        levels = c("C41.0-Bones of skull and face and associated joints", 
                   "C41.2-Vertebral column", 
                   "C41.4-Pelvic bones, sacrum, coccyx and associated joints"
        ),
        labels = c('Bones of skull and face','Vertebral column','Pelvic bones, sacrum, coccyx')
      ),
      Stage = Stage %>% factor(
        levels = c("IA", "IB","IIA", "IIB","III",  "IVA",  "IVB"),
        labels = c("IA", "IB","IIA", "IIB","III",  "IVA",  "IVB")
      ),
      `AJCC T` = `AJCC T` %>% factor(
        levels = c("T1", "T2", "T3"),
        labels = c("T1", "T2", "T3")
      ),
      `AJCC N` = `AJCC N` %>% factor(
        levels = c("N0",  "N1"),
        labels = c("N0",  "N1")
      ),
      `AJCC M` = `AJCC M` %>% factor(
        levels = c("M0", "M1b",  "M1a", "M1NOS"),
        labels = c("M0", "M1",  "M1", "M1")
      ),
      Grade = Grade %>% factor(
        levels = c("Well differentiated; Grade I","Moderately differentiated; Grade II",  
                   "Poorly differentiated; Grade III", "Undifferentiated; anaplastic; Grade IV"),
        labels = c("Well differentiated","Moderately differentiated",  
                   "Poorly differentiated", "Undifferentiated")
        ),
      Surgery = Surgery %>% factor(
        levels = c(0, 
                   15, 19, 25,
                   26,
                   30, 40, 41, 42, 50, 51, 52, 53, 54
                   ),
        labels = c('None',
                   rep('Local excision',3),
                   rep('Partial resection',1),
                   rep('Radical excision',9)
                   )
      ),
      `Systemic therapy` = `Systemic therapy` %>% factor(
        levels = c("No systemic therapy and/or surgical procedures", "Systemic therapy both before and after surgery", 
                   "Systemic therapy before surgery", "Systemic therapy after surgery"),
        labels = c("Not", "before surgery", "after surgery", "both before and after surgery")
      ),
      Radiotherapy = grepl('^No',Radiotherapy) %>% `!` %>% factor(labels = c('Not','Yes')),
      Chemotherapy = grepl('^Yes',Chemotherapy) %>% factor(labels = c('Not','Yes')),
      `Tumor size` = `Tumor size` %>% as.numeric(),
      `Number of tumors` = `Number of tumors` %>% as.numeric() %>% cut(breaks=c(-Inf,1,Inf),labels=c('1','> 1')),
      `Tumor extension` = `Tumor extension` %>% factor(
        levels = c("100", "200",
                   "600", "400",  "300", "350", "310",
                   "700", "800", "820", "850"
                   ),
        labels = c(
          rep('No break in periosteum',2),
          rep('Extension beyond periosteum',5),
          rep('Further extension',4)
        )
      ),
      `Distant metastasis` = `Distant metastasis` %>% factor(
        levels = c("0",
                   "30",
                   "35", "40", "50"),
        labels = c(
          'Not',
          rep('Yes',4)
        )
      ),
      `Median income` = `Median income` %>% factor(
        levels = c("< $35,000", "$35,000 - $39,999", "$40,000 - $44,999","$45,000 - $49,999",
                   "$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999",
                   "$65,000 - $69,999", "$70,000 - $74,999", "$75,000+"),
        labels = c(rep('< 60K',6), 
                   rep('60K - 75K',3),
                   "> 75K")
        
      ),
      `Survival months` = `Survival months` %>% as.numeric,
      Status = Status %>% factor(levels = c('Alive','Dead'))
      )
  
  plot_distribution <- function(data_){
    plot_list <- list()
    for (colname in colnames(data_)){
      print(colname)
      if (data_[[colname]] %>% is.numeric){
        p <- ggplot(data_,mapping=aes(x=.data[[colname]])) + geom_histogram()
      }else{
        p <- ggplot(data_,aes(x=.data[[colname]])) + geom_bar() + 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 
      }
      p <- p +
        theme_bw()+ scale_color_npg()+ theme(axis.text.x = element_text(angle = 0,hjust=0.5,size = 5))
      plot_list <- append(plot_list,list(p))
    }
    grid.arrange(grobs = plot_list, ncol = 4)
  }
  # data %>% plot_distribution()
  # write.csv(data,'data1.csv')
  data
    
}
  
