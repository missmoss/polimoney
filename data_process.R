library("dplyr")
library("ggplot2")
pm_cand_raw <- read.csv("A05_basic_all.csv")
pm_all_raw <- read.csv("A02_company_all_public.csv")
cand <- read.csv("candidates.csv")

levels(pm_cand_raw$推薦政黨)
# [1] "大愛憲改聯盟"       "泛盟黨"             "健保免費連線"       "軍公教聯盟黨"      
# [5] "勞工黨"             "綠黨社會民主黨聯盟" "民國黨"             "民主進步黨"        
# [9] "親民黨"             "人民民主陣線"       "社會福利黨"         "時代力量"          
# [13] "樹黨"               "台灣團結聯盟"       "無黨籍"             "無黨團結聯盟"      
# [17] "新黨"               "信心希望聯盟"       "中國國民黨"         "中國生產黨"        
# [21] "中華民國機車黨"     "中華統一促進黨"     "自由台灣黨"

levels(pm_cand_raw$推薦政黨) <- c(levels(pm_cand_raw$推薦政黨)[1:5] 
                              , "綠社盟" #6
                              , levels(pm_cand_raw$推薦政黨)[7:21]
                              , "中華統促黨" #22
                              , levels(pm_cand_raw$推薦政黨)[23])

levels(pm_cand_raw$推薦政黨)
# [1] "大愛憲改聯盟"   "泛盟黨"         "健保免費連線"   "軍公教聯盟黨"   "勞工黨"        
# [6] "綠社盟"         "民國黨"         "民主進步黨"     "親民黨"         "人民民主陣線"  
# [11] "社會福利黨"     "時代力量"       "樹黨"           "台灣團結聯盟"   "無黨籍"        
# [16] "無黨團結聯盟"   "新黨"           "信心希望聯盟"   "中國國民黨"     "中國生產黨"    
# [21] "中華民國機車黨" "中華統促黨"     "自由台灣黨"   

cand_pm <- merge(x = cand, y = pm_cand_raw, all = TRUE)

# 以下為 c3h3 的貢獻
df = cand_pm %>% 
  mutate(候選人=姓名,region=縣市) %>% 
  right_join(pm_all_raw_filtered) %>% 
  mutate(InRegion=!is.na(縣市),name=候選人) %>% 
  group_by(name,InRegion)  %>% 
  summarise(n=n(),income=sum(收入金額))  

cand_m = cand_pm  %>% 
  mutate(候選人=姓名) %>% 
  mutate(InRegion=!is.na(縣市)
         ,name=候選人
         ,vote=as.double(gsub("%","",得票率))
         ,win=ifelse(當選註記=="*",1,0)
         ,re=ifelse(是否現任 == "是",1,0)) %>% 
  select(name,vote,win,re)

ddf = df %>% inner_join(cand_m) %>% View
readr::write_csv(ddf,"ddf.csv")