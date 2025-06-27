mbeta <- betareg(RE_prp ~ HLR_md+LR_kgNhay+area_m2 |HLR_md + area_m2 ,
                 #link = "log",
                 data = df %>% mutate(RE_prp=RE_pct/100))

m <- mbeta

