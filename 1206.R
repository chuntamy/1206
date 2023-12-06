URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE) #把整個網頁的資訊讀進來

head(txt, 15)

#遇到空白或是換行符號他就會自動下一行，用函數「paste」把他們貼成一個長字串(連續的字串較好處理)
txt_new = paste(txt, sep = "", collapse = " ")