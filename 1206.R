URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE) #把整個網頁的資訊讀進來

head(txt, 15)

#遇到空白或是換行符號他就會自動下一行，用函數「paste」把他們貼成一個長字串(連續的字串較好處理)
txt_new = paste(txt, sep = "", collapse = " ")


#把這個網頁的title擷取下來，利用正則表達式
TITLE.pos = gregexpr("<title>.*</title>", txt_new) #找出title的位置
start.TITLE.pos = TITLE.pos[[1]][1] #找出title開始處
end.TITLE.pos = start.TITLE.pos + attr(TITLE.pos[[1]], "match.length")[1] - 1 #找出title結束處

TITLE.word = substr(txt_new, start.TITLE.pos, end.TITLE.pos) #擷取出從開始到結束處

TITLE.word
