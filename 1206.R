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


#用函數「gsub」取得乾淨一點的標題
TITLE.word = gsub("<title>", "", TITLE.word)
TITLE.word = gsub("</title>", "", TITLE.word)
TITLE.word



#想抓到「等候掛號人數：」後面的人數
#因為以tr結尾的有非常多，所以必須分開抓取，無法使用正則表達式
start.pos = gregexpr("<tr>", txt_new) #找出所有為<tr>開頭
end.pos = gregexpr("</tr>", txt_new)  #找出所有為</tr>結尾

i = 1 #<tr>開頭第一個為何?
sub.start.pos = start.pos[[1]][i] #<tr>開頭第一個位置
sub.end.pos = end.pos[[1]][i] + attr(end.pos[[1]], "match.length")[i] - 1 #<tr>開頭第一個結尾位置

sub_txt = substr(txt_new, sub.start.pos, sub.end.pos) #擷取出 <tr>開頭第一個 從開始到結束處
sub_txt 

#只擷取出人數
sub_txt = gsub('等候掛號人數：', '', sub_txt)
sub_txt = gsub('</?tr>', '', sub_txt)
sub_txt = gsub('</?td>', '', sub_txt)
sub_txt = gsub(' ', '', sub_txt)
sub_txt
