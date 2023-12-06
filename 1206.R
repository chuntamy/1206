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


#==========================================practice
#產生自訂函數「等候掛號人數」、「等候看診人數」、「等候住院人數」、「等候ICU人數」、「等候推床人數」等資訊

NTU_info = function () {
  
  result = data.frame(item = c('等候掛號人數', '等候看診人數', '等候住院人數', '等候ICU人數', '等候推床人數'),
                      info = NA,
                      stringsAsFactors = FALSE)
  
  URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"
  
  txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE) #擷取網址程式碼
  txt_new = paste(txt, sep = "", collapse = " ")
  
  start.pos = gregexpr("<tr>", txt_new)
  end.pos = gregexpr("</tr>", txt_new)
  
  #迴圈找出1:5旗資訊的人數
  for (i in 1:5) {
    
    sub.start.pos = start.pos[[1]][i]
    sub.end.pos = end.pos[[1]][i] + attr(end.pos[[1]], "match.length")[i] - 1
    
    sub_txt = substr(txt_new, sub.start.pos, sub.end.pos)
    sub_txt = gsub('等.*：', '', sub_txt)
    sub_txt = gsub('</?tr>', '', sub_txt)
    sub_txt = gsub('</?td>', '', sub_txt)
    result[i,'info'] = gsub(' ', '', sub_txt)
    
  }
  
  result #所有結果放置result
  
}

NTU_info()
#==========================================

#用「rvest」套件來自動擷取想要的資訊
#函數「html_nodes」:能把某種標籤的文字萃取出來
#「html_text」:能把標籤通通去掉

library(rvest)

URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

website = read_html(URL) #「read_html」:能讀取網頁

needed_txt = website %>% html_nodes("tr") %>% html_text() 
needed_txt


#對「a」這個標籤有興趣，因為它包含著文章的連結
URL = "https://www.ptt.cc/bbs/AllTogether/index3245.html"
website = read_html(URL)

needed_html = website %>% html_nodes("a")
needed_html

needed_txt = needed_html %>% html_text() #留下字即可
needed_txt

#比較有興趣的是徵女文，找到徵女文的位置在哪
intrested_pos = grep("[徵女]", needed_txt, fixed = TRUE)
needed_txt[intrested_pos]


#更有興趣的可能是這篇文章的連結
needed_link = needed_html[intrested_pos] %>% html_attr("href") #得到文章連結


#想要知道某篇文章的基本資料
#他們都被span標籤給抓到，但跟span有關的有太多了，但有一個特殊的class叫做article-meta-value
i = 1
sub_link = paste("https://www.ptt.cc", needed_link[i], sep = "")
sub_website = read_html(sub_link) 

article_info = sub_website %>% html_nodes(".article-meta-value")
article_info

#==========================================practice2
#隨時調閱最近的10篇徵男文
#從最新的頁面開始抓取徵男文的標題與連結，直到抓到10篇為止
#抓滿10篇之後，進去連結內去看看發文者ID以及時間，並把他填入表格之內

URL = "https://www.ptt.cc/bbs/AllTogether/index.html"
website = read_html(URL)

website %>% html_nodes("a") %>% .[8] %>% html_attr("href")

my_table = matrix("", nrow = 10, ncol = 4)
colnames(my_table) = c("Title", "url", "ID", "time")

URL = "https://www.ptt.cc/bbs/AllTogether/index.html"
current_id = 1

#抓到10篇為止
for (i in 1:10) {
  
  website = read_html(URL)
  needed_html = website %>% html_nodes("a")
  needed_txt = needed_html %>% html_text()
  intrested_pos = grep("[徵男]", needed_txt, fixed = TRUE)
  
  if (length(intrested_pos) > 0) {
    
    for (j in intrested_pos) {
      
      if (current_id <= 10) {
        my_table[current_id, 1] = needed_txt[j]
        my_table[current_id, 2] = needed_html[j] %>% html_attr("href")
      }
      
      current_id = current_id + 1
      
    }
    
  }
  
  if (current_id > 10) {
    break
  }
  
  next_page = website %>% html_nodes("a") %>% .[8] %>% html_attr("href")
  URL = paste0("https://www.ptt.cc", next_page, sep = "")
  
}

for (i in 1:nrow(my_table)) {
  
  sub_URL = paste("https://www.ptt.cc", my_table[i, 2], sep = "")
  sub_website = read_html(sub_URL)
  article_info = sub_website %>% html_nodes(".article-meta-value") %>% html_text()
  my_table[i, 3] = article_info[1]
  my_table[i, 4] = article_info[4]
  
}

my_table

#==========================================
#使用cookie
#使用cookie讀取網頁需要用到套件「RCurl」
library(RCurl)

URL = 'https://www.ptt.cc/bbs/Gossiping/index.html'
curl = getCurlHandle()
curlSetOpt(cookie = "over18=1", followlocation = TRUE, curl = curl) #自動將cookie打勾


html_character = getURL(URL, curl = curl)

website = read_html(html_character)
needed_html = website %>% html_nodes("a")
needed_txt = needed_html %>% html_text()
needed_txt









