library(telegram)

send_telegram_message <- function(text, chat_id, bot_token){ 
  require(telegram) 
  bot <- TGBot$new(token = bot_token) 
  bot$sendMessage(text = text, chat_id = chat_id) 
}


for(i in 1:100000){
   
   print(i)
  
}

send_telegram_message(text = paste("Hey, your script is done!",timestamp()),
                      chat_id = 555649681,
                      bot_token = '478033552:AAHeBRJDkEDv-pswf-0PmWrNb9VnBXv0GKo'
                      )


