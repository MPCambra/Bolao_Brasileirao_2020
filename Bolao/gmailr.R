options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)

gm_auth_configure(key = "324060675307-l9emjplcbti55k4rt9r0cp8olt9eao73.apps.googleusercontent.com",
                  secret = "Zz8Vf2V-0EOVwZIv2O7qyovU")

options(
  gargle_oauth_cache = ".secret",
  gargle_oauth_email = "bolao.brasileirao.ie@gmail.com"
)
gm_auth(email = "bolao.brasileirao.ie@gmail.com")

text_msg <- gm_mime() %>%
  gm_to("bolao.brasileirao.ie@gmail.com") %>%
  gm_from("bolao.brasileirao.ie@gmail.com") %>%
  gm_text_body('Got muscle?')

gm_send_message(text_msg)
