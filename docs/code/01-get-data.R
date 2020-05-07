# 라이브러리
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggrepel)
library(gghighlight)
library(extrafont)
loadfonts()

# 원본데이터
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

# 데이터 전처리
gdpr_df <- gdpr_violations %>%
  mutate(date = as_date(date, format = "%m/%d/%Y", tz = "UTC")) %>%
  mutate(wrong_date = date == as_date("1970-01-01")) %>%
  mutate(date = if_else(
    grepl("25.maj_2018", source) &
      wrong_date,
    as_date("2018-05-25"),
    date
  )) %>%
  mutate(date = if_else(
    grepl("2019/03/29", source) &
      wrong_date,
    as_date("2019-03-29"),
    date
  )) %>%
  mutate(date = if_else(
    grepl(
      "https://theword.iuslaboris.com/hrlaw/insights/spain-video-surveillance-and-data-protection-in-the-workplace",
      source
    ) & wrong_date,
    as_date("2019-09-20"),
    date
  )) %>%
  mutate(date = if_else(
    grepl(
      "https://www.etrend.sk/ekonomika/gdpr-zacina-hryzt-telekomunikacny-operator-dostal-pokutu-40-tisic-eur.html",
      source
    ) & wrong_date,
    as_date("2019-09-27"),
    date
  )) %>%
  filter(date > as_date("1970-01-01"))

gdpr_tbl <- gdpr_df %>% 
  transmute(id, country = name, price, authority, date, controller, type,
            articles = str_extract_all(article_violated, "Art. ?[:digit:]+")) %>% 
  mutate(ttl_articles = map_int(articles, length)) %>% 
  unnest(articles) %>% 
  mutate(articles = str_trim(articles) %>% str_remove(., " ") %>%  str_extract(., "Art\\. ?[:digit:]+")) %>% 
  nest(articles = c(articles))

