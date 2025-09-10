
from requests_html import HTMLSession
session = HTMLSession()
url = 'https://news.google.com/home?hl=en-NG&gl=NG&ceid=NG:en'
r = session.get(url)
r.html.render(sleep=1, scrolldown=5)
articles = r.html.find('article')
print(articles)



from requests_html import HTMLSession
session = HTMLSession()
url = 'https://news.google.com/home?hl=en-NG&gl=NG&ceid=NG:en'
r = session.get(url)
r.html.render(sleep=1, scrolldown=0)
articles = r.html.find('article')
print(articles)
try:
for item in articles:
    newsitem = item.find('h3', first=True)
    title = newsitem.text
    link = newsitem.absolute_link
    print(title, link)
except:
    pass






from requests_html import HTMLSession
session = HTMLSession()
url = 'https://news.google.com/home?hl=en-NG&gl=NG&ceid=NG:en'
r = session.get(url)
r.html.render(sleep=1, scrolldown=5)
articles = r.html.find('article')
newslist = []

for item in articles:
    try:
        newsitem = item.find('h3', first=True)
        newsarticle = {
            'title': newsitem.text,
            'link': newsitem.absolute_links
        }
        newslist.append(newsarticle)
    except:
        pass

print(len(newslist))

print(newslist[0])