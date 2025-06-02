import wget
from bs4 import BeautifulSoup
import requests
import re


#will take url in call and then identify the .gz to download


def getFiles2(url):
    r  = requests.get("https://" +url)
    data = r.text
    z=[]
    try:
        soup = BeautifulSoup(data)
        for link in soup(text=re.compile(r'.gz')):
            z.append(link.string)
        for i in z:
            site='https://'+url+i
            print(site)
            wget.download(site)
    except:
        print("Try Again")
  
getFiles2('www.sec.gov/Archives/edgar/Feed/2017/QTR1/')
