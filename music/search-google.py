import datetime
import googlesearch
import os
import ffmpeg
import re
import string
import sys
import time
import traceback

'''
Search the following Web sites for pieces by me:

https://michaelgogins.bandcamp.org
https://michaelgogins.tumblr.com
https://music.youtube.com
https://soundcloud.com
  https://soundcloud.com/search/sounds?q=michael%20gogins
https://youtube.com
https://sonus.ca
'''
# This doesn't work well, as it is soon throttled by Google.
# However, manual searches for pieces can be done.
## results = googlesearch.search('"Michael Gogins" site:music.youtube.com', num_results=40, advanced=True)
results = googlesearch.search('https://soundcloud.com/search/sounds?q="michael%20gogins"', num_results=40, advanced=True)

for result in results:
    print(result.url, result.title)


