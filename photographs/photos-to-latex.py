'''
PHOTOS TO LATEX
Copyright (C) 2020 by Michael Gogins

Given a manifest of photographs with optional captions, extracts metadata and 
builds a LaTeX file to publish the photographs. This file is inteded to be 
included in a complete LaTeX book.

The manifest format is simple:
relative pathname to image, followed by optional pipe symbol and caption text.
'''
import exif
from exif import Image
import io
import os.path
import string
import sys
import traceback

image_root = "/home/mkg/Dropbox/images/"

manifest = """c-2013-03-12_12-07-24.jpg|This is a scan of the first picture I took that I actually liked, a 35 mm slide. It was in 1968 in the back yard of my father's girlfriend Doreen's house in Taylorsville, Utah, just after sunset. I believe this was Christmas Day.
Mick, Wendy, Jane, Michael XMas.jpg|Scan of a slide of my grandfather Milton (Mick) Swensen, my sister Wendy, my grandmother Jane, and myself, Murray, Utah, Christmas or New Years 1968 I think.
renamed/c 2013-03-11_04-41-39.1.jpg|Scan of a slide of Elaine Constable, to whom I was briefly married, First Avenue stairs, Salt Lake City, Utah, 1971.
renamed/c 2013-03-11_04-41-41.1.jpg|Scan of a slide, sidewalk leaves, South Temple Street, Salt Lake City, Utah, 1972.
renamed/c 2013-03-11_04-41-44.1.jpg|Scan of a slide, Capitol Hill, Salt Lake City, Utah, 1972.
renamed/c 2013-03-11_04-41-51.1.jpg|Scan of a slide, dry cleaners around the corner from where I lived after separating from Elaine, Salt Lake City, Utah, 1972.
renamed/c 2013-03-11_04-41-49.1.jpg|Scan of a slide, coin laundromat, Third Avenue, Salt Lake City, Utah, 1972 or so.
renamed/c 2013-03-11_04-26-23.1.jpg|Scan of a slide taken by a friend with my camera of myself on flute, free jazz jam in Loring Park, Minneapolis, Minnesota, 1973.
renamed/c 2013-03-11_04-41-56.1.jpg|Scan of a slide of my sister Wendy, outside our building in the Loring Park neighborhood, Minneapolis, Minnesota, 1973.
renamed/c 2013-03-11_04-41-52.1.jpg|Scan of a slide, Bike on porch, Lake Harriet neighborhood, Minneapolis, Minnesota, 1973.
renamed/c 2013-03-11_04-41-57.1.jpg|Scan of a slide, my girlfriend Penny Suess, downtown Minneapolis, Minnesota, 1973.
renamed/c 2013-03-11_04-41-36.1.jpg|Scan of a slide taken at the Minnesota State Fair, 1973.
renamed/c 2013-03-11_04-41-58.1.jpg|Scan of a slide of my mother's sister Donna after Mick's funeral, on the street where she grew up, St. Paul, Minnesota, 1975.
renamed/c 2013-03-11_04-26-24.2.jpg|Scan of a slide, eastern side of Mount Timpanogos, Wasatch Mountains, Utah, 1977.
renamed/c 2013-03-11_04-41-50.1.jpg|Scan of a slide of my girlfriend Esther's friend Merrie, Salt Lake City, Utah, 1977 or so.
Jan 76P3 22 retouched.jpg|Scan of a slide of my cousin Jared and his then wife, Salt Lake City, Utah, 1976.
renamed/c 2013-03-12_12-22-47.jpg|Scan of a slide of Ruby Francis, Salt Lake City, Utah, I think in 1977.
renamed/c 2013-03-11_04-41-46.1.jpg|Scan of a slide of Charlie Potts, poet, Salt Lake City, Utah, 1977 or so.
renamed/c 2013-03-11_04-41-47.1.jpg|Scan of a slide of Memory Grove Park, Salt Lake City, Utah, winter of 1977 or so.
c 2016-04-06_21-05-56.1.jpg|Scan of a slide of the Deep Creek Mountains, western Utah, 1977 or 1978.
014.JPG|
renamed/c 2013-03-11_04-26-22.1.jpg|Taken while hitchiking across the country.
Scan7.jpg|Scan of a slide of the Roman Forum.
2004-04-13-b 010.jpg|
2004-12-18a 023.jpg|
2004-12-18a 026.jpg|
2005-02-28a 002.jpg|The only riding stable in Brooklyn.
2005-05-17a 042.jpg|
2005-07-09-a 100.jpg|
2005-10-02-a 020.jpg|
2005-10-10a 053.jpg|An old tree on our farm in Bovina, New York.
2009-08-27-a 097.jpg|
DSC01131.JPG|
DSC01240.JPG|
DSC01577.JPG|The old ice cream stand in Stamford, New York. Replaced by a newer (but not quite as friendly and funky) version.
DSC01723.JPG|Computer music luminaries Richard Boulanger (left) and Jean-Claude Risset, and their spouses, at the 2nd International Csound Conference, Berklee School of Music, Boston.
DSC02503.JPG|Magazine covers, Girona, Spain.
DSC02654.JPG|
RX100M5/100MSDCF/DSC06075.JPG|
RX100/100MSDCF/DSC06643.JPG|
RX100M5/100MSDCF/DSC06552.JPG|Chinese Garden, Dunedin, New Zealand.
RX100/100MSDCF/DSC00956.JPG|Lower East Side, Manhattan.
DSC02857.JPG|The Father offering the Son, Narbonne, France.
DSC02910.JPG|Christmas carnival in Carcassone, France.
DSC09755.JPG|Merry-go-round on the boardwalk in Tel Aviv, Israel.
20130604_134331.jpg|A remnant of Music Row in Manhattan.
20130619_080155.jpg|Central Park, Manhattan.
Heidi in Central Park.jpg|My wife Heidi, in Central Park, New York City, before we were married.
SM-950U/20180216_201359.jpg|Recoleta, Buenos Aires, Argentina."""

names_for_tags = {}

tags_ = '''Filename,file_name
Date taken,datetime_original
GPS longitude,gps_longitude
GPS latitude,gps_latitude
Make,make
Model,model
Focal length (35mm eq),focal_length_in_35mm_film
Exposure,exposure_time
F stop,f_number
ISO,photographic_sensitivity
Width,pixel_x_dimension
Height,pixel_y_dimension'''

tags = []
lines = tags_.split('\n')
for line in lines:
    name, tag = line.split(',')
    names_for_tags[tag] = name
    tags.append(tag)
    
stringio = io.StringIO()
    
photos = manifest.split("\n")
for photo in photos:
    filename, caption = photo.split("|")
    pathname = os.path.join(image_root, filename)
    pathname = r"" + pathname
    with open(pathname, 'rb') as image:
        image_with_metadata = Image(image)
        image_with_metadata["file_name"] = filename
        stringio.write(r'''
\KOMAoptions{pagesize}
\clearpage
\recalctypearea
\newpage
\noindent
''')
        for tag in tags:
            try:
                stringio.write(names_for_tags[tag])
                stringio.write(": ")
                stringio.write(str(image_with_metadata.get(tag)))
                stringio.write("\\\\ \n")
            except:
                pass
    stringio.write(r'''
\clearpage
\recalctypearea
\newpage
\noindent
\begin{figure}
    \includegraphics[width=\linewidth,height=\textheight,keepaspectratio]{''')
    stringio.write(r"" + pathname + "}")
    stringio.write("\n")
    stringio.write(r'''    \captionlistentry[figure]{\url{\protect\detokenize{''')
    stringio.write(r"" + caption + "}}}")
    stringio.write('''
\end{figure}
''')

print(stringio.getvalue())
                    
    
