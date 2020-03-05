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
import subprocess
import sys
import traceback

image_root = "/home/mkg/Dropbox/images/"

output_filename = "PhotographsContent1.tex"

manifest = """c-2013-03-12_12-07-24.jpg|This is a scan of the first picture I took that I actually liked, a 35 mm slide. It was in 1968 in the back yard of my father's girlfriend Doreen's house in Taylorsville, Utah, just after sunset. I believe this was Christmas Day.
Mick,_Wendy,_Jane,_Michael_XMas.jpg|Scan of a slide of my grandfather Milton (Mick) Swensen, my sister Wendy, my grandmother Jane, and myself, Murray, Utah, Christmas or New Years 1968 I think.
renamed/c_2013-03-11_04-41-39.1.jpg|Scan of a slide of Elaine Constable, to whom I was briefly married, First Avenue stairs, Salt Lake City, Utah, 1971.
renamed/c_2013-03-11_04-41-41.1.jpg|Scan of a slide, sidewalk leaves, South Temple Street, Salt Lake City, Utah, 1972.
renamed/c_2013-03-11_04-41-44.1.jpg|Scan of a slide, Capitol Hill, Salt Lake City, Utah, 1972.
renamed/c_2013-03-11_04-41-51.1.jpg|Scan of a slide, dry cleaners around the corner from where I lived after separating from Elaine, Salt Lake City, Utah, 1972.
renamed/c_2013-03-11_04-41-49.1.jpg|Scan of a slide, coin laundromat, Third Avenue, Salt Lake City, Utah, 1972 or so.
renamed/c_2013-03-11_04-26-23.1.jpg|Scan of a slide taken by a friend with my camera of myself on flute, free jazz jam in Loring Park, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-56.1.jpg|Scan of a slide of my sister Wendy, outside our building in the Loring Park neighborhood, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-52.1.jpg|Scan of a slide, Bike on porch, Lake Harriet neighborhood, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-57.1.jpg|Scan of a slide, my girlfriend Penny Suess, downtown Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-36.1.jpg|Scan of a slide taken at the Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-41-58.1.jpg|Scan of a slide of my mother's sister Donna after Mick's funeral, on the street where she grew up, St. Paul, Minnesota, 1975.
renamed/c_2013-03-11_04-26-24.2.jpg|Scan of a slide, eastern side of Mount Timpanogos, Wasatch Mountains, Utah, 1977.
renamed/c_2013-03-11_04-41-50.1.jpg|Scan of a slide of my girlfriend Esther's friend Merrie, Salt Lake City, Utah, 1977 or so.
Jan_76P3_22_retouched.jpg|Scan of a slide of my cousin Jared and his then wife, Salt Lake City, Utah, 1976.
renamed/c_2013-03-12_12-22-47.jpg|Scan of a slide of Ruby Francis, Salt Lake City, Utah, I think in 1977.
renamed/c_2013-03-11_04-41-46.1.jpg|Scan of a slide of Charlie Potts, poet, Salt Lake City, Utah, 1977 or so.
renamed/c_2013-03-11_04-41-47.1.jpg|Scan of a slide of Memory Grove Park, Salt Lake City, Utah, winter of 1977 or so.
c_2016-04-06_21-05-56.1.jpg|Scan of a slide of the Deep Creek Mountains, western Utah, 1977 or 1978.
014.JPG|
renamed/c_2013-03-11_04-26-22.1.jpg|Taken while hitchiking across the country.
Scan7.jpg|Scan of a slide of the Roman Forum.
2004-04-13-b_010.jpg|
2004-12-18a_023.jpg|
2004-12-18a_026.jpg|
2005-02-28a_002.jpg|The only riding stable in Brooklyn.
2005-05-17a_042.jpg|
2005-07-09-a_100.jpg|
2005-10-02-a_020.jpg|
2005-10-10a_053.jpg|An old tree on our farm in Bovina, New York.
2009-08-27-a_097.jpg|
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
Heidi_in_Central_Park.jpg|My wife Heidi, in Central Park, New York City, before we were married.
SM-950U/20180216_201359.jpg|Recoleta, Buenos Aires, Argentina."""

names_for_tags = {}

tags_ = '''Date,datetime_original
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
    
def bounding_box(pathname):
    result = subprocess.run(["extractbb", "-O", pathname], encoding='utf-8', stdout=subprocess.PIPE)
    result = result.stdout.split("\n")
    result = result[2].split()
    return ', bb= 0 0 {} {}'.format(str(result[3]), str(result[4]))    
    
stringio = io.StringIO()
    
photos = manifest.split("\n")
for photo in photos:
    filename, caption = photo.split("|")
    pathname = os.path.join(image_root, filename)
    pathname = r"" + pathname
    with open(pathname, 'rb') as image:
        bb = bounding_box(pathname)
        image_with_metadata = Image(image)
        orientation = "None"
        try:
            orientation = str(image_with_metadata.get("orientation"))
        except:
            pass
        # print(orientation)
        width = 0
        height = 0
        try:
            width = int(str(image_with_metadata.get("pixel_x_dimension")))
            height = int(str(image_with_metadata.get("pixel_y_dimension")))
        except:
            pass
        stringio.write(r'''

\noindent ''' + caption)
        stringio.write(r'''
\begin{lstlisting}
''')
        stringio.write(r"Filename: " + filename)
        stringio.write("\n\n")
        for tag in tags:
            try:
                value = r"" + str(image_with_metadata.get(tag))
                if value != "None":
                    stringio.write(r"" + names_for_tags[tag])
                    stringio.write(r": ")
                    stringio.write(value)
                    stringio.write("\n")
            except:
                pass
        stringio.write(r'''\end{lstlisting}
''')
        if width > height:
            stringio.write(r'''
\begin{landscape}
''')
        else:
            stringio.write(r'''
\clearpage
''')
        stringio.write('''
\includegraphics[''')
        stringio.write(r'''width=\linewidth,height=\textheight,keepaspectratio''')
        #bb = ",bb=0 0 " + str(height) + " " + str(width)
        stringio.write(bb)
        stringio.write(r''']{''');
        stringio.write(r"" + pathname + "}")
        if width > height:
            stringio.write(r'''
\end{landscape}

''')
        else:
            stringio.write('''
\clearpage
''')
    
    
print(stringio.getvalue())

output = open(output_filename, 'w')
output.write(stringio.getvalue())
                    
    
