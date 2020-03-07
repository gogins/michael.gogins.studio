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
import os
import os.path
import string
import subprocess
import sys
import traceback

image_root = "/home/mkg/Dropbox/images/"

output_filename1 = "/home/mkg/michael.gogins.studio/photographs/Gogins-Photographs-Content-I.tex"

manifest1 = """c-2013-03-12_12-07-24.jpg|This is a scan of the first picture I took that I actually liked, a 35 mm slide. It was in 1968 in the back yard of my father's girlfriend Doreen's house in Taylorsville, Utah, just after sunset. I believe this was Christmas Day.
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

output_filename2 = "/home/mkg/michael.gogins.studio/photographs/Gogins-Photographs-Content-II.tex"

manifest2 = """SM-950U/20190411_114936.jpg|
SM-950U/20190409_150209.jpg|
SM-950U/20190407_154615.jpg|
XT_1585/Camera/IMG_20170310_145854935.jpg|
renamed/c_2013-03-11_04-42-42.1.jpg|Scan of a slide, visual poet Karl Kempton.
renamed/c_2013-03-11_04-42-41.1.jpg|Scan of a slide, Pacific shore near Karl Kempton's house, Avila Beach, California.
renamed/c_2013-03-11_04-42-39.1.jpg|Scan of a slide, collectibles shop window, Venice Beach, California.
renamed/c_2013-03-11_04-42-38.1.jpg|Scan of a slide, amusement park midway, The Pike, Long Beach, California, probably 1973 or 1974.
renamed/c_2013-03-11_04-42-37.1.jpg|Scan of a slide, glass of ginger ale, Venice Beach, California, probably 1975.
renamed/c_2013-03-11_04-42-35.1.jpg|Scan of a slide.
renamed/c_2013-03-11_04-42-31.1.jpg|Scan of a slide, the merry-go-round in Griffith Park, Los Angeles, California, probably 1975 or 1976.
renamed/c_2013-03-11_04-42-30.1.jpg|Scan of a slide, stream bed, Uinta Mountains, Utah, probably 1977 or 1978.
renamed/c_2013-03-11_04-42-28.1.jpg|Scan of a slide, from the summit of Mount Whitney, Sierra Nevada Mountains, California, probably 1975.
renamed/c_2013-03-11_04-42-27.1.jpg|Scan of a slide. The rounded summit at one o'clock high is Mount Whitney, Sierra Nevada Mountains, California, probably 1975.
renamed/c_2013-03-11_04-42-26.1.jpg|Scan of a slide, near Alvarado Street, Los Angeles, California, mid to late 1970s.
renamed/c_2013-03-11_04-42-24.1.jpg|Scan of a slide, doorway, Avila Beach, California. probably 1975 or 1976.
renamed/c_2013-03-11_04-42-23.1.jpg|Scan of a slide, movie theater on Broadway, downtown Los Angeles, probably 1974 or 1975.
renamed/c_2013-03-11_04-42-18.1.jpg|Scan of a siide, through the window of a motel room, Santa Monica, California, probably 1974 or 1975.
renamed/c_2013-03-11_04-42-17.1.jpg|Scan of a slide, my girlfriend Penny Suess, supermarket parking lot on Lincoln Boulevard, Venice Beach, California, probably 1975.
renamed/c_2013-03-11_04-42-15.1.jpg|Scan of a slide, Penny's apartment, Santa Monica, California, probably 1975.
renamed/c_2013-03-11_04-42-13.1.jpg|Scan of a slide, Penny's apartment, Santa Monica, California, probably 1975.
renamed/c_2013-03-11_04-42-12.1.jpg|Scan of a slide, near Pershing Square, downtown Los Angeles, probably 1975 or 1976.
renamed/c_2013-03-11_04-42-11.1.jpg|Scan of a slide, near Pershing Square, downtown Los Angeles, probably 1975 or 1976.
renamed/c_2013-03-11_04-42-10.1.jpg|Scan of a slide, beachfront diner, Venice Beach, California, 1973.
renamed/c_2013-03-11_04-42-09.1.jpg|Scan of a slide, reporter's apartment, St. Louis, Missouri, 1973.
renamed/c_2013-03-11_04-42-08.1.jpg|Scan of a slide, woods in Minnesota near the Mississipi River, 1973.
renamed/c_2013-03-11_04-42-07.1.jpg|Scan of a slide, Riverside Park, New York City.
renamed/c_2013-03-11_04-42-06.1.jpg|Scan of a slide, Ferris wheel, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-05.1.jpg|Scan of a slide, ride, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-04.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-03.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-02.1.jpg|Scan of a slide, concession, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-01.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
20130626_080014.jpg|Bethesda Fountain, Central Park, New York City, on my way to or from work.
DSC01359.JPG|Flowers for sale, Chinatown, New York City.
DSC01372.JPG|Store window, Upper West Side, New York City.
DSC02089.JPG|Naval gun, the Castillo, Barcelona, Spain.
DSC02492.JPG|Girls on the train, Spain.
DSC02788.JPG|View from a Cathar castle, Languedoc, France.
DSC03275.JPG|Christmas scene, Chatham, New Jersey.
renamed/[2016-11-05_17-39-25][DSC00875][SONY][DSC-RX100][4f803edb1d9ac20236123e7787e8f734].1.jpg|New York Harbor from Carroll Gardens, Brooklyn, New York.
renamed/[2016-11-05_16-35-08][DSC00850][SONY][DSC-RX100][11d702afddf23ab11cc11b780ffba4de].1.jpg|Wall, Williamsburg, Brooklyn, New York City.
renamed/[2016-11-04_16-28-55][DSC00837][SONY][DSC-RX100][48fcaa6370377afea6e483ef14f9efcd].1.jpg|Museum of the City of New-York, New York City.
renamed/[2016-10-25_17-21-56][DSC00805][SONY][DSC-RX100][8e062614d3bbed5885ffb2648f83724e].1.jpg|Quebec City, Quebec, Canada.
renamed/[2016-09-09_19-02-13][DSC00615][SONY][DSC-RX100][017d30847041035aeaa5c80728795dc7].1.jpg|Garret, Greenwich Village, New York City.
renamed/[2016-07-22_19-15-20][DSC00365][SONY][DSC-RX100][5fa29079ce1388e440851a65ae601eb8].1.jpg|Drain, Bovina Creamery, Bovina Center, New York.
renamed/[2009-03-29_16-33-11][IMG_3998_2)][Canon][Canon_PowerShot_G7][d1f87e4ff942e2fee95a04206b38b641].1.jpg|My father's sister's husband Herb, who had been a jet engine mechanic, in his model shop at home in Florida.
renamed/[2009-03-29_13-58-00][IMG_3991][Canon][Canon_PowerShot_G7][0e450e9db99082f2bd8fecfaf333953f].1.jpg|In my father's sister's home, Florida.
renamed/[2013-03-10_16-46-34][DSC01069][SONY][DSC-RX100][9efc31162858a51f6e3e6332aca8a187].1.jpg|Toy display, Borough Park, Brooklyn, New York.
renamed/[2013-05-01_19-26-15][DSC01348][SONY][DSC-RX100][56ee9f69d83e82ba490c944db6e32d55].1.jpg|Collectibles shop, Greenwich Village, New York City."""

output_filename3 = "/home/mkg/michael.gogins.studio/photographs/Gogins-Photographs-Content-III.tex"

manifest3 = """renamed/[0000-00-00_00-00-00][2004-11-06-a_006][OLYMPUS_CORPORATION][C8080WZ][32bd997e981ec382127ad8d842056cee].1.jpg|
renamed/[2004-11-14_18-33-35][2004-11-14-a_063][OLYMPUS_CORPORATION][C8080WZ][705f111e8b2d48a3ecc6d8f978a61c4a].1.jpg|Ansonia Hotel, Upper West Side, New York City.
renamed/[2004-11-20_10-13-21][2004-11-20-a_002][OLYMPUS_CORPORATION][C8080WZ][23c3e981749af0ee300f75db2df49108].1.jpg|Child's coin-operated ride, Midtown, New York City.
renamed/[2004-11-21_17-49-19][2004-11-20-b_032][OLYMPUS_CORPORATION][C8080WZ][dc1eb86703708f0708c64ae019649286].1.jpg|Engagement party, Far Rockaway, New York City.
renamed/[2005-01-01_15-18-51][2005-01-01a_040][OLYMPUS_CORPORATION][C8080WZ][7cd839dea926d2896e5e25ac0f953d86].1.jpg|Grass, near our farm, Bovina, New York.
renamed/[2005-02-27_17-52-28][2005-02-28a_076][OLYMPUS_CORPORATION][C8080WZ][16fe16a27c2a7a83a412361ee51b1fc3].1.jpg|The bride and her mother, New York City.
renamed/[2005-05-04_18-12-56][2005-05-17a_034][OLYMPUS_CORPORATION][C8080WZ][9ea0d5501307d278cf1540585cc93ee7].1.jpg|New Jersey from Riverside Park, Manhattan.
renamed/[2005-07-10_11-00-02][2005-07-10-a_078][OLYMPUS_CORPORATION][C8080WZ][3ff8464df29df1e432d383066b6ed034].1.jpg|Bois de Boulogne, Paris, France.
renamed/[2005-09-18_16-54-27][2005-10-02-a_039][OLYMPUS_CORPORATION][C8080WZ][0c5523990779fd3d8f95d97b30ce3d6a].1.jpg|Stuff from our garden, Bovina, New York.
renamed/[2005-11-05_15-53-17][2005-11-14a_001][OLYMPUS_CORPORATION][C8080WZ][1d8262316a9e715755f56e20342e1e7e].1.jpg|Heidi with her hens on our farm, Bovina, New York.
renamed/[2012-12-22_20-12-45][DSC00769][SONY][DSC-RX100][db9a668105910211c6b8acb0fc6bd46a].1.jpg|Pacific ocean, coast of Northern California.
renamed/[2012-12-23_19-03-09][DSC00815][SONY][DSC-RX100][70a88e537b25517647ea99b868a5d9cb].1.jpg|Stream, I think in Sonoma County, Northern California.
renamed/[2012-12-24_23-21-17][DSC00831][SONY][DSC-RX100][5ea80573303c03ed7c7cdaadf4c0aed9].1.jpg|Little girl with Penbo toy, Christmas, Albany, California.
renamed/[2012-12-26_20-49-59][DSC00874][SONY][DSC-RX100][0c48cdd6a2b2def3f2c76d0621e9c6d5].1.jpg|Christmas decoration, Albany, California.
renamed/[2013-02-09_16-21-34][DSC00931][SONY][DSC-RX100][80fcb5529dfea18d2971cf84cd6533e9].1.jpg|Big snowfall attracting sledders, Central Park, New York City.
renamed/[2005-01-01_13-25-04][2005-01-01a_033][OLYMPUS_CORPORATION][C8080WZ][0cc4f0247f0e89d45a4571526d87358d].1.jpg|"""

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
    dir = os.path.dirname(pathname)
    filename = os.path.basename(pathname)
    os.chdir(dir)
    result = subprocess.run(["extractbb", filename])
    result = subprocess.run(["extractbb", "-O", pathname], encoding='utf-8', stdout=subprocess.PIPE)
    result = result.stdout.split("\n")
    result = result[2].split()
    return ',bb= 0 0 {} {}'.format(str(result[3]), str(result[4]))    
    
def process(manifest, output_filename):
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
\clearpage
\onecolumn
\noindent ''' + caption)
            stringio.write(r'''
\noindent
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
\clearpage
''')
            stringio.write(r'''
\begin{figure}
\includegraphics[''')
            stringio.write(r'''width=\linewidth,height=\textheight,keepaspectratio''')
            stringio.write(bb)
            stringio.write(r''']{''');
            stringio.write(r"" + pathname + "}")
            stringio.write(r'''
\captionlistentry[figure]{\url{\protect\detokenize{''')
            stringio.write(r"" + filename)
            stringio.write(r'''}}}
\end{figure}
    ''')
    print(stringio.getvalue())
    output = open(output_filename, 'w')
    output.write(stringio.getvalue())
    
process(manifest1, output_filename1)
process(manifest2, output_filename2)
process(manifest3, output_filename3)
                    
    
