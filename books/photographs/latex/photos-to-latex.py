'''
PHOTOS TO LATEX
Copyright (C) 2022 by Michael Gogins

Given a manifest of photographs with optional captions, extracts metadata and 
builds a LaTeX file to publish the photographs. This file is intended to be 
included in a complete LaTeX book.

The manifest format is simple:
relative pathname to image, followed by optional pipe symbol and caption text.
'''
import dropbox
import exif
from exif import Image
import io
import os
import os.path
import string
import subprocess
import sys
import traceback
import unicodedata

#~ 1
image_root = "/Users/michaelgogins/Dropbox/images/"

output_filename1 = "Gogins-Photographs-Content-Ia.tex"

# NOTA BENE: 
#
# Keep all images in chronological order BY HAND as image metadata may be 
# missing or misleading. Each scan should be given a caption containing the 
# word "scan."
#
# The manifest should be broken up into volumes of no more than 500 megabytes 
# per pdf.

page_template = '''
\clearpage
\section{{\protect\detokenize{{{basename}}}}}
\\noindent {text}
\\noindent
\\begin{{lstlisting}}
{metadata}
\\end{{lstlisting}}
\\clearpage
\\begin{{figure}}
\\includegraphics[width=\\linewidth,height=\\textheight,keepaspectratio,{bb}]{{{basename}}}
\\end{{figure}}

'''

manifest1 = """c-2013-03-12_12-07-24.jpg|This is a scan of the first picture I took that I actually liked, a 35 mm slide. It was in 1968 in the back yard of my father's girlfriend Doreen's house in Taylorsville, Utah, just after sunset. I believe this was Christmas Day.
Mick_Wendy_Jane_Michael_XMas.jpg|Scan of a slide of my grandfather Milton (Mick) Swensen, my sister Wendy, my grandmother Jane, and myself, Murray, Utah, Christmas or New Years 1968 I think.
renamed/c_2013-03-11_04-41-39.1.jpg|Scan of a slide of Elaine Constable, to whom I was briefly married, First Avenue stairs, Salt Lake City, Utah, 1971.
renamed/c_2013-03-11_04-41-41.1.jpg|Scan of a slide, sidewalk leaves, South Temple Street, Salt Lake City, Utah, 1972.
renamed/c_2013-03-11_04-41-44.1.jpg|Scan of a slide, Capitol Hill, Salt Lake City, Utah, 1972.
renamed/c_2013-03-11_04-41-51.1.jpg|Scan of a slide, dry cleaners around the corner from where I lived after separating from Elaine, Salt Lake City, Utah, 1972.
renamed/c_2016-04-06_21-05-56.1_v1.jpg|Scan of a slide of the Deep Creek Mountains, western Utah, 1972 or so.
renamed/c_2013-03-11_04-42-07.1.jpg|Scan of a slide, Riverside Park, New York City, 1972.
renamed/c_2013-03-11_04-26-22.1.jpg|Scan of a slide taken while hitchiking across the country, 1972 I think.
renamed/c_2013-03-11_04-41-49.1.jpg|Scan of a slide, coin laundromat, Third Avenue, Salt Lake City, Utah, 1972 or so.
renamed/c_2013-03-11_04-26-23.1.jpg|Scan of a slide taken by a friend with my camera of myself on flute, free jazz jam in Loring Park, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-56.1.jpg|Scan of a slide of my sister Wendy, outside our building in the Loring Park neighborhood, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-52.1.jpg|Scan of a slide, bike on porch, Lake Harriet neighborhood, Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-57.1.jpg|Scan of a slide, my girlfriend Penny Suess, downtown Minneapolis, Minnesota, 1973.
renamed/c_2013-03-11_04-41-36.1.jpg|Scan of a slide taken at the Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-06.1.jpg|Scan of a slide, Ferris wheel, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-05.1.jpg|Scan of a slide, ride, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-04.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-03.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-02.1.jpg|Scan of a slide, concession, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-01.1.jpg|Scan of a slide, crowd, Minnesota State Fair, 1973.
renamed/c_2013-03-11_04-42-08.1.jpg|Scan of a slide, woods in Minnesota near the Mississipi River, 1973.
renamed/c_2013-03-11_04-42-09.1.jpg|Scan of a slide, reporter's apartment, St. Louis, Missouri, 1973.
renamed/c_2013-03-11_04-42-10.1.jpg|Scan of a slide, beachfront diner, Venice Beach, California, 1973.
renamed/c_2013-03-11_04-42-42.1.jpg|Scan of a slide, visual poet Karl Kempton.
renamed/c_2013-03-11_04-42-24.1.jpg|Scan of a slide, doorway, Avila Beach, California. probably 1975 or 1976.
renamed/c_2013-03-11_04-42-41.1.jpg|Scan of a slide, Pacific shore near Karl Kempton's house, Avila Beach, California.
renamed/c_2013-03-11_04-42-39.1.jpg|Scan of a slide, collectibles shop window, Venice Beach, California.
renamed/c_2013-03-11_04-42-38.1.jpg|Scan of a slide, amusement park midway, The Pike, Long Beach, California, probably 1973 or 1974.
renamed/c_2013-03-11_04-42-37.1.jpg|Scan of a slide, glass of ginger ale, Venice Beach, California, probably 1975.
renamed/c_2013-03-11_04-42-35.1.jpg|Scan of a slide.
renamed/c_2013-03-11_04-41-58.1.jpg|Scan of a slide of my mother's sister Donna after Mick's funeral, on the street where she grew up, St. Paul, Minnesota, 1975.
renamed/c_2013-03-11_04-42-31.1.jpg|Scan of a slide, the merry-go-round in Griffith Park, Los Angeles, California, probably 1975 or 1976.
renamed/c_2013-03-11_04-42-28.1.jpg|Scan of a slide, from the summit of Mount Whitney, Sierra Nevada Mountains, California, probably 1975.
renamed/c_2013-03-11_04-42-27.1.jpg|Scan of a slide. The rounded summit at one o'clock high is Mount Whitney, Sierra Nevada Mountains, California, probably 1975.
renamed/c_2013-03-11_04-42-26.1.jpg|Scan of a slide, near Alvarado Street, Los Angeles, California, mid to late 1970s.
renamed/c_2013-03-11_04-42-23.1.jpg|Scan of a slide, movie theater on Broadway, downtown Los Angeles, probably 1974 or 1975.
renamed/c_2013-03-11_04-42-18.1.jpg|Scan of a slide, through the window of a motel room, Santa Monica, California, probably 1974 or 1975.
renamed/c_2013-03-11_04-42-17.1.jpg|Scan of a slide, my girlfriend Penny Suess, supermarket parking lot on Lincoln Boulevard, Venice Beach, California, probably 1975.
renamed/c_2013-03-11_04-42-15.1.jpg|Scan of a slide, Penny's apartment, Santa Monica, California, probably 1975.
renamed/c_2013-03-11_04-42-13.1.jpg|Scan of a slide, Penny's apartment, Santa Monica, California, probably 1975.
renamed/c_2013-03-11_04-42-12.1.jpg|Scan of a slide, near Pershing Square, downtown Los Angeles, probably 1975 or 1976.
renamed/c_2013-03-11_04-42-11.1.jpg|Scan of a slide, near Pershing Square, downtown Los Angeles, probably 1975 or 1976.
Jan_76P3_22_retouched.jpg|Scan of a slide of my cousin Jared and his then wife, Salt Lake City, Utah, 1976.
renamed/c_2013-03-11_04-42-30.1.jpg|Scan of a slide, stream bed, Uinta Mountains, Utah, probably 1977 or 1978.
renamed/c_2013-03-11_04-26-24.2.jpg|Scan of a slide, eastern side of Mount Timpanogos, Wasatch Mountains, Utah, 1977.
renamed/c_2013-03-11_04-41-46.1.jpg|Scan of a slide of Charlie Potts, poet, Salt Lake City, Utah, 1977 or so.
renamed/c_2013-03-12_12-22-47.jpg|Scan of a slide of Ruby Francis, Salt Lake City, Utah, I think in 1977.
renamed/c_2013-03-11_04-41-50.1.jpg|Scan of a slide of my girlfriend Esther's friend Merrie, Salt Lake City, Utah, 1977 or so.
renamed/c_2013-03-11_04-41-47.1.jpg|Scan of a slide of Memory Grove Park, Salt Lake City, Utah, winter of 1977 or so.
Heidi_in_Central_Park.jpg|Scan of a slide of my wife Heidi, in Central Park, New York City, before we were married, 1996.
Scan7.jpg|Scan of a slide of the Roman Forum, 2001.
renamed/[2002-01-01_00-00-00][PICT0060][Minolta_Co_Ltd][DiMAGE_F100][b0b62f51d659715fac87c1c3938d742f].1.jpg|Matrioshka dolls, Park Avenue, New York City.
2004-04-13-b_010.jpg|
PICT0018.JPG|Flowers, Connecticut, United States.
renamed/[2002-08-03_18-20-36][PICT0021][Minolta_Co_Ltd][DiMAGE_F100][4900c836b547772b89c08c30df77c9b2].1.jpg|Antique firearms, Delaware County Historical Society, Delhi, Delaware County, New York, United States.
PICT0014a.JPG|Heidi on our grass at the farm, Crescent Valley Road, Bovina Center, Delaware County, New York, United States.
PICT0010.JPG|Our neighbor Robert Bindler at his farm, Crescent Valley, Bovina Center, New York.
renamed/[2002-01-01_00-00-00][2004-10-12-a_008][Minolta_Co_Ltd][DiMAGE_F100][33e98d430688f40e0eac873100ade7ec].1.jpg|Eggs from Heidi's chickens, our farm, Crescent Valley Road, Bovina Center, Delaware County, New York, United States.
renamed/[0000-00-00_00-00-00][2004-11-06-a_006][OLYMPUS_CORPORATION][C8080WZ][32bd997e981ec382127ad8d842056cee].1.jpg|
renamed/[2004-11-14_18-33-35][2004-11-14-a_063][OLYMPUS_CORPORATION][C8080WZ][705f111e8b2d48a3ecc6d8f978a61c4a].1.jpg|Ansonia Hotel, Upper West Side, New York City.
renamed/[2004-11-20_10-13-21][2004-11-20-a_002][OLYMPUS_CORPORATION][C8080WZ][23c3e981749af0ee300f75db2df49108].1.jpg|Child's coin-operated ride, Midtown, New York City.
renamed/[2004-11-21_17-49-19][2004-11-20-b_032][OLYMPUS_CORPORATION][C8080WZ][dc1eb86703708f0708c64ae019649286].1.jpg|Engagement party, Far Rockaway, New York City.
renamed/[2004-11-25_13-02-49][2004-11-25-a_030][OLYMPUS_CORPORATION][C8080WZ][2f11ebf07a71a53cec60a4b8684e8087].1.jpg|Central Park, Manhattan.
renamed/[2004-11-28_16-10-30][2004-12-01-a_036][OLYMPUS_CORPORATION][C8080WZ][20e4fe68a5b13e344f512392d6025cac].1.jpg|Sky from our farm, Crescent Valley, Bovina Center, New York.
2004-12-18a_023.jpg|
2004-12-18a_026.jpg|
renamed/[2005-01-01_13-25-04][2005-01-01a_033][OLYMPUS_CORPORATION][C8080WZ][0cc4f0247f0e89d45a4571526d87358d].1.jpg|
renamed/[2005-01-01_15-18-51][2005-01-01a_040][OLYMPUS_CORPORATION][C8080WZ][7cd839dea926d2896e5e25ac0f953d86].1.jpg|Grass, near our farm, Bovina, New York.
2005-02-28a_002.jpg|The only riding stable in Brooklyn.
renamed/[2005-02-27_17-52-28][2005-02-28a_076][OLYMPUS_CORPORATION][C8080WZ][16fe16a27c2a7a83a412361ee51b1fc3].1.jpg|The bride and her mother, New York City.
renamed/[2005-02-27_20-05-01][2005-02-28a_258][OLYMPUS_CORPORATION][C8080WZ][4f662313298f585db64d537fb3542cf2].1.jpg|Bride leaving her wedding, New York City.
renamed/[2005-05-04_18-12-56][2005-05-17a_034][OLYMPUS_CORPORATION][C8080WZ][9ea0d5501307d278cf1540585cc93ee7].1.jpg|New Jersey from Riverside Park, Manhattan.
2005-05-17a_042.jpg|Riverside Park and Hudson River, Manhattan.
renamed/[2005-06-17_18-10-15][2005-06-26a_010][OLYMPUS_CORPORATION][C8080WZ][f92166a5bea3a09f42dbcf667cf23b49].1.jpg|Statue of Jose Marti, Central Park South, New York City.
renamed/[2005-07-09_06-36-44][2005-07-09-a_043][OLYMPUS_CORPORATION][C8080WZ][2f611944a86ca4656f65d7ea48a9cd7b].1.jpg|Fountain at the palace of Versailles, France.
2005-07-09-a_100.jpg|
renamed/[2005-07-10_03-55-25][2005-07-10-a_041][OLYMPUS_CORPORATION][C8080WZ][5621556dc949240eadaf07aea4bca647].1.jpg|Postcards for sale, Paris, France.
renamed/[2005-07-10_11-00-02][2005-07-10-a_078][OLYMPUS_CORPORATION][C8080WZ][3ff8464df29df1e432d383066b6ed034].1.jpg|Bois de Boulogne, Paris, France.
2005-10-02-a_020.jpg|An old tree on our farm in Bovina, New York.
renamed/[2005-09-18_16-54-27][2005-10-02-a_039][OLYMPUS_CORPORATION][C8080WZ][0c5523990779fd3d8f95d97b30ce3d6a].1.jpg|Stuff from our garden, Bovina, New York.
2005-10-10a_053.jpg|A leaf on the road at our farm, Crescent Valley, Bovina Center, Delaware County, New York.
renamed/[2005-10-02_10-37-50][2005-10-02-a_041][OLYMPUS_CORPORATION][C8080WZ][03e111f120127f552da99ed99f49002a].1.jpg|Kitsch for sale, New York City.
2005-10-10a_048.jpg|Hillside in autumn on our farm, Crescent Valley, Bovina Center, New York.
renamed/[2005-11-05_15-53-17][2005-11-14a_001][OLYMPUS_CORPORATION][C8080WZ][1d8262316a9e715755f56e20342e1e7e].1.jpg|Heidi with her hens on our farm, Bovina, New York.
renamed/[2005-12-26_19-03-55][2005-12-27a_041][OLYMPUS_CORPORATION][C8080WZ][165f51258de62c00f81134ba945bd682].1.jpg|Pacific Ocean, Northern California, United States.
renamed/[2005-12-24_14-22-27][2005-12-24a_124][OLYMPUS_CORPORATION][C8080WZ][57a546800620505241d6cb1ae06b1880].1.jpg|Sonoma County, California.
renamed/[2005-12-29_18-56-59][2005-12-30a_191][OLYMPUS_CORPORATION][C8080WZ][071807195a691e0e537540fbef1402df].1.jpg|Unkown woman, Fairfax Farmers Market, Los Angeles, California.
renamed/[2005-12-29_19-18-23][2005-12-30a_224][OLYMPUS_CORPORATION][C8080WZ][3bf39aa7f66e0de9261fd0930b12b5fc].1.jpg|Billboard, The Grove shopping centerx, Los Angeles, California.
renamed/[2006-05-20_18-37-46][Survey_-_2006-01-15_097][OLYMPUS_CORPORATION][C8080WZ][64e6408c5a84eb7a6a1155fb7327a7fa].1.jpg|Michael Williams and a friend, our apartment, New York City.
renamed/[2006-08-18_13-10-09][2006-07-18-a_051][OLYMPUS_CORPORATION][C8080WZ][64cb3cc699778fc59f76c8b932040f1e].1.jpg|Delaware Coumty Fair, Walton, New York.
renamed/[2007-03-12_11-41-10][2007-03-12-a_144][Canon][Canon_PowerShot_G7][8da3a099769a0842274d66eb7f8e891c].1.jpg|Stains on wall, New York City.
renamed/[2007-03-13_17-44-33][2007-03-13-a_059][Canon][Canon_PowerShot_G7][25d5afb2694acd89cc8a18cc0d490f55].1.jpg|Times Square, New York City.
renamed/[2007-03-13_17-47-51][2007-03-13-a_069][Canon][Canon_PowerShot_G7][0635f53ef2f63b22beed6af1d4059512].1.jpg|Times Square, New York City.
renamed/[2007-04-24_18-28-49][2007-03-31-b_218][Canon][Canon_PowerShot_G7][0023f5f85a13297e3b3c47bf22d5b175].1.jpg|Loading dock, Williamsburg, New York City.
renamed/[2007-05-20_14-09-50][2007-05-20-a_094][Canon][Canon_PowerShot_G7][9b10932b3988089fc4b36cb1cfabfa57].1.jpg|Hike in the Adirondack Mountains, New York.
renamed/[2007-05-21_18-37-14][2007-05-21-a_024][Canon][Canon_PowerShot_G7][1c1d0a9f40021421167b68ebc3d7cc76].1.jpg|Thousand Islands, St. Lawrence Seaway, New York
renamed/[2007-05-23_12-22-59][2007-05-25-a_122][Canon][Canon_PowerShot_G7][df1c2c0bc54b7603a9c4f200433ef608].1.jpg|Blossoming tree, Cornell University arboretum, Ithaca, New York.
renamed/[2007-08-05_10-25-02][2007-08-18-a_148][Canon][Canon_PowerShot_G7][4f1dbab672327455d87f4c8cb46a30b2].1.jpg|Birch bark, Bill Carpenter's yard, Slingerlands, New York.
renamed/[2007-08-27_11-11-38][2007-11-22-a_026][Canon][Canon_PowerShot_G7][32263f004507ab07625955e6d7eddda9].1.jpg|Rocking horse from Poland, bought by Heidi's parents, at our farm.
renamed/[2007-10-19_18-49-20][2007-10-22-a_066][OLYMPUS_IMAGING_CORP__][u760S760_______][05555ee9b2cefea3a54d3291f767a8e4].1.jpg|Antelope Island from the south end of the Great Salt Lake, Utah.
renamed/[2007-10-21_11-22-00][2007-10-22-a_124][OLYMPUS_IMAGING_CORP__][u760S760_______][842e50b23b0ef23277443f7cc147622a].1.jpg|My father Laird, a year or so before his death, Salt Lake City, Utah.
renamed/[2007-12-07_13-15-18][PC070125][OLYMPUS_IMAGING_CORP__][u760S760_______][9482ade77febe36bfbd18d7b646d61c6].1.jpg|New York City, self-titled.
renamed/[2007-12-22_10-35-52][2007-12-30-a_001][Canon][Canon_PowerShot_G7][6d463a8c423b2c353a456ec0598e7983].1.jpg|San Francisco Bay from the Berkeley hills.
renamed/[2008-01-19_20-35-25][P1190200][OLYMPUS_IMAGING_CORP__][u760S760_______][105b77651b262a04653436810bd24652].1.jpg|George and Carole Silvers' doll house, Crescent Valley, Bovina Center, New York.
renamed/[2008-09-17_18-17-05][2008-10-08-a_155][Canon][Canon_PowerShot_G7][ee38a79382b5c26b0004e134a62a0620].1.jpg|Tenth Avenue, Manhattan.
renamed/[2008-10-05_16-30-49][2008-10-08-a_224][Canon][Canon_PowerShot_G7][0c153b55498ecb3c12bfe1d4fa0ceaaf].1.jpg|My father, Heidi, my stepbrother Scott Fechner, and his wife Claudia, Brighton, Utah.
renamed/[2008-11-06_14-58-54][2008-11-06-a_009][Canon][Canon_PowerShot_G7][f914cb79e53df3fcf69ea4e106b6c87a].1.jpg|The Wasatch Front above Farmington Bay Wildlife Refuge, Utah.
renamed/[2008-11-06_18-25-11][2008-11-06-a_021][Canon][Canon_PowerShot_G7][0730ea6941897f8e7f8d09d6fd3f97db].1.jpg|Cache Valley, Utah and Wyoming.
renamed/[2008-12-21_14-06-33][2008-12-31-a_160][Canon][Canon_PowerShot_G7][b6f135162a820b2797a244d53cc44f9c].1.jpg|Merry-go-round, London.
renamed/[2008-12-22_07-05-02][2008-12-31-a_180][Canon][Canon_PowerShot_G7][523e76a47e69497f0b7dba8603f25ba0].1.jpg|The Thames and the City from the SunGard office, Docklands, London, United Kingdom.
renamed/[2008-12-23_09-26-06][2008-12-31-a_267][Canon][Canon_PowerShot_G7][41ac1582c91cfedfde1ab1587ca14521].1.jpg|Professor John ffitch, maintainer of Csound, in his office at the University of Bath, United Kingdom.
renamed/[2008-12-29_10-10-04][2008-12-31-a_420][Canon][Canon_PowerShot_G7][ebcc482203fb0d3473d03c974f1a1cdd].1.jpg|South Devon, United Kingdom.
renamed/[2008-12-31_07-31-46][2008-12-31-a_484][Canon][Canon_PowerShot_G7][38bf2d0396733e566a720cbdc4f03c57].1.jpg|Cliffs near Lyme Regis, South Devon, United Kingdom.
renamed/[2008-12-31_09-09-29][2008-12-31-a_503][Canon][Canon_PowerShot_G7][0f63698c9866de49fa6e38e09624f92c].1.jpg|South Devon, United Kingdom.
renamed/[2009-03-07_15-38-21][2009-03-08-a_155][Canon][Canon_PowerShot_G7][ba84389a7d29f0486b303577b05ce949].1.jpg|Gene Bertoncini.
renamed/[2009-03-08_08-57-25][2009-03-08-a_259][Canon][Canon_PowerShot_G7][2f6839f9261e9e527d5f901023ac8267].1.jpg|Eggs from Heidi's hens, in our kitchen in Manhattan.
renamed/[2009-03-27_14-37-47][IMG_3933_2)][Canon][Canon_PowerShot_G7][06fe0850c7fde687debf1190e8a6347b].1.jpg|Florida.
renamed/[2009-03-29_13-58-00][IMG_3991][Canon][Canon_PowerShot_G7][0e450e9db99082f2bd8fecfaf333953f].1.jpg|In my father's sister's home, Florida.
renamed/[2009-03-29_16-33-11][IMG_3998_2)][Canon][Canon_PowerShot_G7][d1f87e4ff942e2fee95a04206b38b641].1.jpg|My father's sister's husband Herb, who had been a jet engine mechanic, in his model shop at home in Florida.
renamed/[2009-03-29_19-11-08][IMG_4016_2)][Canon][Canon_PowerShot_G7][adeb220d561845d5406081b8dbb4e0e9].1.jpg|My father's sisters Corinne and Ginger, Heidi, and Corinne's husband Herb, in Florida.
renamed/[2009-03-30_18-57-34][IMG_4062][Canon][Canon_PowerShot_G7][773fb2e2e258cc766b12443d48ce0044].1.jpg|South (I think) Carolina coast.
renamed/[2009-04-01_13-33-33][IMG_4121_2)][Canon][Canon_PowerShot_G7][2c5ef7982116c1a37230654d5e6fa9a0].1.jpg|Charleston, South Carolina.
renamed/[2009-04-03_09-00-29][IMG_4154_2)][Canon][Canon_PowerShot_G7][88231ee6052d4ad5a8700bc2a0291f5f].1.jpg|Swamp, I think in South Carolina.
renamed/[2009-05-22_09-54-04][2009-07-20-a_063][Canon][Canon_PowerShot_G7][f8ebe6c4478673e252575e1a5a99f21d].1.jpg|Lucky Dog store, Hamden, New York.
renamed/[2009-07-16_19-24-30][2009-07-20-a_314][Canon][Canon_PowerShot_G7][2322343e1d287713a19a7da83e574a73].1.jpg|Kayakers off Jersey City, Hudson River.
renamed/[2009-07-19_10-51-13][2009-07-20-a_339][Canon][Canon_PowerShot_G7][03d8a8420de03a14b7c087d130c84dbf].1.jpg|A few of Ron Mellot's trophies, Cape Horn Road, Delaware County, New York.
renamed/[2009-07-27_19-20-29][2009-08-27-a_041][Canon][Canon_PowerShot_G7][d01b4862bf9b7095ec941a1ae849d8ad].1.jpg|Trees and evening sky, our farm, Crescent Valley, Bovina Center, Delaware County, New York.
renamed/[2009-07-28_12-28-05][2009-08-27-a_050][Canon][Canon_PowerShot_G7][cadc367150961c1ccdab609e28969f44].1.jpg|The lost and lamented Biblio Barn used bookstore, Roses Brook Road, Delaware County, New York.
renamed/[2009-08-16_18-56-43][2009-08-27-a_097][Canon][Canon_PowerShot_G7][208337ef284be2caa8b5ef94d29c175d].1.jpg|Evening sky, from our porch on the farm, Crescent Valley, Bovina Center, Delaware County, New York.
renamed/[2009-08-27_18-13-03][2009-08-27-a_104][Canon][Canon_PowerShot_G7][9c28d3f3a000e0c5f805fe2e4be2be15].1.jpg|Promenade, Brooklyn Heights, New York City.
renamed/[2009-08-27_18-21-27][2009-08-27-a_112][Canon][Canon_PowerShot_G7][f4207eec2b311d1980fb9c5ca870173e].1.jpg|New York Harbor from the Brooklyn Heights promenade, New York City.
renamed/[2009-10-10_09-30-15][2009-10-23-a_092][Canon][Canon_PowerShot_G7][11f796905f538e69264de86d61a9723f].1.jpg|Steam tug \emph{Orion} in the boat museum, Stockholm, Sweden.
renamed/[2009-10-18_07-03-35][2009-10-23-a_141][Canon][Canon_PowerShot_G7][006df5104f549a10c5cdf8e46bbc2399].1.jpg|Leaves and water, Stockholm, Sweden.
renamed/[2009-10-25_16-01-19][2010-01-24a_055][Canon][Canon_PowerShot_G7][968adacd82817398235ca5322eb0ed17].1.jpg|Brooklyn Heights, New York City.
renamed/[2010-07-04_21-29-36][IMG_5644][Canon][Canon_PowerShot_G7][8246d88c77c9b706690c25f81bd16bf0].1.jpg|Bonfire after 4th of July fireworks, Peter Schelldahl's house, Bovina, Delaware County, New York.
renamed/[2010-09-03_18-05-45][IMG_5789][Canon][Canon_PowerShot_G7][ec7225a7b47a591df669208c3b038f97].1.jpg|Frogs, garden tour, Delaware County, New York.
renamed/[2010-09-03_18-38-07][IMG_5795][Canon][Canon_PowerShot_G7][d910ea481344c1aca6b5271df86e702d].1.jpg|Clouds, Delaware County, New York.
014.JPG|
renamed/[2010-12-25_11-38-58][127][Canon][Canon_PowerShot_G7][a9cdba412ab3d8c6aed6d229744f3ae7].1.jpg|John Blood and David Furber, London, United Kingdom.
renamed/[2011-02-08_18-21-39][2011-02-08-a_050][Canon][Canon_PowerShot_G7][38e83e685b82b6f2b241518a86478011].1.jpg|Heidi in her store, Frank Music Company, Manhattan.
renamed/[2011-02-08_18-38-12][2011-02-08-a_072][Canon][Canon_PowerShot_G7][ce60756d3575aaa0b08a879e15ebb57c].1.jpg|The clock at Frank Music Company, Manhattan.
renamed/[2011-02-21_17-54-51][2011-05-14-a_001][Canon][Canon_PowerShot_G7][cb9c35d724dd9f0712aa1bab9e66d0fc].1.jpg|The Brooklyn Queens Expressway from the Brooklyn Promenade, New York City.
renamed/[2011-05-09_11-17-01][2011-05-14-a_129][Canon][Canon_PowerShot_G7][f29fae70bf90f2099852d54e352e9321].1.jpg|County Cork, Republic of Ireland.
renamed/[2011-05-12_12-34-26][2011-05-14-a_232][Canon][Canon_PowerShot_G7][8d3e0ac248557479b1fd69d9029dc91b].1.jpg|Coast of County Clare, Republic of Ireland.
renamed/[2011-05-13_06-45-45][2011-05-14-a_271][Canon][Canon_PowerShot_G7][bca07c4456f7f17eeb51157fbd098157].1.jpg|Coast of County Clare, Republic of Ireland.
renamed/[2011-05-14_06-27-49][IMG_6471][Canon][Canon_PowerShot_G7][dc0c0ca690934f38911a539107163abf].1.jpg|Landscape in the Republic of Ireland.
renamed/[2011-05-17_07-45-22][IMG_6533][Canon][Canon_PowerShot_G7][c893864643271647f38f052a1a77cdfa].1.jpg|Landscape in the Republic of Ireland.
renamed/[2011-05-19_14-35-29][IMG_6664][Canon][Canon_PowerShot_G7][9c0b29dda7d027da86515d89a92b8e23].1.jpg|Coastline, Republic of Ireland.
renamed/[2011-06-25_17-51-48][2011-09-18-a_064][Canon][Canon_PowerShot_G7][93b7a99e6dc2e89de3ec33d769275178].1.jpg|Woods near Pepacton Reservoir, Delaware County, New York, United States.
renamed/[2011-06-25_18-32-46][2011-09-18-a_091][Canon][Canon_PowerShot_G7][2d89b7a332ed302da2fe585871c0805c].1.jpg|Pepacton Reservoir, Delaware County, New York, from the causeway.
renamed/[2011-09-17_13-06-30][2011-09-18-a_221][Canon][Canon_PowerShot_G7][fd8df3e316e38bd7c1e68d2f18d7182d].1.jpg|David Brossart's drafting table, Swarthmore, Pennsylvania, United States.
renamed/[2011-11-24_11-39-47][2012-02-04-a_189][Canon][Canon_PowerShot_G7][a621a7fc131faa0f7d1a6121a1e3f3a6].1.jpg|Looking across our field at Robert Bindler's farm, Crescent Valley, Bovina, Delaware County, New York.
renamed/[2011-12-25_15-53-48][2012-02-04-a_242][Canon][Canon_PowerShot_G7][e9aba7cd07ab337f4dc297c2fbf7dad8].1.jpg|Getting ready to put out a fire, Paul DePinto's house, Chatham, New Jersey, United States.
renamed/[2011-11-26_16-56-01][2012-02-04-a_224][Canon][Canon_PowerShot_G7][027083a682303e7de7035fa76743510f].1.jpg|Model trees, Tom Grove's house, Delaware County, New York.
renamed/[2012-05-19_13-18-59][2012-08-28-a_102][Canon][Canon_PowerShot_G7][78c06147355e4d72efda4c1c31428540].1.jpg|Fabric store, United States.
renamed/[2012-06-23_18-49-52][2012-08-28-a_196][Canon][Canon_PowerShot_G7][19aca482b447723ddb8b32aa8fde177c].1.jpg|Cape Horn Road, Bovina, Delaware County, New York, United States.
renamed/[2012-09-22_14-35-18][IMG_7717][Canon][Canon_PowerShot_G7][54c87ddeaafbb5e65382e70b5965f336].1.jpg|Oak leaf, Manhattan.
renamed/[2012-12-01_20-35-07][DSC00384][SONY][DSC-RX100][4fa6f0b4f10c858a18f704169c179a65].1.jpg|Manhattan.
renamed/[2012-12-22_20-12-45][DSC00769][SONY][DSC-RX100][db9a668105910211c6b8acb0fc6bd46a].1.jpg|Pacific ocean, coast of Northern California.
renamed/[2012-12-23_19-03-09][DSC00815][SONY][DSC-RX100][70a88e537b25517647ea99b868a5d9cb].1.jpg|Stream, I think in Sonoma County, Northern California.
renamed/[2012-12-24_23-21-17][DSC00831][SONY][DSC-RX100][5ea80573303c03ed7c7cdaadf4c0aed9].1.jpg|Little girl with Penbo toy, Christmas, Albany, California.
renamed/[2012-12-26_20-49-59][DSC00874][SONY][DSC-RX100][0c48cdd6a2b2def3f2c76d0621e9c6d5].1.jpg|Christmas decoration, Albany, California.
renamed/[2013-02-09_16-21-34][DSC00931][SONY][DSC-RX100][80fcb5529dfea18d2971cf84cd6533e9].1.jpg|Big snowfall attracting sledders, Central Park, New York City.
renamed/[2013-02-17_13-48-24][DSC00976][SONY][DSC-RX100][7ccb75659d59d4bb0ac1da97ec1c3b5e].1.jpg|Fracking protest, Washington, D.C., United States.
renamed/[2013-03-10_16-46-34][DSC01069][SONY][DSC-RX100][9efc31162858a51f6e3e6332aca8a187].1.jpg|Toy display, Borough Park, Brooklyn, New York.
renamed/[2013-03-31_13-55-35][DSC01114][SONY][DSC-RX100][b0aaf10ef0b5fec8f50f178b9b3577d4].1.jpg|Floor in the Truscott/Benelli house, Delaware County, New York, United States.
DSC01131.JPG|
DSC01240.JPG|
renamed/[2013-05-01_19-26-15][DSC01348][SONY][DSC-RX100][56ee9f69d83e82ba490c944db6e32d55].1.jpg|Collectibles shop, Greenwich Village, New York City.
renamed/[2013-05-04_17-12-41][DSC01369][SONY][DSC-RX100][9e7a4d6d5c4f82bfcc6bbd790e50c6eb].1.jpg|Sidewalk, Manhattan.
renamed/[2013-05-04_17-14-11][DSC01373][SONY][DSC-RX100][0eddf9ff1723848933d116c125fa2b0b].1.jpg|Store window, Upper Broadway, Manhattan.
renamed/[2013-05-05_17-16-14][DSC01413][SONY][DSC-RX100][5f3351ccaa5fd4aa7a5f50ee1c090aa5].1.jpg|Sundays the Great Organ recital, Cathedral of St. John the Divine, Upper West Side, Manhattan.
DSC01359.JPG|Flowers for sale, Chinatown, New York City.
renamed/[2013-05-31_16-57-03][20130531_165704][SAMSUNG][SGH-M919][cf88030da0b1b576bfa17d44f654c18c].1.jpg|Midtown Comics, Manhattan.
renamed/[2013-05-31_18-08-04][20130531_180805][SAMSUNG][SGH-M919][00cfd7d37b282684945b92e579a37c7d].1.jpg|Sidewalk, Manhattan.
20130604_134331.jpg|A remnant of Music Row in Manhattan.
20130619_080155.jpg|Central Park, Manhattan.
20130626_080014.jpg|Bethesda Fountain, Central Park, New York City, on my way to or from work.
renamed/[2013-07-15_18-20-48][20130715_182048][SAMSUNG][SGH-M919][f8a2cbe9f4010f99239ad31246607263].1.jpg|
DSC01577.JPG|The old ice cream stand in Stamford, New York. Replaced by a newer (but not quite as friendly and funky) version.
renamed/[2013-08-25_20-18-56][DSC01584][SONY][DSC-RX100][4ae2d8386a6cf0b557a273974c9bdfa2].1.jpg|Delaware County, New York, United States.
DSC01723.JPG|Computer music luminaries Richard Boulanger (left) and Jean-Claude Risset, and their spouses, at the 2nd International Csound Conference, Berklee School of Music, Boston.
renamed/[2013-10-26_16-03-19][DSC01780][SONY][DSC-RX100][df7e924ad414154b9dd786615c3e42c5].1.jpg|Juno Kang, Producing Music with Csound, International Csound Conference, Boston, Massachusetts, United States.
DSC02089.JPG|Naval gun, the Castillo, Barcelona, Spain.
DSC02492.JPG|Girls on the train, Spain.
DSC02503.JPG|Magazine covers, Girona, Spain.
DSC02654.JPG|
DSC02788.JPG|View from a Cathar castle, Languedoc, France.
DSC02857.JPG|The Father offering the Son, Narbonne, France.
DSC02910.JPG|Christmas carnival in Carcassone, France.
DSC03275.JPG|Christmas scene, Chatham, New Jersey.
renamed/[2014-01-30_13-36-32][DSC03287][SONY][DSC-RX100][91cb585a410b303c6fb8971e6853dcf6].1_v1.jpg|New York Public Library, Bryant Square, Manhattan. Statue of Beauty, by Frederick MacMonnies, modeled by Audrey Munson.
renamed/[2014-03-08_19-21-03][DSC03426][SONY][DSC-RX100][e705ef61a6376e1a4d7c571d25d5a1f2].1.jpg|Flushing, Queens, New York City.
renamed/[2014-07-17_08-34-49][DSC04261][SONY][DSC-RX100][d6fe17819b66ddbc3a7ab3ba87e0dc46].1.jpg|Juice wagon, Columbus Circle, Manhattan.
renamed/[2014-07-19_20-15-33][DSC04309][SONY][DSC-RX100][15d6cbab12cf97e714ccfb15e52c20b5].1.jpg|Flowers grown by Bob Reiter and Judith Lamb, on their terrace, Hamden, Delaware County, New York, United States.
renamed/[2014-07-26_19-29-03][DSC04425][SONY][DSC-RX100][4a7ea281a713be8e25852b9d5f58f9f1].1.jpg|Bill Carpenter and Cheryl Randall, Saratoga Springs, New York, United States.
renamed/[2014-08-01_19-58-36][DSC04456][SONY][DSC-RX100][187af7f204efd4075ae20c12ca60dbb2].1.jpg|Tourists and blossoming trees, Clinton, Manhattan.
renamed/[2014-08-14_20-52-21][DSC04632][SONY][DSC-RX100][26f00f9760401269963a1c3b62c83b5e].1.jpg|Promenade, Brooklyn Heights, New York City.
renamed/[2014-08-14_20-53-37][DSC04636][SONY][DSC-RX100][a538bdb99bae29800444fa671f9888f2].1.jpg|Promenade, Brooklyn Heights, New York City.
renamed/[2014-08-22_08-34-41][DSC04729][SONY][DSC-RX100][cdb097b4c38f14ec58828f0f8fd123c2].1.jpg|Water on marble coping, Midtown Manhattan.
renamed/[2014-08-28_18-19-43][DSC04791][SONY][DSC-RX100][f5d954d8fd97d77214089daab3f7d8a4].1_v1.jpg|Subway train crossing the Manhattan Bridge, from Dumbo, Brooklyn, New York City.
renamed/[2014-08-28_19-42-59][DSC04808][SONY][DSC-RX100][c041647721e94be0bf09f7b61a144a78].1.jpg|Downtown Manhattan from Brooklyn Heights, Brooklyn, New York City.
renamed/[2014-09-14_05-31-18][DSC04978][SONY][DSC-RX100][6d75ecaf41c2213fd546ee00b93113a4].1.jpg|Capitoline Museum, the Campodiglio, Rome, Italy.
renamed/[2014-09-14_18-46-45][DSC05082][SONY][DSC-RX100][ec0a6ab0d6242869a5a0418a72016eaf].1.jpg|Dance club under a bridge, Trastevere. Rome, Italy.
renamed/[2014-09-15_07-37-23][DSC05105][SONY][DSC-RX100][aa06be3e379d6916c8e06deb5dbb7463].1.jpg|Sky in a park, Trastevere, Rome, Italy.
renamed/[2014-09-15_08-47-51][DSC05134][SONY][DSC-RX100][694b831dddf88c95573a295237976132].1.jpg|Lip of a fountain in a park, Trastevere, Rome, Italy.
renamed/[2014-09-15_16-33-34][DSC05197][SONY][DSC-RX100][cd322780068d919e28f5478ac93b5b12].1.jpg|Costume jewelry display, Trastevere, Rome, Italy.
renamed/[2014-09-16_12-57-24][DSC05291][SONY][DSC-RX100][81205f4e155e5a889d64f047f27a76b6].1.jpg|Naples, Italy.
renamed/[2014-09-17_03-36-11][DSC05308][SONY][DSC-RX100][23572344b03d7d175dc751a729d9d963].1.jpg|Naples, Italy.
renamed/[2014-09-17_05-36-56][DSC05340][SONY][DSC-RX100][a16926ad5e0f19e4e5114b1c38cc7ec6].1.jpg|Naples, Italy.
renamed/[2014-09-17_05-44-49][DSC05352][SONY][DSC-RX100][9f25e8dabacfed81043845a9c27f721e].1.jpg|Posters on wall, Naples, Italy.
renamed/[2014-09-17_08-48-50][DSC05372][SONY][DSC-RX100][5366088b596d6dcf1e04f115996ab946].1.jpg|Heidi photographing the owners and staff of Ristorante Pizzeria da Nicola, Agerola, Italy. Talk about pizza and spaghetti! Very highly recommended.
renamed/[2014-09-19_09-39-20][DSC05542][SONY][DSC-RX100][07635e033555b70daf28671aea233026].1.jpg|Amalfi Coast, Italy.
renamed/[2014-09-25_12-52-14][DSC05821][SONY][DSC-RX100][153fa880ed2e5310df21060cdd5fa683].1.jpg|Harbor, Naples, from ferry to Sicily.
renamed/[2014-09-29_14-49-59][DSC05982][SONY][DSC-RX100][17d604fefb0d4dd500632bc01334e7af].1.jpg|Piazza del Duomo, Syracuse, Sicily.
renamed/[2014-10-04_09-56-41][DSC06265][SONY][DSC-RX100][b8352396242c7ed023425812ef812b94].1.jpg|Merry-go-round, park, Rome.
renamed/[2014-10-04_16-28-45][DSC06320][SONY][DSC-RX100][fcbbee70429861d27230bd94d0f830a2].1_v1.jpg|The study of our hosts at the Bed & Breakfast Arco del Lauro, Rome, Italy.
renamed/[2015-03-08_17-28-50][DSC06763][SONY][DSC-RX100][46fc7496604e86e85ed28d2fe4f9e64a].1.jpg|Merry-go-round, Brooklyn.
renamed/[2015-04-17_13-40-13][DSC06923][SONY][DSC-RX100][d43eceacdecc22ff832a89b6a324d594].1.jpg|Manhattan.
renamed/[2015-04-29_19-39-45][DSC07000][SONY][DSC-RX100][1444fbe85ed97c111e7a532ca1439139].1.jpg|Back of townhouse, Promenade, Brooklyn Heights, New York City.
renamed/[2015-08-18_20-15-27][DSC07740][SONY][DSC-RX100][c7d4475d947dd60634501b11f43ecf48].1.jpg|The Great Lawn at twilight, Central Park, Manhattan.
renamed/[2015-08-21_17-17-16][DSC07793][SONY][DSC-RX100][8a234110be08c3df8154b7b8222d75b4].1.jpg|SeaGlass merry-go-round, Battery Park, Manhattan.
renamed/[2015-09-06_13-57-21][DSC07863][SONY][DSC-RX100][45dee3e2efcb41b3e551caccc6059e9f].1.jpg|Dairy Princesses at Bovina Farm Day, Crescent Valley Road, Bovina, Delaware County, New York.
renamed/[2016-02-18_08-19-25][DSC08412][SONY][DSC-RX100][178906581e848291326540903c59bf48].1.jpg|Man praying, Wailing Wall, Temple Mount, Jerusalem.
renamed/[2016-02-27_09-07-25][DSC08613][SONY][DSC-RX100][c5c679d04fac75bfe885d041f49caef9].1.jpg|Girls on a swing, beach, Tel Aviv, Israel.
renamed/[2016-02-29_08-14-30][DSC08629][SONY][DSC-RX100][d4d3bedd1554b07e430bb028fffd457b].1.jpg|Fabric store window, Tel Aviv, Israel.
renamed/[2016-02-29_09-18-19][DSC08650][SONY][DSC-RX100][b4e489b25d253bdadc1071f3067922f2].1.jpg|Soccer balls for sale, Tel Aviv, Israel.
renamed/[2016-03-07_09-41-22][DSC08886][SONY][DSC-RX100][37eb4ec0f23d126845085ec0939a1ed8].1.jpg|This shop used to be a grocery run by my sister and her husband, Safed, Israel.
renamed/[2016-03-11_18-22-12][DSC08998][SONY][DSC-RX100][75b433239a69d9a48f0a87bbc76622a7].1.jpg|My neice Rivka, my wife Heidi, my sister Wendy, my nephew Emanuel, Tel Aviv, Israel.
renamed/[2016-03-13_13-07-33][DSC09060][SONY][DSC-RX100][cd66ab077431ed470565be60e90aaf13].1_v1.jpg|House, Haifa, Israel.
renamed/[2016-03-15_11-15-05][DSC09253][SONY][DSC-RX100][f4e88d78f3a566e5db5a1acf7c67623c].1.jpg|Ships in the ridings, Crusader walls, Akko, Israel.
renamed/[2016-03-16_10-28-58][DSC09313][SONY][DSC-RX100][784f65227b15304bb58a3e8303b329e8].1.jpg|Sea of Galilee, Israel.
renamed/[2016-03-29_08-08-23][DSC09661][SONY][DSC-RX100][4f343954bc8d52ead742dcf976abcb10].1.jpg|Mt. Scopus, no-man's land, East Jerusalem, Israel.
renamed/[2016-04-06_13-12-31][DSC09755][SONY][DSC-RX100][e50c7c9f3d613f58ac154f7a991c9cd9].1.jpg|Merry-go-round on the boardwalk in Tel Aviv, Israel.
renamed/[2016-04-07_15-33-39][DSC09844][SONY][DSC-RX100][4f68343a482dbc4c0fce97ef08c2e2ea].1.jpg|Window in the house of my sister's son Michael Mesguich, Ashdod, Israel.
renamed/[2016-04-14_16-58-19][DSC09991][SONY][DSC-RX100][31303283474ee78193594d1e2d49458e].1.jpg|Convenience store, Tel Aviv, Israel.
renamed/[2016-07-22_19-15-20][DSC00365][SONY][DSC-RX100][5fa29079ce1388e440851a65ae601eb8].1.jpg|Drain, Bovina Creamery, Bovina Center, New York.
renamed/[2016-09-09_19-02-13][DSC00615][SONY][DSC-RX100][017d30847041035aeaa5c80728795dc7].1.jpg|Garret, Greenwich Village, New York City.
XT_1585/Camera/IMG_20161006_135310102_TOP.jpg|Heidi at Bodrum, Upper West Side, Manhattan.
renamed/[2016-10-25_17-21-56][DSC00805][SONY][DSC-RX100][8e062614d3bbed5885ffb2648f83724e].1.jpg|Quebec City, Quebec, Canada.
renamed/[2016-11-04_16-28-55][DSC00837][SONY][DSC-RX100][48fcaa6370377afea6e483ef14f9efcd].1.jpg|Museum of the City of New-York, New York City.
renamed/[2016-11-05_17-39-25][DSC00875][SONY][DSC-RX100][4f803edb1d9ac20236123e7787e8f734].1.jpg|New York Harbor from Carroll Gardens, Brooklyn, New York.
renamed/[2016-11-05_17-46-07][DSC00884][SONY][DSC-RX100][bff212101c13d1a1ae1d21e281d44b85].1.jpg|Red Hook, Brooklyn, New York.
renamed/[2016-11-05_16-35-08][DSC00850][SONY][DSC-RX100][11d702afddf23ab11cc11b780ffba4de].1.jpg|Wall, Williamsburg, Brooklyn, New York City.
renamed/[2016-12-08_15-38-18][DSC00974][SONY][DSC-RX100][9068c44ac64153e42b398c79885992c7].1.jpg|Chinatown, Manhattan.
RX100/100MSDCF/DSC00956.JPG|Lower East Side, Manhattan.
RX100/100MSDCF/DSC01064.JPG|Melbourne, Australia.
RX100/100MSDCF/DSC01171.JPG|Barber shop, Brunswick East, Melbourne, Australia.
RX100/100MSDCF/DSC01306.JPG|Harbor, Hobart, Tasmania.
RX100/100MSDCF/DSC01356.JPG|Toby mugs, village restaurant, Tasmania.
RX100/100MSDCF/DSC01359.JPG|Landscape on the way to Wineglass Bay, Tasmania.
XT_1585/Camera/IMG_20170310_145854935.jpg|
RX100/100MSDCF/DSC01572.JPG|Sunset on Port Philip Bay from Hampton, Melbourne, Australia.
RX100/100MSDCF/DSC01622.JPG|Bluffs along Great Ocean Road, Australia.
RX100/100MSDCF/DSC01681.JPG|Farmlands, Victoria, Australia.
RX100/100MSDCF/DSC01705.JPG|Commuter train station, Melbourne, Australia.
RX100/100MSDCF/DSC01980.JPG|Mara Helmuth after a concert at the New York City Festival of Electroacoustic Music.
SM-950U/20180216_201312_v1.jpg|View from our terrace, Recoleta, Buenos Aires, Argentina.
SM-950U/20180216_201359.jpg|View from our terrace, Recoleta, Buenos Aires, Argentina.
RX100/100MSDCF/DSC03726.JPG|Collectibles shop window, Retiro, Buenos Aires, Argentina.
RX100/100MSDCF/DSC03861_v1.JPG|Cerro Tronador, Andes, border of Argentina and Chile.
RX100/100MSDCF/DSC03889.JPG|Mountain above Rio Negro, Puella, Patagonia, Chile.
RX100/100MSDCF/DSC03912.JPG|Stream bed near Puella, Patagonia, Chile.
RX100/100MSDCF/DSC04018.JPG|Volcan Orsono, Andes, Chile.
RX100/100MSDCF/DSC04065_v2.JPG|Cerro Tronador, Andes, border of Argentina and Chile.
RX100/100MSDCF/DSC04084.JPG|Lago Nahuel Nuapi, Argentina.
RX100/100MSDCF/DSC04140.JPG|Reliquary, Museo de Arte Hispanoamericano Isaac Fernandez Blanco, Buenos Aires, Argentina.
RX100/100MSDCF/DSC04154.JPG|Queen of Heaven, Museo de Arte Hispanoamericano Isaac Fernandez Blanco, Buenos Aires, Argentina.
SM-950U/20180324_153614.jpg|Day of Remembrance for those murdered by the junta, Buenos Aires, Argentina. We should all remember what the Argentine people did to restore democracy to their country.
RX100/100MSDCF/DSC04199.JPG|Street in the jewelry district, Buenos Aires, Argentina.
RX100/100MSDCF/DSC04235.JPG|Teatro Colon, Buenos Aires, Argentina.
RX100/100MSDCF/DSC04286.JPG|Marsh dwelling family coming to town, Tigre, Argentina.
RX100/100MSDCF/DSC04453.JPG|Rooftops from our terrace, Recoleta, Buenos Aires, Argentina.
RX100/100MSDCF/DSC04741.JPG|Fun Time, Brooklyn, New York City.
RX100/100MSDCF/DSC04927.JPG|Milon Restaurant, Lower East Side, Manhattan.
RX100/100MSDCF/DSC05122.JPG|Thank you, Price Chopper supermarket, Delhi, Delaware County, New York.
RX100/100MSDCF/DSC05143.JPG|Field Days, Margaretville, Delaware County, New York.
RX100/100MSDCF/DSC05149.JPG|Midway, Field Days, Margaretville, Delaware County, New York.
RX100/100MSDCF/DSC05239.JPG|Useful clothing, Lower East Side, Manhattan.
RX100/100MSDCF/DSC05312.JPG|Downtown from the Lower East Side, Manhattan.
SM-950U/20180827_182514_v1.jpg|The Great Lawn, Central Park, Manhattan.
RX100/100MSDCF/DSC05497.JPG|Midtown from a ferry boat, East River, Manhattan.
RX100/100MSDCF/DSC05619.JPG|Deysenroth farm, County Road 18, Delaware County, New York.
RX100/100MSDCF/DSC05620.JPG|Barn cats, Deysenroth farm, County Road 18, Delaware County, New York.
RX100/100MSDCF/DSC05625.JPG|Toys in barn, Deysenroth farm, County Road 18, Delaware County, New York.
RX100/100MSDCF/DSC05663.JPG|Chinatown, Manhattan.
RX100/100MSDCF/DSC05784.JPG|Early gear used for making electronic music, exhibit at the Performing Arts Library, Lincoln Center, Manhattan.
RX100M5/100MSDCF/DSC06075.JPG|
RX100M5/100MSDCF/DSC06552.JPG|Chinese Garden, Dunedin, New Zealand.
RX100/100MSDCF/DSC06643.JPG|Southern Alps from Lake Wakatipu, Glenorchy, South Island, New Zealand.
SM-950U/20190406_154218.jpg|Southern Alps, South Island, New Zealand.
SM-950U/20190407_154615.jpg|Back country road, South Island, New Zealand.
SM-950U/20190408_171520.jpg|Trail, Southern Alps, South Island, New Zealand.
SM-950U/20190409_150209.jpg|
SM-950U/20190411_114936.jpg|
SM-950U/20191122_141030.jpg|San Simeon, California, United States."""

output_filename2 = "/Users/michaelgogins/michael.gogins.studio/photographs/Gogins-Photographs-Content-IIa.tex"

manifest2 = """SM-950U/20190411_114936.jpg|
SM-950U/20190409_150209.jpg|
SM-950U/20190407_154615.jpg|
XT_1585/Camera/IMG_20170310_145854935.jpg|
renamed/c_2013-03-11_04-42-42.1.jpg|Scan of a slide, visual poet Karl Kempton.
renamed/c_2013-03-11_04-42-41.1.jpg|Scan of a slide, Pacific shore near Karl Kempton's house, Avila Beach, California.
renamed/c_2013-03-11_04-42-39.1.jpg|Scan of a slide, collectibles shop window, Venice Beach, California.
renamed/c_2013-03-11_04-42-38.1.jpg|Scan of a slide, amusement park midway, The Pike, Long Beach, California, probably 1973 or 1974.
renamed/c_2013-03-11_04-42-37.1.jpg|Scan of a slide, glass of ginger ale, Venice Beach, California, probably 1975.
renamed/c_2013-03-11_04-42-35.1.jpg|Scan of a slide, Cotati, Sonoma County, California.
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

output_filename3 = "/Users/michaelgogins/michael.gogins.studio/photographs/Gogins-Photographs-Content-IIIa.tex"

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
renamed/[2005-01-01_13-25-04][2005-01-01a_033][OLYMPUS_CORPORATION][C8080WZ][0cc4f0247f0e89d45a4571526d87358d].1.jpg|
renamed/[2005-02-27_20-05-01][2005-02-28a_258][OLYMPUS_CORPORATION][C8080WZ][4f662313298f585db64d537fb3542cf2].1.jpg|Bride leaving her wedding, New York City.
renamed/[2005-07-09_06-36-44][2005-07-09-a_043][OLYMPUS_CORPORATION][C8080WZ][2f611944a86ca4656f65d7ea48a9cd7b].1.jpg|Fountain at the palace of Versailles, France.
renamed/[2005-07-10_03-55-25][2005-07-10-a_041][OLYMPUS_CORPORATION][C8080WZ][5621556dc949240eadaf07aea4bca647].1.jpg|Postcards for sale, Paris, France.
renamed/[2005-12-26_19-03-55][2005-12-27a_041][OLYMPUS_CORPORATION][C8080WZ][165f51258de62c00f81134ba945bd682].1.jpg|Pacific Ocean, Northern California, United States.
2005-10-10a_048.jpg|Hillside in autumn on our farm, Crescent Valley, Bovina Center, New York.
PICT0010.JPG|Our neighbor Robert Bindler at his farm, Crescent Valley, Bovina Center, New York.
renamed/[2002-01-01_00-00-00][PICT0060][Minolta_Co_Ltd][DiMAGE_F100][b0b62f51d659715fac87c1c3938d742f].1.jpg|Matrioshka dolls, Park Avenue, New York City.
renamed/[2004-11-28_16-10-30][2004-12-01-a_036][OLYMPUS_CORPORATION][C8080WZ][20e4fe68a5b13e344f512392d6025cac].1.jpg|Sky from our farm, Crescent Valley, Bovina Center, New York.
renamed/[2005-06-17_18-10-15][2005-06-26a_010][OLYMPUS_CORPORATION][C8080WZ][f92166a5bea3a09f42dbcf667cf23b49].1.jpg|Statue of Jose Marti, Central Park South, New York City.
renamed/[2005-10-02_10-37-50][2005-10-02-a_041][OLYMPUS_CORPORATION][C8080WZ][03e111f120127f552da99ed99f49002a].1.jpg|Kitsch for sale, New York City.
renamed/[2005-12-24_14-22-27][2005-12-24a_124][OLYMPUS_CORPORATION][C8080WZ][57a546800620505241d6cb1ae06b1880].1.jpg|Sonoma County, California.
renamed/[2005-12-29_18-56-59][2005-12-30a_191][OLYMPUS_CORPORATION][C8080WZ][071807195a691e0e537540fbef1402df].1.jpg|Unkown woman, Fairfax Farmers Market, Los Angeles, California.
renamed/[2005-12-29_19-18-23][2005-12-30a_224][OLYMPUS_CORPORATION][C8080WZ][3bf39aa7f66e0de9261fd0930b12b5fc].1.jpg|Billboard, The Grove shopping centerx, Los Angeles, California.
renamed/[2006-05-20_18-37-46][Survey_-_2006-01-15_097][OLYMPUS_CORPORATION][C8080WZ][64e6408c5a84eb7a6a1155fb7327a7fa].1.jpg|Michael Williams and a friend, our apartment, New York City.
renamed/[2006-08-18_13-10-09][2006-07-18-a_051][OLYMPUS_CORPORATION][C8080WZ][64cb3cc699778fc59f76c8b932040f1e].1.jpg|Delaware Coumty Fair, Walton, New York.
renamed/[2007-03-12_11-41-10][2007-03-12-a_144][Canon][Canon_PowerShot_G7][8da3a099769a0842274d66eb7f8e891c].1.jpg|Stains on wall, New York City.
renamed/[2007-03-13_17-44-33][2007-03-13-a_059][Canon][Canon_PowerShot_G7][25d5afb2694acd89cc8a18cc0d490f55].1.jpg|Times Square, New York City.
renamed/[2007-03-13_17-47-51][2007-03-13-a_069][Canon][Canon_PowerShot_G7][0635f53ef2f63b22beed6af1d4059512].1.jpg|Times Square, New York City.
renamed/[2007-04-24_18-28-49][2007-03-31-b_218][Canon][Canon_PowerShot_G7][0023f5f85a13297e3b3c47bf22d5b175].1.jpg|Loading dock, Williamsburg, New York City.
renamed/[2007-05-20_14-09-50][2007-05-20-a_094][Canon][Canon_PowerShot_G7][9b10932b3988089fc4b36cb1cfabfa57].1.jpg|Hike in the Adirondack Mountains, New York.
renamed/[2007-05-21_18-37-14][2007-05-21-a_024][Canon][Canon_PowerShot_G7][1c1d0a9f40021421167b68ebc3d7cc76].1.jpg|Thousand Islands, St. Lawrence Seaway, New York
renamed/[2007-05-23_12-22-59][2007-05-25-a_122][Canon][Canon_PowerShot_G7][df1c2c0bc54b7603a9c4f200433ef608].1.jpg|Blossoming tree, Cornell University arboretum, Ithaca, New York.
renamed/[2007-08-05_10-25-02][2007-08-18-a_148][Canon][Canon_PowerShot_G7][4f1dbab672327455d87f4c8cb46a30b2].1.jpg|Birch bark, Bill Carpenter's yard, Slingerlands, New York.
renamed/[2007-08-27_11-11-38][2007-11-22-a_026][Canon][Canon_PowerShot_G7][32263f004507ab07625955e6d7eddda9].1.jpg|Rocking horse from Poland, bought by Heidi's parents, at our farm.
renamed/[2007-10-19_18-49-20][2007-10-22-a_066][OLYMPUS_IMAGING_CORP__][u760S760_______][05555ee9b2cefea3a54d3291f767a8e4].1.jpg|Antelope Island from the south end of the Great Salt Lake, Utah.
renamed/[2007-10-21_11-22-00][2007-10-22-a_124][OLYMPUS_IMAGING_CORP__][u760S760_______][842e50b23b0ef23277443f7cc147622a].1.jpg|My father Laird, a year or so before his death, Salt Lake City, Utah.
renamed/[2007-12-07_13-15-18][PC070125][OLYMPUS_IMAGING_CORP__][u760S760_______][9482ade77febe36bfbd18d7b646d61c6].1.jpg|New York City, self-titled.
renamed/[2007-12-22_10-35-52][2007-12-30-a_001][Canon][Canon_PowerShot_G7][6d463a8c423b2c353a456ec0598e7983].1.jpg|San Francisco Bay from the Berkeley hills.
renamed/[2008-01-19_20-35-25][P1190200][OLYMPUS_IMAGING_CORP__][u760S760_______][105b77651b262a04653436810bd24652].1.jpg|George and Carole Silvers' doll house, Crescent Valley, Bovina Center, New York.
renamed/[2008-09-17_18-17-05][2008-10-08-a_155][Canon][Canon_PowerShot_G7][ee38a79382b5c26b0004e134a62a0620].1.jpg|Manhattan.
renamed/[2008-10-05_16-30-49][2008-10-08-a_224][Canon][Canon_PowerShot_G7][0c153b55498ecb3c12bfe1d4fa0ceaaf].1.jpg|My father, Heidi, my stepbrother Scott Fechner, and his wife Claudia, Brighton, Utah.
renamed/[2008-11-06_14-58-54][2008-11-06-a_009][Canon][Canon_PowerShot_G7][f914cb79e53df3fcf69ea4e106b6c87a].1.jpg|The Wasatch Front above Farmington Bay Wildlife Refuge, Utah.
renamed/[2008-11-06_18-25-11][2008-11-06-a_021][Canon][Canon_PowerShot_G7][0730ea6941897f8e7f8d09d6fd3f97db].1.jpg|Cache Valley, Utah and Wyoming.
renamed/[2008-12-21_14-06-33][2008-12-31-a_160][Canon][Canon_PowerShot_G7][b6f135162a820b2797a244d53cc44f9c].1.jpg|Merry-go-round, London.
renamed/[2008-12-23_09-26-06][2008-12-31-a_267][Canon][Canon_PowerShot_G7][41ac1582c91cfedfde1ab1587ca14521].1.jpg|Professor John ffitch, maintainer of Csound, in his office at the University of Bath, United Kingdom.
renamed/[2008-12-29_10-10-04][2008-12-31-a_420][Canon][Canon_PowerShot_G7][ebcc482203fb0d3473d03c974f1a1cdd].1.jpg|South Devon, United Kingdom.
renamed/[2008-12-31_07-31-46][2008-12-31-a_484][Canon][Canon_PowerShot_G7][38bf2d0396733e566a720cbdc4f03c57].1.jpg|Cliffs near Lyme Regis, South Devon, United Kingdom.
renamed/[2008-12-31_09-09-29][2008-12-31-a_503][Canon][Canon_PowerShot_G7][0f63698c9866de49fa6e38e09624f92c].1.jpg|South Devon, United Kingdom.
renamed/[2009-03-07_15-38-21][2009-03-08-a_155][Canon][Canon_PowerShot_G7][ba84389a7d29f0486b303577b05ce949].1.jpg|Gene Bertoncini.
renamed/[2009-03-08_08-57-25][2009-03-08-a_259][Canon][Canon_PowerShot_G7][2f6839f9261e9e527d5f901023ac8267].1.jpg|Eggs from Heidi's hens, in our kitchen in Manhattan.
renamed/[2009-03-27_14-37-47][IMG_3933_2)][Canon][Canon_PowerShot_G7][06fe0850c7fde687debf1190e8a6347b].1.jpg|Florida.
renamed/[2009-03-29_19-11-08][IMG_4016_2)][Canon][Canon_PowerShot_G7][adeb220d561845d5406081b8dbb4e0e9].1.jpg|My father's sisters Corinne and Ginger, Heidi, and Corinne's husband Herb, in Florida.
renamed/[2009-03-30_18-57-34][IMG_4062][Canon][Canon_PowerShot_G7][773fb2e2e258cc766b12443d48ce0044].1.jpg|South (I think) Carolina coast.
renamed/[2009-04-01_13-33-33][IMG_4121_2)][Canon][Canon_PowerShot_G7][2c5ef7982116c1a37230654d5e6fa9a0].1.jpg|Charleston, South Carolina.
renamed/[2009-04-03_09-00-29][IMG_4154_2)][Canon][Canon_PowerShot_G7][88231ee6052d4ad5a8700bc2a0291f5f].1.jpg|Swamp, I think in South Carolina.
renamed/[2009-05-22_09-54-04][2009-07-20-a_063][Canon][Canon_PowerShot_G7][f8ebe6c4478673e252575e1a5a99f21d].1.jpg|Lucky Dog store, Hamden, New York.
renamed/[2009-07-16_19-24-30][2009-07-20-a_314][Canon][Canon_PowerShot_G7][2322343e1d287713a19a7da83e574a73].1.jpg|Kayakers off Jersey City, Hudson River.
renamed/[2009-07-19_10-51-13][2009-07-20-a_339][Canon][Canon_PowerShot_G7][03d8a8420de03a14b7c087d130c84dbf].1.jpg|A few of Ron Mellot's trophies, Cape Horn Road, Delaware County, New York.
renamed/[2009-07-27_19-20-29][2009-08-27-a_041][Canon][Canon_PowerShot_G7][d01b4862bf9b7095ec941a1ae849d8ad].1.jpg|Trees and evening sky, our farm, Crescent Valley, Bovina Center, Delaware County, New York.
renamed/[2009-08-16_18-56-43][2009-08-27-a_097][Canon][Canon_PowerShot_G7][208337ef284be2caa8b5ef94d29c175d].1.jpg|Evening sky, from our porch on the farm, Crescent Valley, Bovina Center, Delaware County, New York.
renamed/[2009-08-27_18-13-03][2009-08-27-a_104][Canon][Canon_PowerShot_G7][9c28d3f3a000e0c5f805fe2e4be2be15].1.jpg|Promenade, Brooklyn Heights, New York City.
renamed/[2009-10-25_16-01-19][2010-01-24a_055][Canon][Canon_PowerShot_G7][968adacd82817398235ca5322eb0ed17].1.jpg|Brooklyn Heights, New York City.
renamed/[2009-07-28_12-28-05][2009-08-27-a_050][Canon][Canon_PowerShot_G7][cadc367150961c1ccdab609e28969f44].1.jpg|The lost and lamented Biblio Barn used bookstore, Roses Brook Road, Delaware County, New York.
renamed/[2009-08-27_18-21-27][2009-08-27-a_112][Canon][Canon_PowerShot_G7][f4207eec2b311d1980fb9c5ca870173e].1.jpg|New York Harbor from the Brooklyn Heights promenade, New York City.
renamed/[2009-10-10_09-30-15][2009-10-23-a_092][Canon][Canon_PowerShot_G7][11f796905f538e69264de86d61a9723f].1.jpg|Steam tug \emph{Orion} in the boat museum, Stockholm, Sweden.
renamed/[2009-10-18_07-03-35][2009-10-23-a_141][Canon][Canon_PowerShot_G7][006df5104f549a10c5cdf8e46bbc2399].1.jpg|Leaves and water, Stockholm, Sweden.
renamed/[2010-07-04_21-29-36][IMG_5644][Canon][Canon_PowerShot_G7][8246d88c77c9b706690c25f81bd16bf0].1.jpg|Bonfire after 4th of July fireworks, Peter Schelldahl's house, Bovina, Delaware County, New York.
renamed/[2010-09-03_18-05-45][IMG_5789][Canon][Canon_PowerShot_G7][ec7225a7b47a591df669208c3b038f97].1.jpg|Frogs, garden tour, Delaware County, New York.
renamed/[2010-09-03_18-38-07][IMG_5795][Canon][Canon_PowerShot_G7][d910ea481344c1aca6b5271df86e702d].1.jpg|Clouds, Delaware County, New York.
renamed/[2010-12-25_11-38-58][127][Canon][Canon_PowerShot_G7][a9cdba412ab3d8c6aed6d229744f3ae7].1.jpg|John Blood and David Furber, London, United Kingdom.
renamed/[2011-02-08_18-21-39][2011-02-08-a_050][Canon][Canon_PowerShot_G7][38e83e685b82b6f2b241518a86478011].1.jpg|Heidi in her store, Frank Music Company, Manhattan.
renamed/[2011-02-08_18-38-12][2011-02-08-a_072][Canon][Canon_PowerShot_G7][ce60756d3575aaa0b08a879e15ebb57c].1.jpg|The clock at Frank Music Company, Manhattan.
renamed/[2011-02-21_17-54-51][2011-05-14-a_001][Canon][Canon_PowerShot_G7][cb9c35d724dd9f0712aa1bab9e66d0fc].1.jpg|The Brooklyn Queens Expressway from the Brooklyn Promenade, New York City.
renamed/[2011-05-09_11-17-01][2011-05-14-a_129][Canon][Canon_PowerShot_G7][f29fae70bf90f2099852d54e352e9321].1.jpg|County Cork, Republic of Ireland.
renamed/[2011-05-13_06-45-45][2011-05-14-a_271][Canon][Canon_PowerShot_G7][bca07c4456f7f17eeb51157fbd098157].1.jpg|Coast of County Kerry, Republic of Ireland."""

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
images_for_dates = {}
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
    print("bb result: ", result)
    result = result.stdout.split("\n")
    result = result[2].split()
    return 'bb= 0 0 {} {}'.format(str(result[3]), str(result[4]))    
    
def process(manifest, output_filename_):
    output = open(output_filename_, 'w')
    photos = manifest.split("\n")
    for photo in photos:
        print("Processing photo: ", photo);
        try:
            filename, caption = photo.split("|")
        except:
            print("I*** Missed: " + pathname + "\n");
            next
        pathname = os.path.join(image_root, filename)
        basename = os.path.basename(pathname)
        pathname = r"" + pathname
        dropbox_name = r"/images/{}".format(filename);
        u_dropbox_filepath = unicodedata.normalize('NFC', dropbox_name).lower()
        u_local_filepath = unicodedata.normalize('NFC', pathname)
        #~ # If file is in sync, process it; otherwise, download it from Dropbox 
        #~ # and then process it.
        file_size = os.path.getsize(pathname)
        if file_size == 0:
            try:
                print("\nUpdating:\n\t'{}'\nfrom Dropbox:\n\t'{}'".format(u_local_filepath, u_dropbox_filepath))
                command = r"open {}".format(pathname)
                print("command: ", command)
                os.system(command)
                print("\tUpdated...")
                pass
            except:
                traceback.print_exc()
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
            metadata = []
            for tag in tags:
                try:
                    value = r"" + str(image_with_metadata.get(tag))
                    if value != "None":
                        metadata.append(r"{}: {}".format(names_for_tags[tag], value));
                except:
                    pass
            metadata_text = "\n".join(metadata)
            page_text = page_template.format(basename=basename, text=caption, bb=bb, metadata=metadata_text)
            print(page_text)
            output.write(page_text)
    
process(manifest1, output_filename1)
#process(manifest2, output_filename2)
#process(manifest3, output_filename3)

print("Check chronological order:\n")

for item in images_for_dates.items():
    print(item[0], item[1])
    
print("\nTotal timestamped images: " + str(len(images_for_dates.items())))
                    
    
