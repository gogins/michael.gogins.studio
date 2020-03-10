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
renamed/[2008-09-17_18-17-05][2008-10-08-a_155][Canon][Canon_PowerShot_G7][ee38a79382b5c26b0004e134a62a0620].1.jpg|Manhattan, New York City.
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
                    
    
