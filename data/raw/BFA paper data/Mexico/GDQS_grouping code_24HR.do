*Modified by Simone passarelli on 11/20/20 to add food codes to dataset
cd "/Users/Simone/Dropbox/Github/subnational_distributions/data/"
use "raw/Mexico/Base ENSANUT 2016 entrega_11_11_2020.dta", clear

*changed cancon_f_5 to canconc_f_6
*changed codigomex to id_ing

*Mixed dishes

*sweets and ice cream
gen sic=1 if inlist(alim_id, 1148, 1150, 1156, 1161, 1164, 1198, 1216, 1217, 1219, 1220, 1222, 1225, 1239, 1388)
replace sic=1 if strpos(alim, "BODOQUE DULCE, PREPARACI”N ESTANDARIZADA")
replace sic=1 if strpos(alim, "CAPIROTADA, PREPARACI”N ESTANDARIZADA")
replace sic=1 if strpos(alim, "CHURRO PREPARADO")
replace sic=0 if sic==.

*refined grains
gen rg=1 if inlist(alim_id, 1155)
replace rg=1 if strpos(alim, "GORDITA DE ELOTE DULCE, PREPARACI”N ESTANDARIZADA")
replace rg=1 if strpos(alim, "EMPANADA DULCE")
replace rg=0 if rg==.

*aguas
gen aguas=1 if inlist(alim_id, 1452, 1453, 1454, 1455, 1456, 1457, 1458, 1459, 1460, 1461, 1462, 1463, 1464, 1465, 1466, 1467, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1478, 1479, 1481, 1482, 1483, 1484, 1485, 1488, 1664, 1678, 1714, 1715, 1872)
replace aguas=1 if strpos(alim, "AGUA CON AZUCAR")
replace aguas=0 if aguas==.

*mixed dishes identifier:
gen prep=0
replace prep=1 if sic==1
replace prep=1 if rg==1
replace prep=1 if aguas==1


*Individual foods

*1. Dark green leafy vegetables
gen group1=cancon_f_6 if inlist(id_ing, 99, 113, 145, 158, 160, 168, 169, 171, 180, 181, 185, 187, 190, 198, 199, 201, 202, 1345, 5006, 108) & prep==0
replace group1=0 if group1==.

*2. Cruciferous vegetables
gen group2=cancon_f_6 if inlist(id_ing, 111, 146, 148, 149) & prep==0
replace group2=0 if group2==.

*3. deep orange vegetables
gen group3=cancon_f_6 if inlist(id_ing, 114, 116, 161, 203, 1095) & prep==0
replace group3=0 if group3==.

*4. deep orange fruits
gen group4=cancon_f_6 if inlist(id_ing, 221, 226, 254, 256, 257, 258, 261, 264, 265, 272, 273, 290, 4004) & prep==0
replace group4=0 if group4==.

*5. deep orange tubers
gen group5=cancon_f_6 if inlist(id_ing, 205, 206) & prep==0
replace group5=0 if group5==.

*6. other vegetables
gen group6=cancon_f_6 if inlist(id_ing, 104, 105, 106, 107, 109, 112, 115, 118, 119, 121, 122, 126, 127, 128, 129, 130, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 144, 159, 166, 175, 176, 177, 178, 179, 183, 184, 186, 188, 189, 192, 196, 197, 510, 537, 544, 630, 692, 1047, 1203, 1225, 1248, 4016, 1519, 4001, 547, 543, 545, 548, 1255) & prep==0
replace group6=0 if group6==.

*7. citrus fruits
gen group7=cancon_f_6 if inlist(id_ing, 251, 252, 253, 255, 4006, 267, 268, 269, 270, 291) & prep==0
replace group7=0 if group7==.

*8. other fruits
gen group8=cancon_f_6 if inlist(id_ing, 100, 101, 102, 103, 217, 219, 220, 222, 225, 227, 230, 231, 235, 236, 237, 241, 243, 244, 246, 247, 249, 250, 259, 4002, 274, 275, 4007, 276, 277, 278, 279, 281, 282, 283, 287, 4005, 289, 293, 295, 297, 300, 301, 565, 672, 847, 1057, 1071, 1163) & prep==0
replace group8=0 if group8==.

*9. legumes
gen group9=cancon_f_6 if inlist(id_ing, 58, 60, 57, 63, 77, 1509, 61, 64, 65, 66, 67, 68, 69, 71, 72, 1126, 1514, 78, 79, 1069, 1394, 124, 167, 452, 1192, 598, 1261, 1054, 1094, 1156, 1256) & prep==0
replace group9=0 if group9==.

*10. nuts and seeds
gen group10=cancon_f_6 if inlist(id_ing, 82, 83, 85, 4014, 86, 87, 88, 91, 92, 94, 95, 97, 1136, 1258, 4015) & prep==0
replace group10=0 if group10==.

*11. poultry
gen group11=cancon_f_6 if inlist(id_ing, 337, 343, 363, 366, 367, 1101, 368, 686, 4011, 4020, 1523, 1064, 4013, 1066, 1100, 1068, 1358, 4012, 4008, 4010, 935, 306, 307, 308, 320, 341, 389, 424, 425, 328, 378) & prep==0
replace group11=0 if group11==.

*12. fish
gen group12=cancon_f_6 if inlist(id_ing, 336, 382, 383, 1259, 1376, 384, 385, 386, 388, 395, 396, 397, 398, 402, 403, 404, 408, 409, 410, 413, 414, 417, 418, 419, 420, 421, 422, 423, 1091, 1097, 1389, 400, 381, 394, 1533, 393, 392, 405, 1060, 1254, 411, 1081, 416) & prep==0
replace group12=0 if group12==.

*13. whole grains
gen group13=cancon_f_6 if inlist(id_ing, 1, 9, 11, 15, 17, 29, 50, 56, 43, 514, 515, 522, 523, 524, 585, 595, 596, 601, 689, 690, 704, 1080, 1089, 1193, 1186, 1215, 33, 156, 1520, 53, 615, 155, 591, 711, 1034, 1036, 1202, 1511) & prep==0
replace group13=0 if group13==.

*14. liquid oils
gen group14=cancon_f_6 if inlist(id_ing, 484, 485, 486, 487, 489, 490, 491, 492, 1056) & prep==0
replace group14=0 if group14==.

*15. low-fat dairy
gen group15=cancon_f_6 if inlist(id_ing, 441, 442, 444, 447, 459, 478, 476, 1143, 531, 1108, 1250, 1110, 1074, 1217, 1221, 1109, 1111) & prep==0
replace group15=0 if group15==.

*16. eggs
gen group16=cancon_f_6 if inlist(id_ing, 426, 431, 432, 438, 480, 910) & prep==0
replace group16=0 if group16==.

*17. white roots and tubers
gen group17=cancon_f_6 if inlist(id_ing, 209, 210, 211, 1526, 1527, 214, 215, 248, 668, 669, 280, 665, 666) & prep==0
replace group17=0 if group17==.

*18. red meat 
gen group18=cancon_f_6 if inlist(id_ing, 303, 304, 324, 327, 332, 333, 1524, 1525, 309, 310, 311, 312, 313, 314, 315, 317, 319, 316, 329, 330, 342, 344, 349, 352, 379, 358, 365, 372, 375, 1131, 1133) & prep==0
replace group18=0 if group18==.

*19. processed meat
gen group19=cancon_f_6 if inlist(id_ing, 326, 351, 346, 347, 353, 354, 370, 374, 377, 1098, 1207, 1355, 1369, 1378, 1379, 321, 322) & prep==0
replace group19=0 if group19==.

*20. refined grains and baked goods
gen group20=cancon_f_6 if inlist(id_ing, 3, 4, 7, 8, 16, 19, 25, 27, 623, 28, 40, 41, 44, 1346, 46, 47, 48, 518, 648, 699, 709, 1209, 1212, 1277, 1350, 19, 1375, 1386, 23, 30, 24, 26, 31, 34, 37, 38, 39, 52, 55, 519, 616, 710, 1077, 1127, 1390, 42, 45, 516, 520, 525, 535, 538, 539, 540, 549, 550, 551, 558, 559, 561, 562, 567, 572, 573, 575, 576, 577, 584, 587, 600, 602, 637, 655, 658, 703, 714, 730, 731, 1085, 1189, 1278, 1353, 958, 542, 599, 664) & prep==0
replace group20=cancon_f_6 if rg==1
replace group20=0 if group20==.

*21. sugar-sweetened beverages
gen group21=cancon_f_6 if inlist(id_ing, 512, 513, 736, 737, 800, 744, 735, 738, 748, 1053, 1072, 1276) & prep==0
replace group21=0 if group21==.

*22. sweets and ice cream
gen group22=cancon_f_6 if inlist(id_ing, 18, 684, 705, 713, 1351, 1354, 35, 223, 439, 1124, 501, 502, 503, 506, 508, 509, 553, 554, 555, 556, 557, 569, 570, 590, 641, 643, 679, 682, 588, 589, 603, 605, 606, 607, 608, 617, 626, 1198, 631, 632, 642, 644, 647, 650, 1148, 657, 661, 662, 1141, 1142, 663, 673, 675, 680, 706, 804, 808, 953, 1059, 1087, 1090, 1149, 1154, 1174, 1180, 1223, 1481, 1196, 1231, 1232, 1233, 1240, 1253, 1265, 1268, 1269, 1270, 1271, 1272, 1273, 1275) & prep==0
replace group22=cancon_f_6 if sic==1
replace group22=0 if group22==.

*23. high-fat dairy
gen group23=cancon_f_6 if inlist(id_ing, 443, 440, 445, 448, 449, 454, 455, 456, 457, 458, 461, 462, 464, 465, 466, 467, 1263, 468, 469, 471, 473, 481, 482, 1052, 1070, 1266, 1352, 1387, 430, 446, 483, 1075, 636, 639, 1113, 1115, 1122, 460, 463, 475, 1367, 428, 429, 493, 1132) & prep==0
replace group23=0 if group23==.

*24. juice
gen group24=cancon_f_6 if inlist(id_ing, 204, 224, 232, 266, 563, 629, 1366, 634, 652, 654, 681, 708, 718, 719, 1106 1109, 633, 1183, 1206, 1147, 1168, 1214, 1348) & prep==0
replace group24=cancon_f_6 if aguas==1
replace group24=0 if group24==.

*25. fried foods
gen group25=cancon_f_6 if inlist(id_ing, 525, 542, 561, 599, 665, 666, 668, 325, 1349, 59, 166, 234, 587) 
replace group25=cancon_f_6 if inlist(alim_id, 166, 1098, 1099, 1738, 1742, 554, 1148, 1150, 1153, 593, 594, 595, 596, 597, 598, 599, 603, 604, 600, 601, 1692, 602, 607, 1781, 610, 611, 637, 638, 1748, 639, 640, 693, 701, 694, 695, 696, 697, 698, 699, 700, 1758, 702, 703, 704, 1155)
replace group25=cancon_f_6 if strpos(alim, "BU—UELOS, PREPARACI”N ESTANDARIZADA")
replace group25=cancon_f_6 if strpos(alim, "PAPAS FRITA")
replace group25=cancon_f_6 if strpos(alim, "CHURRO PREPARADO")
replace group25=cancon_f_6 if strpos(alim, "NUGGETS DE POLLO PARA PREPARAR")
replace group25=cancon_f_6 if strpos(alim, "GORDA DE CARNE")
replace group25=cancon_f_6 if strpos(alim, "QUESADILLA DE CAZON, PREPARACI”N ESTANDARIZADA")
replace group25=cancon_f_6 if strpos(alim, "GORDITA DE ELOTE DULCE, PREPARACI”N ESTANDARIZADA")
replace group25=0 if group25==.


save "raw/Mexico/Base ENSANUT 2016 entrega_11_11_2020_coded.dta", replace
