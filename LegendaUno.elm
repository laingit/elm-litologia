module LegendaUno exposing (json)


json : String
json =
    """
    {"data":[
      {"rgb":"#FDCEAD","liv_2_desc":"Leucograniti, Leucomicrograniti,  Graniti s.l.,  Leucosienograniti, Microsienograniti, Sienograniti","liv_2":"A1.1"},
      {"rgb":"#FCAD7D","liv_2_desc":"Monzograniti, Leucomonzograniti, 'Granodioriti monzogranitiche' Auct.","liv_2":"A1.2"},
      {"rgb":"#E876B9","liv_2_desc":"Granodioriti, Granodioriti tonalitiche, Microgranodioriti, Granodioriti monzogranitiche","liv_2":"A1.3"},
      {"rgb":"#A286BF","liv_2_desc":"Tonaliti, Tonaliti granodioritiche, Tonaliti   quarzodioritiche","liv_2":"A1.4"},
      {"rgb":"#805EAB","liv_2_desc":"Gabbri, Quarzogabbri, Gabbrodioriti, Dioriti, Noriti, masse basiche gabbroidi","liv_2":"A1.5"},
      {"rgb":"#D7457F","liv_2_desc":"Sieniti, Episieniti, Sieniti monzonitiche","liv_2":"A1.6"},
      {"rgb":"#C47900","liv_2_desc":"Rioliti e Riodaciti ","liv_2":"A2.1"},
      {"rgb":"#D59D38","liv_2_desc":"Daciti","liv_2":"A2.2"},
      {"rgb":"#AB773F","liv_2_desc":"Basalti alcalini, Trachibasalti, Hawaiiti, Mugeariti, Fonoliti, Fonoliti   tefritiche","liv_2":"A2.3"},
      {"rgb":"#E87B00","liv_2_desc":"Andesiti e Andesiti basaltiche","liv_2":"A2.4"},
      {"rgb":"#C29C6A","liv_2_desc":"Basalti, Basalti andesitici","liv_2":"A2.5"},
      {"rgb":"#DD8235","liv_2_desc":"Rocce ultrabasiche, Basaniti","liv_2":"A2.6"},
      {"rgb":"#FDBB67","liv_2_desc":"Trachiti, Latiti","liv_2":"A2.7"},
      {"rgb":"#E13718","liv_2_desc":"Filoni e ammassi acidi (quarzo, riolitici, riodacitici, pegmatitici, aplitici, aplopegmatitici,   dacitici)","liv_2":"A3.1"},
      {"rgb":"#8096C6","liv_2_desc":"Filoni  e ammassi basici (basaltici) e intermedio-basici (andesitici, andesitico-basaltici, dioritici, sienitici, quarzoandesitici)","liv_2":"A3.2"},
      {"rgb":"#95CA78","liv_2_desc":"Metarioliti, Metariodaciti, 'Porfiroidi' Auct.,  Metavulcaniti acide","liv_2":"B1.1"},
      {"rgb":"#9BD355","liv_2_desc":"Migmatiti acide, Diatessiti, Ortogneiss granodioritici, Ortogneiss leucogranitici, Aplopegmatiti e Pegmatiti   foliate","liv_2":"B1.2"},
      {"rgb":"#4AAA62","liv_2_desc":"Migmatiti basiche, Eclogiti, Anfiboliti, Metatessiti","liv_2":"B1.3"},
      {"rgb":"#6EB451","liv_2_desc":"Metagabbri, Metadoleriti, Metavulcaniti basiche, Metaepiclastiti","liv_2":"B1.4"},
      {"rgb":"#D6D641","liv_2_desc":"Rocce parametamorfiche terrigene: Filladi, Micascisti, Gneiss, Miloniti, Filoniti, Fels, Quarziti, Metaconglomerati, Metarenarie,Metargilliti, Liditi, Diaspri","liv_2":"B2.1"},
      {"rgb":"#5084BC","liv_2_desc":"Rocce parametamorfiche   carbonatiche: Marmi, Marmi dolomitici, Marmi azoici, Contattiti, Metacalcari, Metadolomie, 'Calcari grigi' Auct., 'Dolomia rigata' Auct.,'Dolomia gialla' Auct.,  Calcari silicizzati","liv_2":"B2.2"},
      {"rgb":"#DCDCDC","liv_2_desc":"Depositi terrigeni antropici (saline, vasche di salificazione, aree di rispetto lagunare, discariche: minerarie, industriali, per inerti, per rifiuti solidi urbani; materiali di riporto e aree bonificate)","liv_2":"C1.1"},
      {"rgb":"#DDF2EA","liv_2_desc":"Depositi   terrigeni continentali di conoide e piana alluvionale (ghiaie, sabbie, limi, argille), (conglomerati, arenarie, siltiti, peliti) ","liv_2":"C1.2"},
      {"rgb":"#FFFFCF","liv_2_desc":"Depositi terrigeni continentali legati a gravità (detriti di versante, frane, coltri eluvio-colluviali, 'debris avalanches', brecce)","liv_2":"C1.3"},
      {"rgb":"#E6F5A4","liv_2_desc":"Depositi terrigeni palustri, lacustri, lagunari (limi, argille limose, fanghi torbosi con materia organica anche con intercalazioni di   sabbie, selci)","liv_2":"C1.4"},
      {"rgb":"#FFFF7F","liv_2_desc":"Depositi terrigeni litorali (ghiaie, sabbie, arenarie, conglomerati)","liv_2":"C1.5"},
      {"rgb":"#F3C643","liv_2_desc":"Depositi terrigeni marini (siltiti, argilliti, peliti) ","liv_2":"C1.6"},
      {"rgb":"#FEE88A","liv_2_desc":"Depositi terrigeni eolici (sabbie, arenarie)","liv_2":"C1.7"},
      {"rgb":"#FFED00","liv_2_desc":"Depositi terrigeni fluvio-deltizi (sabbie, microconglomerati, arenarie carbonatiche, siltiti   argillose)","liv_2":"C1.8"},
      {"rgb":"#C70155","liv_2_desc":"Depositi terrigeni residuali (Bauxiti, paleosuoli)","liv_2":"C1.9"},
      {"rgb":"#95D6D0","liv_2_desc":"Depositi carbonatici lacustri e lagunari (Calcari, Dolomie, Calcari silicizzati, Travertini)","liv_2":"C2.1"},
      {"rgb":"#80A3CD","liv_2_desc":"Depositi carbonatici marini (Marne, Calcari, Calcari dolomitici, Calcari oolitici, Calcari bioclastici, Calcareniti)","liv_2":"C2.2"},
      {"rgb":"#4EBBC4","liv_2_desc":"Depositi vulcano-sedimentari di   ambienti fluvio-lacustri e lagunari (Epiclastiti, Tufiti, Tufi, Cineriti, Vulcaniti, sedimenti clastici(sabbioso-siltoso-arenacei) e indistinti ","liv_2":"C3.1"},
      {"rgb":"#99DDFF","liv_2_desc":"Canali","liv_2":"Canali"},
      {"rgb":"#FFFFFF","liv_2_desc":"Laghi","liv_2":"Laghi"},
    {"rgb":"#DCDCDC","liv_2_desc":"Non Classificato","liv_2":"nc"}
    ]}
    """
