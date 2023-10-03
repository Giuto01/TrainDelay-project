import requests
import pandas as pd

f = open(".stations.pl", "w+")
df = pd.read_csv('../Dataset/stazioni_regioni.csv')

f.write("%station(stationID, station_name, region_name).\n")

for i in df.index:
    stationID = df["idStazione"][i]
    station_name = df["stazione"][i]
    x = requests.get(f"http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno/regione/{stationID}")
    if x.status_code == 200:
        region_code = x.json()
        # Replace region code with the name of region
        if region_code == 0:
            region_name = "Ita"
        elif region_code == 1:
            region_name = "Lombardia"
        elif region_code == 2:
            region_name = "Liguria"
        elif region_code == 3:
            region_name = "Piemonte"
        elif region_code == 4:
            region_name = "Valle d'Aosta"
        elif region_code == 5:
            region_name = "Lazio"
        elif region_code == 6:
            region_name = "Umbria"
        elif region_code == 7:
            region_name = "Molise"
        elif region_code == 8:
            region_name = "Emilia Romagna"
        elif region_code == 9:
            region_name = "Trentino Alto Adige"
        elif region_code == 10:
            region_name = "Friuli Venezia Giulia"
        elif region_code == 11:
            region_name = "Marche"
        elif region_code == 12:
            region_name = "Veneto"
        elif region_code == 13:
            region_name = "Toscana"
        elif region_code == 14:
            region_name = "Sicilia"
        elif region_code == 15:
            region_name = "Basilicata"
        elif region_code == 16:
            region_name = "Puglia"
        elif region_code == 17:
            region_name = "Calabria"
        elif region_code == 18:
            region_name = "Campania"
        elif region_code == 19:
            region_name = "Abruzzo"
        elif region_code == 20:
            region_name = "Sardegna"
        elif region_code == 22:
            region_name = "Trentino Alto Adige"
        # Save selected info in a csv file   
        string = f"station({stationID.lower()}, \"{station_name}\", \"{region_name}\").\n"
        print(string)
        f.write(string)
    else:
        string = f"station({stationID.lower()}, \"{station_name}\", \"Unavailable\").\n"
        f.write(string)

f.close()
