from pyswip import Prolog

prolog = Prolog()

prolog.consult("Knowledge base/trenitalia.pl")
prolog.consult("Knowledge base/rules.pl")
prolog.consult("Knowledge base/station.pl")
#s = input("Inserisci il nome della stazione di partenza: ").upper()
#v = input("Inserisci il nome della stazione di arrivo: ").upper()
#o = input("Inserisci l'orario (hh:mm): ").replace(".", ":").replace(" ", "") 
l = list(prolog.query("cerca_nome_stazione_da_a_ora(\"BARI CENTRALE\", \"LECCE\", \"07:00\", Stazioni)."))

#l = list(prolog.query(f"cerca_nome_stazione_da_a_ora(\"{s}\", \"{v}\", \"{o}\", Stazione)."))

list_trainsID = l[0]["Stazioni"]


DepartureID = "DepartureID"
ArrivalID = "ArrivalID"
DepartureTime = "DepartureTime"
ArrivalTime = "ArrivalTime"

for trainID in list_trainsID:
    print("*******************************************************")
    a = list(prolog.query(f"get_train_fact({trainID}, Type, DepartureID, ArrivalID, DepartureTime, ArrivalTime)."))
    print(f"ID : {trainID} \nPartenza : {a[0][DepartureID]} ({a[0][DepartureTime]}) \nArrivo : {a[0][ArrivalID]} ({a[0][ArrivalTime]})")
    print("*******************************************************")


#print(l[0]["Stazioni"])


# s11119, s11145, '12:20', '14:13'