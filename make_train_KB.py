import requests

f = open("Knowledge base/trenitalia.pl", "a")
f.write("train(trainID, train_type, originID, arrivalID, departure_time, arrival_time, train_stops).")
for trainID in range(1000000):
    x = requests.get(f'http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno/cercaNumeroTreno/{trainID}')
    # Check the existence of a train with that id 
    if x.status_code == 200:
        train_info = x.json()
        origin_stationID = train_info["codLocOrig"]
        temp = train_info["dataPartenza"]
        try:
            y = requests.get(f"http://www.viaggiatreno.it/infomobilita/resteasy/viaggiatreno/andamentoTreno/{origin_stationID}/{trainID}/{temp}")
            train_route_info = y.json()
            arrivalID = train_route_info["idDestinazione"]
            originID = train_route_info["idOrigine"]
            departure_time = train_route_info["compOrarioPartenzaZero"]
            arrival_time = train_route_info["compOrarioArrivoZero"]
            train_type = train_route_info["compTipologiaTreno"]
            train_stops = []

            for stops in range(len(train_route_info["fermate"])):
                train_stops.append(((train_route_info["fermate"])[stops])["id"])

            # Save selected info in a csv file
            string = f"train({trainID}, {train_type}, {originID}, {arrivalID}, {departure_time}, {arrival_time}, {train_stops}).\n".lower().replace("\'", "")
            f.write(string)
        except:
            print("Error") 
