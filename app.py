import warnings
import inquirer
import options
import networkx as nx
import joblib

def mainMenu():

    while True:

        questions = [
            inquirer.List('choice',
                          message="Scegli un'opzione",
                          choices=[
                              '🚄 Cerca treno tra due stazioni',
                              '📍 Cerca itinerario',
                              '🚪 Uscire'
                          ]),
        ]

        answer = inquirer.prompt(questions)

        if answer['choice'] == '🚄 Cerca treno tra due stazioni':

            departure = input("Inserisci la stazione di partenza: ").upper()
            arrival = input("Inserisci la stazione di destinazione: ").upper()
            time = input("Inserisci l'ora di partenza (HH:MM): ").replace(".", ":").replace(" ", "")
            # Import AI model
            model = joblib.load("machine_learning/train_delay_clf_v2.pkl")
            response = options.search(departure, arrival, time, model)

            if response == None:
                print("\nNessun treno disponibile per la tua ricerca. 😥\n")
            else:
                print(response, "\n")

        elif answer['choice'] == '📍 Cerca itinerario':

            departure = input("Inserisci la stazione di partenza: ").upper()
            arrival = input("Inserisci la stazione di destinazione: ").upper()
            # Import stations graph
            graph = nx.read_gexf("graphs/station_graph.gexf")
            # Import AI model
            model = joblib.load("machine_learning/train_delay_clf_v2.pkl")
            options.searchItinerary(graph, departure, arrival, model)

        elif answer['choice'] == '🚪 Uscire':
            print("Grazie per aver usato il nostro sistema, arrivederci! 👋")
            return


if __name__ == "__main__":
    print("Benvenuto nel sistema di ricerca treni!\n")
    warnings.filterwarnings("ignore", category=UserWarning)
    mainMenu()
