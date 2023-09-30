import inquirer
import options
import networkx as nx

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

            response = options.search(departure, arrival, time)

            if response == None:
                print("\nNessun treno disponibile per la tua ricerca. 😥\n")
            else:
                print(response, "\n")

        elif answer['choice'] == '📍 Cerca itinerario':

            departure = input("Inserisci la stazione di partenza: ").upper()
            arrival = input("Inserisci la stazione di destinazione: ").upper()
            graph = nx.read_gexf("graphs/station_graph.gexf")

            options.searchItinerary(graph, departure, arrival)

        elif answer['choice'] == '🚪 Uscire':
            print("Grazie per aver usato il nostro sistema, arrivederci! 👋")
            return


if __name__ == "__main__":
    print("Benvenuto nel sistema di ricerca treni!\n")
    mainMenu()
