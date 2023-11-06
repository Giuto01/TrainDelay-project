# TrainDelay-project
Progetto finale per il corso ICON2223 [Uniba](https://www.uniba.it/it/ricerca/dipartimenti/informatica)

## Obiettivo 🎯

Creazione di un sistema di ricerca treni per la pianificazione automatica di itinerari, inoltre incorpora un modello di intelligenza artificiale per offrire predizioni del probabile ritardo di un determinato treno.

**Documentazione completa**: [Report](docs/Report.pdf)

## Requisiti 📜

- [**Prolog**](https://en.wikipedia.org/wiki/Prolog), per la gestione e l'interrogazione nella knowledge base dei treni;
- [**Python**](https://www.python.org/), per eseguire l'applicazione.

## Installazione 🔩
Avendo python installato sulla propria macchina:

  1. Installazione Prolog:
     * Windows: [swi-prolog](https://www.swi-prolog.org/download/stable?show=all)
     * Linux: 
         ```bash
         sudo add-apt-repository ppa:swi-prolog/stable
         sudo apt install swi-prolog
         ```

  2. Clona il repository:

      ```bash
      git clone https://github.com/Giut0/TrainDelay-project.git
      cd TrainDelay-project/
      ```
  3. Creazione ambiente virtuale (opzionale):
      ```bash
      python -m venv .my_env

      # Attiva l'ambiente virtuale (su Windows)
      .my_env\Scripts\activate

      # Attiva l'ambiente virtuale (su macOS e Linux)
      source .my_env/bin/activate
      ```
  4. Installazione dipendenze: 
    
        ```bash
        pip install -r requirements.txt
        ```
  5. Esecuzione applicazione:
      ```bash
      python3 app.py
      ``` 
  
    
## Utilizzo 📍
Eseguendo `app.py` apparirà l'interfaccia utente a riga di comando:

```bash
[?] Scegli una opzione:
  > 🚄 Cerca treno tra due stazioni
    📍 Cerca itinerario
    🚪 Uscire

```

### Possibili opzioni ⚙️
1. `Cerca treno tra due stazioni`: Inserendo il nome di due stazioni e l'orario di partenza il sistema troverà tutti i treni disponibili tra le stazioni scelte e dopo l'orario selezionato;
2. `Cerca itinerario`: Inserendo la stazione di partenza e la stazione di arrivo il sistema troverà l'itinerario migliore in base al minor numero di stazioni del percorso;
3. `Uscire`: Termine del programma.

### Origine dei dati 🗃️
I dati necessari per la realizzazione di questo sistema, in particolare per la costruzione della _knowledge base_, sono stati recuperati attraverso l'interrogazione alle API fornite da **[viaggiatreno.it](http://www.viaggiatreno.it/infomobilita/index.jsp)** per la schedule dei treni e **[sabas](https://github.com/sabas/trenitalia)** per i dati relativi alle stazioni.


### Organizzazione repository 📐
```
TrainDelay-project/
|
├── data_preparation/
│   └── data_pre-processing.ipynb
|
├── data_retrive/
│   └── scrape_train_data.py
│
├── dataset/
│   ├── stations.csv
│   ├── trains.csv
│   ├── trains_refined_classification.csv
│   ├── trains_refined_regression.csv
│   └── new_test_dataset.csv
│
├── docs/
│   ├── Report.tex
│   ├── Report.pdf
│   └── ...
│
├── graphs/
│   ├── generate_station_graph.py
│   └── station_graph.gexf
│
├── knowledge_base/
│   ├── trenitalia_schedule.pl
│   ├── stations.pl
│   ├── rules.pl
│   ├── generate_station_kb.py
│   ├── generate_train_kb.py
│   └── knowledge_engine.py
|
├── machine_learning/
│   ├── classification.ipynb
│   ├── regression.ipynb
│   └── train_delay_clf_v2.pkl
│
├── app.py
├── options.py
├── utils.py
├── README.md
├── LICENSE
└── requirements.txt
```
