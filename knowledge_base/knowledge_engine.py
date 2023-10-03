from pyswip import Prolog


class PrologEngine:
    '''
    A utility class for querying a Prolog knowledge base containing train-related information.

    This class provides methods to perform queries on a Prolog knowledge base that contains
    data related to trains, stations, and their schedules. It allows you to retrieve information
    about trains departing from stations, trains departing at specific times, trains departing
    between stations at a certain time, and more.

    Parameters:
    *paths: Variable-length argument list of paths to Prolog knowledge base files.

    Attributes:
    prolog: The Prolog engine instance used for querying the knowledge base.

    Methods:
    trains_departure_from_station_name(stationName)
    trains_departure_from_station_name_at_time(stationName, departure)
    trains_departure_between_stations_name_at_time(departureStationName, arrivalStationName, time)
    get_train_fact_by_ID(trainID)
    station_name_by_ID(stationID)
    station_ID_by_name(stationName)
    '''

    prolog = None

    def __init__(self, *paths) -> None:
        '''
        Initialize the PrologEngine instance.

        Parameters:
        *paths: Variable-length argument list of paths to Prolog knowledge base files.
        '''

        self.prolog = Prolog()
        for path in paths:
            self.prolog.consult(path)

    def trains_departure_from_station_name(self, stationName):
        '''
        Get a list of trains departing from a specified station.

        Parameters:
        stationName (str): The name of the departure station.

        Returns:
        list: A list of train information departing from the specified station.
        '''

        return list(self.prolog.query(f"trains_departure_from_station_name(\"{stationName}\", Trains)."))[0]["Trains"]

    def trains_departure_from_station_name_at_time(self, stationName, departure):
        '''
        Get a list of trains departing from a specified station at a specific time.

        Parameters:
        stationName (str): The name of the departure station.
        departure (str): The departure time.

        Returns:
        list: A list of train information departing from the specified station at the specified time.
        '''

        return list(self.prolog.query(f"trains_departure_from_station_name_at_time(\"{stationName}\", \"{departure}\", Trains)."))[0]["Trains"]

    def trains_departure_between_stations_name_at_time(self, departureStationName, arrivalStationName, time):
        '''
        Get a list of trains departing between two specified stations at a specific time.

        Parameters:
        departureStationName (str): The name of the departure station.
        arrivalStationName (str): The name of the arrival station.
        time (str): The departure time.

        Returns:
        list: A list of train information departing between the specified stations at the specified time.
        '''

        return list(self.prolog.query(f"trains_departure_between_stations_name_at_time(\"{departureStationName}\", \"{arrivalStationName}\", \"{time}\", Trains)."))[0]["Trains"]
    
    def trains_departure_between_stations_name(self, departureStationName, arrivalStationName):
        '''
        Get a list of trains departing between two specified stations at a specific time.

        Parameters:
        departureStationName (str): The name of the departure station.
        arrivalStationName (str): The name of the arrival station.
        time (str): The departure time.

        Returns:
        list: A list of train information departing between the specified stations at the specified time.
        '''

        return list(self.prolog.query(f"trains_departure_between_stations_name(\"{departureStationName}\", \"{arrivalStationName}\", Trains)."))[0]["Trains"]

    def get_train_fact_by_ID(self, trainID):
        '''
        Get the train fact information by its ID.

        Parameters:
        trainID (str): The ID of the train.

        Returns:
        dict: A dictionary containing information about the specified train.
        '''

        return list(self.prolog.query(f"get_train_fact({trainID}, Type, DepartureID, ArrivalID, DepartureTime, ArrivalTime)."))[0]

    def station_name_by_ID(self, stationID):
        '''
        Get the station name by its ID.

        Parameters:
        stationID (str): The ID of the station.

        Returns:
        str: The name of the station with the specified ID.
        '''

        return list(self.prolog.query(f"station({stationID}, StationName, _)."))[0]["StationName"]
    
    def station_ID_by_name(self, stationName):
        '''
        Get the station ID by its Name.

        Parameters:
        Name (str): The name of the station.

        Returns:
        str: The ID of the station with the specified Name.
        '''

        return list(self.prolog.query(f"station(StationID, \"{stationName}\", _)."))[0]["StationID"]
