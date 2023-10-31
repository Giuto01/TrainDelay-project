def time_to_minutes(time):
    '''
    Converts a time string in the format "HH:MM" to the total number of minutes since midnight.

    Args:
        time (str): A time string in the "HH:MM" format.

    Returns:
        int: The total minutes since midnight.

    Example:
        >>> time_to_minutes("08:30")
        510
    '''

    hour, minutes = time.split(":")
    return int(hour) * 60 + int(minutes)

def bin_train_type(trainType):
    '''
    Maps a train type string to a binary value.

    Args:
        trainType (str): The train type, which can be "regionale" or "nazionale".

    Returns:
        int or None: 1 if trainType is "regionale," 0 if trainType is "nazionale," or None if not recognized.

    Example:
        >>> bin_train_type("regionale")
        1
    '''

    if trainType == "regionale":
        return 1
    elif trainType == "nazionale":
        return 0

def prediction_str(pred):
    '''
    Converts a binary prediction result to a human-readable string.

    Args:
        pred (int): Binary prediction result (0 or 1).

    Returns:
        str: "Yes" if pred is 1, "No" if pred is 0.

    Example:
        >>> prediction_str(1)
        "Yes"
    '''

    if pred[0] == 0:
        return "No"
    else:
        return "Yes"