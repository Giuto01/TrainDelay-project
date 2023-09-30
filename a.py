
def time_to_minutes(time):
    hour, minutes = time.split(":")
    return int(hour)*60 + int(minutes)
