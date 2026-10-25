import statistics

def summary(*values, places=1):
    """Describe numbers by their mean and standard deviation."""
    mean = round(statistics.mean(values), places)
    spread = round(statistics.stdev(values), places)
    return f"{mean} ± {spread}"
