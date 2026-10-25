from collections import Counter

def top_words(text, n=3):
    """The n most common words in text, most common first."""
    words = (word.strip(".,;:!?").lower() for word in text.split())
    return Counter(word for word in words if word).most_common(n)

top_words("The fox saw the dog. The dog ran; the fox ran too.", n=3)
