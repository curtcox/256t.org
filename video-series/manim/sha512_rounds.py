"""A small, slow, readable SHA-512 (FIPS 180-4) that exposes its internal rounds.

Episode 3 uses it to show how a single flipped input bit spreads through the
eight 64-bit working registers, round by round. `digest()` is checked against
hashlib so the visuals are driven by the real algorithm.
"""

from __future__ import annotations

MASK = (1 << 64) - 1


def _primes(count: int) -> list[int]:
    found: list[int] = []
    n = 2
    while len(found) < count:
        if all(n % p for p in found if p * p <= n):
            found.append(n)
        n += 1
    return found


def _iroot(n: int, k: int) -> int:
    """Integer k-th root (floor) of n."""
    x = 1 << ((n.bit_length() + k - 1) // k)
    while True:
        y = ((k - 1) * x + n // x ** (k - 1)) // k
        if y >= x:
            return x
        x = y


# Fractional parts of square roots of the first 8 primes and cube roots of the first 80.
H0 = [_iroot(p << 128, 2) & MASK for p in _primes(8)]
K = [_iroot(p << 192, 3) & MASK for p in _primes(80)]


def _rotr(x: int, n: int) -> int:
    return ((x >> n) | (x << (64 - n))) & MASK


def pad(message: bytes) -> bytes:
    length = len(message) * 8
    padded = message + b"\x80"
    padded += b"\x00" * ((112 - len(padded) % 128) % 128)
    return padded + length.to_bytes(16, "big")


def compress(state: list[int], block: bytes, trace: list[list[int]] | None = None) -> list[int]:
    w = [int.from_bytes(block[i:i + 8], "big") for i in range(0, 128, 8)]
    for t in range(16, 80):
        s0 = _rotr(w[t - 15], 1) ^ _rotr(w[t - 15], 8) ^ (w[t - 15] >> 7)
        s1 = _rotr(w[t - 2], 19) ^ _rotr(w[t - 2], 61) ^ (w[t - 2] >> 6)
        w.append((w[t - 16] + s0 + w[t - 7] + s1) & MASK)
    a, b, c, d, e, f, g, h = state
    for t in range(80):
        ch = (e & f) ^ (~e & g)
        maj = (a & b) ^ (a & c) ^ (b & c)
        t1 = (h + (_rotr(e, 14) ^ _rotr(e, 18) ^ _rotr(e, 41)) + ch + K[t] + w[t]) & MASK
        t2 = ((_rotr(a, 28) ^ _rotr(a, 34) ^ _rotr(a, 39)) + maj) & MASK
        a, b, c, d, e, f, g, h = (t1 + t2) & MASK, a, b, c, (d + t1) & MASK, e, f, g
        if trace is not None:
            trace.append([a, b, c, d, e, f, g, h])
    return [(x + y) & MASK for x, y in zip(state, [a, b, c, d, e, f, g, h])]


def digest(message: bytes) -> bytes:
    state = list(H0)
    data = pad(message)
    for i in range(0, len(data), 128):
        state = compress(state, data[i:i + 128])
    return b"".join(x.to_bytes(8, "big") for x in state)


def round_states(message: bytes) -> list[str]:
    """512-character bit strings of the working registers after each of the 80
    rounds of the first block (the message must fit in one block)."""
    data = pad(message)
    assert len(data) == 128, "use a message shorter than 112 bytes"
    trace: list[list[int]] = []
    compress(list(H0), data, trace)
    return ["".join(f"{x:064b}" for x in regs) for regs in trace]


def changed_bits(a: str, b: str) -> str:
    return "".join("1" if x != y else "0" for x, y in zip(a, b))


if __name__ == "__main__":
    import hashlib
    for sample in (b"", b"Hello", b"x" * 200, bytes(range(256)) * 9):
        assert digest(sample) == hashlib.sha512(sample).digest(), sample[:10]
    first = round_states(b"Hello")
    second = round_states(b"hello")
    for r in (1, 2, 4, 8, 16, 24, 80):
        print(f"after round {r:2d}: {changed_bits(first[r - 1], second[r - 1]).count('1'):3d} of 512 bits differ")
    print("sha512_rounds matches hashlib")
