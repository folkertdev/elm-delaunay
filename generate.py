import random


randoms = { (random.randint(0, 500) , random.randint(0,500)) for i in range(500) }

for (a,b) in randoms:
    print(", (", a, ", ", b, ")" )
