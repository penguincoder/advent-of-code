from time import time


def reverse(text, repeat):
    knot = list(range(256))
    pos = 0
    skip = 0
    for isntevenused in range(repeat):
         for i in text:
            temp = []
            for j in range(i):
                temp.append(knot[(pos+j) % 256])
            for j in range(i):
                knot[(pos+i-1-j) % 256] = temp[j]
            pos += skip + i
            skip += 1
    return knot


def dense(knot):
    dense = [0]*16
    for i in range(16):
        dense[i] = knot[16*i]
        for m in range(1, 16):
            dense[i] ^= knot[16*i+m]
    return dense


def kh(dense):
    knothash = ''
    for i in dense:
        if len(hex(i)[2:]) == 2:
            knothash += hex(i)[2:]
        else:
            knothash += '0' + hex(i)[2:]
    return knothash


start = time()

inp = '102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216'
text = [102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216]
text2 = []

for i in range(len(inp)):
    text2.append(ord(inp[i]))
text2 += [17, 31, 73, 47, 23]

knot = reverse(text, 1)
sparce = reverse(text2, 64)

dense = dense(sparce)
knothash = kh(dense)

print('Part One: ' + str(knot[0]*knot[1]))
print('Part Two: ' + knothash)
print('Completed in ' + str(time() - start) + ' seconds.')

